module type CRDT_element = sig
  type t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module CRDT(E: CRDT_element) = struct

  type elt = E.t

  type author_id = int32

  let prng = Random.State.make_self_init ()

  module Marker : sig
    type t
    val beginning : t
    val ending : t
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
    val of_int64 : int64 -> t
    val generate : unit -> t
  end = struct
    type t = int64
    let beginning : t = -1_L     (* Int64.min_int *)
    let ending    : t = 666_L (* Int64.max_int *)
    let compare = Int64.compare
    let pp = Fmt.int64
    let equal = Int64.equal
    let of_int64 i = i
    let generate () : t =
      let rec skip num =
        if equal num beginning || equal num ending
        then skip (Random.State.int64 prng ending) else num
      in skip beginning
  end

  type element =
    | Live of elt
    | Tombstone of elt

  let pp_element fmt element =
    (match element with Live e -> "+",e | Tombstone e -> "-", e)
    |> fun  (s,s2) ->
    Fmt.pf fmt "(%s %a)" s E.pp s2

  module Markers = struct
    include Map.Make(Marker)
    let pp fmt t =
      iter (fun marker -> fun element ->
          Fmt.pf fmt "(%a:%a)" Marker.pp marker pp_element element
        ) t
  end

  type edge =
    { left: Marker.t ;
      right: Marker.t ;
      author: author_id ;
    }

  let pp_edge fmt v =
    Fmt.pf fmt "{%a < %a [%ld]}" Marker.pp v.left Marker.pp v.right v.author

  module Edges : sig
    type t
    val empty : t
    val insert : edge -> t -> t
    val union : t -> t -> t
    val pp : Format.formatter -> t -> unit
    val fold : ('a -> edge -> 'a) -> 'a -> t -> 'a
  end
  = struct
    include List
    type t = edge list
    let empty = []
    let union a b = a @ b
    let insert a t = a::t
    let pp fmt (t:t) =
      Fmt.pf fmt "@[<v>%a@]" Fmt.(parens @@ list ~sep:(unit "; ") pp_edge) t
    let fold = List.fold_left
  end

  type edit =
    { undo_group_id : unit ; (* TODO *)
      deleted : element Markers.t ;
      inserted : element Markers.t ;
    }

  let pp_edit fmt { deleted ; inserted; _ }=
    Fmt.pf fmt "@[<v>{ @[<v>undo: (TODO);@ deleted: %a@ inserted: %a@]}@]"
      Markers.pp deleted Markers.pp inserted

  let empty_edit =
    { undo_group_id = () ;
      deleted = Markers.empty ;
      inserted = Markers.empty ;
    }

  module Edits = struct include Set.Make(struct
        type t = edit
        let compare = compare
      end)
    let pp fmt (t:t) =
      Fmt.pf fmt "@[<v>" ;
      iter (Fmt.pf fmt "%a ; " pp_edit) t ;
      Fmt.pf fmt "@]"
  end

  type t = (* CRDT state *)
    { elements : element Markers.t ;
      edges: Edges.t ;
      edits    : Edits.t ;
    }

  let pp fmt {elements; edges; edits} =
    Fmt.pf fmt "@[<v>{ @[<v>elements: %a;@ edges: %a@ edits: %a@]@ }@]"
      Markers.pp elements
      Edges.pp edges
      Edits.pp edits

  let empty =
    { elements = Markers.empty ;
      edges = Edges.empty ;
      edits = Edits.empty ;
    }

  let singleton author marker element=
    { empty with elements = Markers.singleton marker (Live element)  ;
                 edges = Edges.(
                     insert {left=Marker.beginning; right=marker; author} empty
                     |> insert {left=marker; right=Marker.ending; author});
    }

  let merge t1 t2 =
    { elements = Markers.union (fun _ a b -> if a = b
                                 then Some a
                                 else failwith "TODO marker id conflict")
          t1.elements t2.elements ;
      edges = Edges.union t1.edges t2.edges ;
      edits = Edits.union t1.edits t2.edits ;
    }

  let append t ~author ~(after:Marker.t) marker element =
    merge t
    { empty with elements = Markers.singleton marker (Live element) ;
                 edges = Edges.(
                     insert    {left=after ; right=marker       ; author} empty
                     |> insert {left=marker; right=Marker.ending; author}
                   ) }

  module Snapshot = struct
    type snapshot = (Marker.t * element) Pvec.t

    let equal (a:snapshot) (b:snapshot) : bool =
      Pvec.equal a b ~eq:(fun a b ->
          Marker.equal (fst a) (fst b)
          && ( match snd a, snd b with
              | Live a, Live b -> E.equal a b
              | Tombstone a, Tombstone b -> E.equal a b
              | _ -> false ) )

    let pp : Format.formatter -> snapshot -> unit =
      Pvec.pp ~sep:Fmt.(unit " -> ")
        Fmt.(pair ~sep:(unit":") Marker.pp pp_element)

    let to_edges author (snapshot:snapshot) : Edges.t =
      Pvec.foldi_left
        (fun acc idx (marker, _) ->
           match idx with
           | 0 when marker = Marker.beginning -> acc
           | 0 -> Edges.insert {left = Marker.beginning;
                                right = marker ; author } acc
           | _ -> Edges.insert { left = fst @@ Pvec.get snapshot (pred idx) ;
                                 right = marker ; author } acc
        ) Edges.empty
        snapshot

    let to_vector (snapshot:snapshot) : E.t Pvec.t =
      Pvec.filter_map (function _, Live x -> Some x
                              | _, Tombstone _ -> None) snapshot

    let of_t {edges = original_edges ; elements ; _ } : snapshot =
      let sorted_dag =
        (* TODO Algorithm:
         * solve LEFTHAND edges :
         * - let edges_begin , edges_tl = split edges into (map: LEFTHAND -> +),
         *                                                   (map + -> +) in
         * - let ties = list of LEFTHAND < x and x < RIGHT with equal RIGHTs in
         * - let vector = ties ordered by author_id in
         * - let unresolved_rest = edges_begin minus resolved_ties in
         * - let unresolved_righthands = map snd unresolved_rest in
         * - fold over unresolved_righthands (unresolved_righthands, edges_tl):
         * - - A. (righthand,edges_tl) ->
         * - - - let (vector, edges_tl) = solve righthand edges_tl
         * - return (vector , edges_tl)
         * let vector, edges_tl = solve Beginning t.edges in
         * make sure the last element of vector is Ending
         * make sure edges_tl is empty
         * *)
        let edge_dag =
          ( Edges.fold
              (fun acc edge ->
                 if List.mem_assoc edge.left acc then
                   List.map (fun (x,y) -> if x = edge.left
                              then x,(edge.author, edge.right)::y
                              else (x,y)
                            )acc
                 else
                   (edge.left, [(edge.author, edge.right)])::acc)
              [] original_edges @ [Marker.ending,[]])
        in
        Fmt.pr "edge_dag: @[<v>%a@]@."
          Fmt.(list ~sep:(unit"@,")
                 (pair ~sep:(unit":") Marker.pp
                    (brackets @@ list ~sep:(unit" ; ")
                     @@ pair (parens int32) Marker.pp))
              ) edge_dag;

        let sort_siblings lst =
          (* sort ascending based on author/author id *)
          List.sort_uniq (fun a b -> Int32.compare (fst a) (fst b)) lst in
        let take_key key =  (* TODO exception *)
          List.assoc key edge_dag |> sort_siblings,
          List.remove_assoc key
        in
        let rec handle_child (ignored,acc) marker : 'b list * 'a list=
          let children, tl = take_key (snd marker) in
          let children =
            List.filter (fun a -> not @@ List.mem a ignored) children in
          if children <> [] then
            Fmt.pr "My[%a] children: @[%a@]@." Marker.pp (snd marker)
              Fmt.(list ~sep:(unit";")
                   @@ pair (parens int32) Marker.pp) children;
          let ignored = children @ ignored in
          (List.fold_left (fun acc marker -> handle_child acc marker)
             (ignored, marker::[]) children) |> fun (ignored, acc2) ->
          ignored, acc @ acc2
        in
        handle_child ([],[]) (-1l,Marker.beginning) |> snd
      in
      Fmt.pr "sorted_dag: @[%a@]@."
        Fmt.(list ~sep:(unit";") @@ pair (parens int32) Marker.pp) sorted_dag;
      let markers = sorted_dag |> List.map snd
                    |> List.filter (fun a -> a <> Marker.beginning
                                             && a <> Marker.ending) in
      let m_elements = List.filter (fun x ->
          not @@ Marker.(equal x beginning) || Marker.(equal x ending)) markers
                       |> List.map (fun m -> Markers.find m elements)
      in
      Fmt.pr "markers: %a@." Fmt.(list ~sep:(unit";")pp_element) m_elements;
      markers
      |> List.fold_left (fun vec marker ->
          if not Marker.(equal marker beginning || equal marker ending) then
            Pvec.add_last vec (marker,(Markers.find marker elements)) else vec)
        Pvec.empty
  end

  let extend_with_snapshot author original_t (vector:E.t Pvec.t) =
    let current_snapshot = Snapshot.of_t original_t in
    Pvec.foldi_left (fun acc idx b ->
        let b_element = (match snd b with Live x -> x | Tombstone x -> x) in
        let vec_element = Pvec.(get vector idx) in
        if E.equal b_element vec_element
        then Pvec.add_last acc b (* the subsets are equivalent *)
        else begin
          (* TODO check if vector contains snd b further down the road*)
          begin match Pvec.left_find ~start:idx (E.equal b_element) vector with
            | Some (next_idx, _) ->
              (* - if it does: insert everything between here and there,
                   increment offset *)
              acc
            | None ->
              (*- if it doesn't: mark b as dead and insert it: *)
              Pvec.add_last acc (fst b, Tombstone b_element)
              (* then append vec_element: *)
              |> fun acc -> Pvec.add_last acc (author, Live vec_element)
          end
        end)
      Pvec.empty current_snapshot

end

module UcharCRDT = struct
  module Uchar = struct include Uchar
    let pp fmt t= Fmt.pf fmt "%0x" (to_int t)
  end
  include CRDT(Uchar)
end

module CharCRDT = struct
  module Char = struct include Char
    let pp fmt = Fmt.pf fmt "%c"
  end
  include CRDT(Char)
end
