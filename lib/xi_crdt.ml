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

  let element_equal a b = match a , b with
    | Live      a, Live b -> E.equal a b
    | Tombstone a, Tombstone b -> E.equal a b
    | _ -> false

  let element_equal_ignoring_status a = function
      Live b | Tombstone b -> E.equal b (match a with Live a -> a
                                                    | Tombstone a -> a)

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
    val filter : (edge -> bool) -> t -> t
    val exists : (edge -> bool) -> t -> bool
    val equal : t -> t -> bool
  end
  = struct
    include List
    type t = edge list
    let empty = []
    let compare a b = (* this is NOT topological comparison!*)
      let left_compare = Marker.compare a.left b.left in
      if 0 <> left_compare then left_compare
      else begin
        let author_compare = Int32.compare a.author b.author in
        if 0 <> author_compare then author_compare
        else begin
          Marker.compare a.right b.right
        end
      end
    let union (a:t) (b:t) = a @ b |> List.sort_uniq compare
    let insert a t = a::t
    let pp fmt (t:t) =
      Fmt.pf fmt "@[<v>%a@]" Fmt.(parens @@ list ~sep:(unit "; ") pp_edge)
        List.(sort (fun {left=a;_} {left=b;_} -> Marker.compare a b)t)
    let fold = List.fold_left
    let exists : (edge -> bool) -> t -> bool = List.exists
    let equal a b = a = b
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
      edges    : Edges.t ;
      edits    : Edits.t ;
    }

  let equal t1 t2 =
    Markers.equal element_equal t1.elements t2.elements
    && Edges.equal t1.edges t2.edges
    && Edits.equal t1.edits t2.edits

  let pp fmt {elements; edges; edits} =
    Fmt.pf fmt "@[<v>{ @[<v>elements: %a;@ edges: %a@ edits: %a@]@ }@]"
      Markers.pp elements
      Edges.pp edges
      Edits.pp edits

  let empty =
    { elements = Markers.empty ;
      edges = Edges.insert { left = Marker.beginning; right = Marker.ending;
                             author = -1l } Edges.empty;
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

  module Snapshot = struct
    type snapshot = (Marker.t * element) Pvec.t

    let elements (snapshot:snapshot) : element Markers.t =
      Pvec.fold_left (fun acc (marker, element) ->
          Markers.add marker element acc) Markers.empty snapshot

    let equal (a:snapshot) (b:snapshot) : bool =
      Pvec.equal a b ~eq:(fun a b ->
          Marker.equal (fst a) (fst b)
          && element_equal_ignoring_status (snd a) (snd b))

    let pp : Format.formatter -> snapshot -> unit =
      Pvec.pp ~sep:Fmt.(unit " -> ")
        Fmt.(pair ~sep:(unit":") Marker.pp pp_element)

    let to_edges author (snapshot:snapshot) : Edges.t =
      Pvec.foldi_left
        (fun acc (idx:int) (marker, _) ->
           match idx with
           | 0 when Marker.equal marker Marker.beginning -> acc
           | 0 -> Edges.insert { left = Marker.beginning;
                                 right = marker; author} acc
           | _ -> Edges.insert { left = fst @@ Pvec.get snapshot (pred idx) ;
                                 right = marker; author} acc
        ) Edges.(empty |> insert { left = fst @@ Pvec.get_last snapshot ;
                                   right = Marker.ending ; author })
        snapshot |> fun produced_edges ->
      Logs.debug (fun m -> m "to_edges: %a produced: %a"
                     pp snapshot Edges.pp produced_edges
                 ) ;
      produced_edges

    let to_vector (snapshot:snapshot) : E.t Pvec.t =
      Pvec.filter_map (function _, Live x -> Some x
                              | _, Tombstone _ -> None) snapshot

    let of_t {edges = original_edges ; elements ; _ } : snapshot =
      let longest_dag =
        let incoming_map =
          (* TODO this implementation needs to find the LONGEST path through the DAG*)
          (* https://en.wikipedia.org/wiki/Longest_path_problem *)
          ( Edges.fold
              (fun acc edge ->
                 if List.mem_assoc edge.right acc then
                   List.map (fun (x,y) -> if Marker.equal x edge.right
                              then x,(edge.author, edge.left)::y
                              else (x,y)
                            )acc
                 else
                   (edge.right, [(edge.author, edge.left)])::acc)
              [] original_edges) @ [Marker.beginning, []]
        in
        let sep = Fmt.unit "," in
        Logs.debug (fun m -> m "incoming graph: @[<v>%a@]"
                       Fmt.(list ~sep:(unit "|@,") @@ pair ~sep Marker.pp
                              (list ~sep:(unit"; ")
                               @@ pair ~sep
                                 (brackets int32) Marker.pp )) incoming_map) ;
        let counted = List.map (fun (x,y) ->
            x, List.length
              (List.filter (fun (_,m) -> not @@ Marker.equal Marker.ending m) y)
          ) incoming_map in

        (* Ensure we have nothing before BEGINNING: *)
        let () = assert(List.assoc Marker.beginning counted = 0) in
        (* Ensure we have at least one thing pointing to ENDING: *)
        let () = assert(List.assoc Marker.ending counted > 0) in

        Logs.debug (fun m ->
            m "counted vertices: @[<v>%a@]"
              Fmt.(list ~sep:(unit "@,")@@ pair ~sep Marker.pp int) counted) ;

        let rec recurse (siblings:(author_id * Marker.t) list)
          : author_id * Marker.t list * int =
          List.fold_left (fun (last_auth,acc, acc_count) (author, marker) ->
              let this_count = try List.assoc marker counted + 1 with
                | Not_found ->
                  Logs.err (fun m ->
                      m "Not found in count: %a" Marker.pp marker) ;
                  failwith"TODO fix assoc"
              in
              let points_to_me = List.assoc marker incoming_map in
              let (_auth, child_path, child_counts) =
                recurse @@ points_to_me in
              Logs.debug (fun m ->
                  m "marker %a children: %d path: %a" Marker.pp marker
                    child_counts
                    Fmt.(list ~sep Marker.pp) child_path );
              let this_total = this_count + child_counts in
              begin if this_total > acc_count
                    || (this_total = acc_count && author < last_auth)
                then begin
                  let actual_siblings =
                    List.filter (fun (_author,sibmark) ->
                        not (List.exists (Marker.equal sibmark) child_path)
                      ) siblings
                    |> List.sort (*sort ascending according to author: *)
                      (fun (aaa,_) (bbb,_) -> Int32.compare aaa bbb)
                    (* this folds right, producing a list in ascending order*)
                    |> List.fold_left
                      (fun acc ((_auth,marker) as elem) ->
                         match acc with
                         | (_, hd)::_ when Marker.equal marker hd -> acc
                         | acc -> elem::acc ) []
                    |> List.sort (*sort decending according to author: *)
                      (fun (aaa,_) (bbb,_) -> Int32.compare bbb aaa)
                    |> List.split |> snd
                  in
                  Logs.debug (fun m ->
                      m "child_path: %a -- Actual siblings for %a : [%a] \
                         - out of [%a]"
                        Fmt.(brackets @@ list ~sep Marker.pp) child_path
                        Marker.pp marker
                        Fmt.(list ~sep Marker.pp) actual_siblings
                        Fmt.(list ~sep @@ pair ~sep:(unit":")
                               int32 Marker.pp) siblings );
                  let acc_after_insertion =
                    let rec insert_after ~tgt ~lst acc = function
                      | [] -> List.rev acc
                      | hd::tl when Marker.equal hd tgt ->
                        Logs.debug (fun m ->
                            m "insert_after: tgt:%a lst:[%a] tl:[%a]"
                              Marker.pp tgt Fmt.(list ~sep Marker.pp) lst
                              Fmt.(list ~sep Marker.pp) tl
                          );
                        insert_after ~tgt ~lst
                          ((List.rev lst) @ (hd::acc)) tl
                      | hd::tl -> insert_after ~tgt ~lst (hd::acc) tl
                    in
                    insert_after
                      ~tgt:marker ~lst:child_path [] actual_siblings
                  in
                  Logs.debug (fun m -> m "sorted after insert: %a"
                                 Fmt.(list ~sep Marker.pp)
                                 acc_after_insertion) ;
                  (author, (acc_after_insertion), this_total)
                end else begin
                  if this_total = acc_count
                  then Logs.debug (fun m -> m"TODO i'm equal %a"
                                      Marker.pp marker);
                  Logs.debug (fun m -> m "rejecting (%ld)%a score:%d \
                                          < (%ld)%a score:%d"
                                 author Marker.pp marker this_total
                                 last_auth
                                 Fmt.(list ~sep Marker.pp) acc acc_count );
                  (last_auth,acc,acc_count)
                end end
            ) (Int32.max_int, [], ~-1)
            (List.sort (fun (a,_) (b,_) -> Int32.compare b a)siblings)
        in
        recurse (List.assoc Marker.ending incoming_map)
        |> fun (_,dag,_) -> List.rev dag
      in
      let sorted_dag = longest_dag in
      Logs.debug (fun m -> m "Longest dag: %a"
                     Fmt.(list ~sep:(unit";") @@ Marker.pp)
                     sorted_dag
                 );

      let markers = sorted_dag
                    |> List.filter (fun a -> a <> Marker.beginning
                                             && a <> Marker.ending) in
      let m_elements =
        List.filter (fun x ->
            not @@ Marker.(equal x beginning)
            || Marker.(equal x ending)) markers
        |> List.map (fun m -> Markers.find m elements)
      in
      Logs.debug (fun m -> m "markers: %a"
                     Fmt.(list ~sep:(unit";")pp_element) m_elements);
      let produced =
        List.fold_left (fun vec marker ->
            if not Marker.(equal marker beginning || equal marker ending) then
              Pvec.add_last vec (marker,(Markers.find marker elements))
            else vec)
          Pvec.empty markers in
      Logs.debug (fun m -> m "of_t: %a" pp produced);
      produced

    let extend_with_vector
        (current_snapshot:snapshot) (original_vector:E.t Pvec.t) : snapshot =
      let make_live element =
        let mark = Marker.generate() in
        Logs.debug (fun m ->
            m "Creating marker (%a, Live %a)" Marker.pp mark E.pp element);
        mark , Live element in
      let with_markers = Pvec.map make_live in
      Pvec.foldi_left (fun (acc,old_vector) _idx b_with_marker ->
          begin match Pvec.pop_first old_vector with
            | None -> (*done dealing with the source*)
              Logs.debug (fun m -> m "Adding remaining %d old_vec"
                           @@ Pvec.length old_vector);
              Pvec.add_last acc b_with_marker, old_vector
            | Some (vec_element, vector_tl) ->
              (* TODO make sure both are alive before all this *)
              let b_element = (match snd b_with_marker with Live x -> x
                                                          | Tombstone x -> x) in
              if E.equal b_element vec_element
              then begin (* the subsets are equivalent: *)
                Logs.debug (fun m ->
                    m "extend_with_vector: adding equal %a element"
                      E.pp b_element);
                Pvec.add_last acc b_with_marker, vector_tl
              end else begin
                (* check if vector contains b's element
                   further down the road: *)
                begin match
                    Pvec.left_find (E.equal b_element) vector_tl with
                | Some (next_idx, _) ->
                  (* - if it does: insert everything between here and there,
                       with new markers, increment offset *)
                  let left, right = Pvec.break_left next_idx vector_tl in
                  let right = Pvec.drop_left 1 right in
                  let acc = Pvec.add_last acc (make_live vec_element) in
                  Logs.debug (fun m ->
                      m "extend_with_vector: adding left (%a) ; %a \
                         ;right(%a) subsets from %d"
                        (Pvec.pp E.pp) left E.pp b_element
                        (Pvec.pp E.pp) right next_idx);
                  let acc = Pvec.add_last acc b_with_marker in
                  Pvec.append acc (with_markers left), right
                | None ->
                  (*- if it doesn't: mark b as dead and insert it: *)
                  Logs.debug (fun m -> m "extend_with_vector: adding dead %a \
                                          element and live %a"
                                 E.pp b_element E.pp vec_element);
                  ( Pvec.add_last acc (fst b_with_marker, Tombstone b_element)
                    (* then append vec_element: *)
                    |> fun acc -> Pvec.add_last acc (make_live vec_element)),
                  vector_tl
                end
              end
          end
        )
        (Pvec.empty, original_vector) current_snapshot
      |> fun (produced, tl) ->
      Logs.debug (fun m -> m "produced: %d tl: %d: %a" (Pvec.length produced)
                     (Pvec.length tl) pp produced) ;
      Pvec.append produced (with_markers tl)
  end

  let update_with_vector author (t:t) vector =
    let old_snapshot = Snapshot.of_t t in
    let next_snapshot = Snapshot.extend_with_vector old_snapshot vector in
    let next_elements =
      let new_elements = Snapshot.elements next_snapshot in
      (* produce a union just in case TODO this shouldn't be needed unless [t]
         contains elements not in the snapshot ... which it shouldn't.*)
      Markers.union (fun marker a b ->
          match a, b with
          | Tombstone _a, Live _b ->
            Logs.err (fun m -> m "Illegal Tombstone->Live resurrection of %a"
                         Marker.pp marker ) ;
            failwith "TODO illegal state transition: Tombstone->Live"
          | Tombstone elt_a, Tombstone elt_b
          | Live      elt_a, Tombstone elt_b
          | Live      elt_a, Live elt_b ->
            if E.equal elt_a elt_b
            then Some b (* pick [b] to support tombstoning*)
            else begin
              Logs.err (fun m ->
                  m "TODO marker %a is not unique!" Marker.pp marker) ;
              failwith ("TODO markers must be unique") end
        )
        t.elements new_elements
    in
    Logs.debug (fun m -> m "next_snapshot: %a" Snapshot.pp next_snapshot);
    let edges_of_next_snapshot = Snapshot.to_edges author next_snapshot in
    Logs.debug (fun m -> m "  ^-- edges: %a" Edges.pp edges_of_next_snapshot);
    let next_edges =
      edges_of_next_snapshot
      (* eliminate existing edges, ignoring author,
         to avoid taking ownership: *)
      (*|> Edges.filter
        (fun next ->
        not @@ Edges.exists
        (fun {left;right; author = old_author;} ->
        left = next.left && right = next.right
        (* update if the old author is < than us to maintain priority*)
        && old_author < author) t.edges)*)
      (* union to re-establish authorship of old edges: *)
      |> Edges.union t.edges
    in
    let produced =
      { elements = next_elements ; edges = next_edges ; edits = Edits.empty } in
    Logs.debug (fun m -> m "Produced: %a" pp produced); produced

  let insert_element t ~author ~(after:Marker.t) marker element =
    merge t
      { empty with
        elements = Markers.singleton marker (Live element) ;
        edges = Edges.(
            insert    {left=after ; right=marker       ; author} empty
            |> insert {left=marker; right=Marker.ending; author} ) }

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
