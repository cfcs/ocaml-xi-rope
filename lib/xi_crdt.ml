module type CRDT_element = sig
  type t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module CRDT(E: CRDT_element) = struct

  type elt = E.t

  type author_id = int32 (* a.k.a. vertice "weight" *)

  let prng = Random.State.make_self_init ()

  module Marker : sig
    type t
    val beginning : t
    val ending : t
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val generate : unit -> t
  end = struct
    type t = int64
    let beginning : t = 0L     (* Int64.min_int *)
    let ending    : t = 0x10000000L (*Int64.max_int TODO*)
    let compare = Int64.compare
    let pp fmt num = Fmt.pf fmt "0x%02Lx" num
    let equal = Int64.equal
    let of_int64 i = i
    let to_int64 i = i
    let generate () : t =
      let rec skip num =
        if equal num beginning || equal num ending
        then skip (Random.State.int64 prng ending) else num
      in skip beginning
  end

  module MarkerSet = Set.Make(Marker)

  let pp_elt = E.pp

  type element =
    | Live of elt
    | Tombstone of elt

  let pp_element fmt element =
    (match element with Live e -> `Green,"+",e | Tombstone e -> `Red, "-", e)
    |> fun  (color,s,s2) ->
    Fmt.pf fmt "(%a)" Fmt.(styled `Bold @@ styled color
                              @@ pair ~sep:(unit" ") string E.pp) (s,s2)

  let element_equal a b = match a , b with
    | Live      a, Live b -> E.equal a b
    | Tombstone a, Tombstone b -> E.equal a b
    | _ -> false

  let element_equal_ignoring_status a = function
      Live b | Tombstone b -> E.equal b (match a with Live a -> a
                                                    | Tombstone a -> a)

  module Markers = struct
    (* Map of markers -> element *)
    include Map.Make(Marker)
    let diff (type x) (t1:x t) (t2:x t) : x t =
      merge (fun _key -> function
          (* exists solely in t2: *)
          | None -> begin fun b -> b end
          | Some a -> begin function
              | None -> None
              | Some b -> if a = b then None else Some b
            end
        ) t1 t2
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

  module EdgeOrder = struct
    type t = edge

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
  end

  module Edges : sig
    type t
    val empty : t
    val insert : edge -> t -> t
    val union : t -> t -> t
    val pp : Format.formatter -> t -> unit
    val fold : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val filter : (edge -> bool) -> t -> t
    val exists : (edge -> bool) -> t -> bool
    val equal : t -> t -> bool
    val remove : edge -> t -> t
    val is_empty : t -> bool
    val diff : t -> t -> t
  end
  = struct
    include Set.Make(EdgeOrder)
    let insert = add
    let pp fmt (t:t) =
      Fmt.pf fmt "@[<v>%a@]" Fmt.(parens @@ list ~sep:(unit "; ") pp_edge)
        List.(sort (fun {left=a;_} {left=b;_} -> Marker.compare a b)
                (elements t) )
    let equal a b = a = b
  end

  type edit =
    { undo_group_id : unit ; (* TODO *)
      deleted : element Markers.t ;
      inserted : element Markers.t ;
      edges : Edges.t;
      author : author_id;
    }

  let pp_edit fmt { deleted ; inserted; _ }=
    Fmt.pf fmt "@[<v>{ @[<v>undo: (TODO);@ deleted: %a@ inserted: %a@]}@]"
      Markers.pp deleted Markers.pp inserted

  let empty_edit =
    { undo_group_id = () ;
      deleted = Markers.empty ;
      inserted = Markers.empty ;
      edges = Edges.empty ;
      author = 0l; (* TODO *)
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
      authors  : author_id Markers.t ;
    }

  let equal t1 t2 =
    Markers.equal element_equal t1.elements t2.elements
    && Edges.equal t1.edges t2.edges
    && Edits.equal t1.edits t2.edits
    && Markers.equal Int32.equal t1.authors t2.authors

  let pp fmt {elements; edges; edits; authors = _ } =
    Fmt.pf fmt "@[<v>{ @[<v>elements: %a;@ edges: %a@ edits: %a@]@ }@]"
      Markers.pp elements
      Edges.pp edges
      Edits.pp edits

  let empty =
    { elements = Markers.empty ;
      edges = Edges.insert { left = Marker.beginning; right = Marker.ending;
                             author = -1l } Edges.empty;
      edits = Edits.empty ;
      authors = Markers.empty
                |> Markers.add Marker.beginning (-1l)
                |> Markers.add Marker.ending (-1l) ;
    }

  let singleton author marker element=
    { empty with elements = Markers.add marker (Live element) empty.elements ;
                 edges = Edges.(
                     insert {left=Marker.beginning; right=marker; author} empty
                     |> insert {left=marker; right=Marker.ending; author});
                 authors = Markers.add marker author empty.authors ;
    }

  let kill_marker t marker =
    let next_elements = Markers.mapi (fun elt_marker ->
        if elt_marker <> marker then (fun a -> a)
        else function
          | Live elt -> Tombstone elt
          | otherwise -> otherwise
      ) t.elements in
    Logs.debug (fun m ->
        m "KILLING marker %a in @[<v>old: %a@ next: %a@]" Marker.pp marker
          Markers.pp t.elements
          Markers.pp next_elements );
    {t with elements = next_elements }

  let merge t1 t2 =
    { elements = Markers.union (fun _ a b -> if a = b
                                 then Some a
                                 else failwith "TODO marker id conflict")
          t1.elements t2.elements ;
      edges = Edges.union t1.edges t2.edges ;
      edits = Edits.union t1.edits t2.edits ;
      authors = Markers.union (fun _ a b -> if a = b
                                then Some a
                                else failwith "TODO marker id conflict")
          t1.authors t2.authors ;
    }

  let diff t1 t2 =
    { elements = Markers.diff t1.elements t2.elements ;
      edges = Edges.diff t1.edges t2.edges ;
      edits = Edits.diff t1.edits t2.edits ;
      authors = Markers.diff t1.authors t2.authors;
    }

  module Snapshot = struct
    type snapshot = ((author_id * Marker.t) * element) Pvec.t

    let elements (snapshot:snapshot) : element Markers.t =
      Pvec.fold_left (fun acc ((_author, marker), element) ->
          Markers.add marker element acc) Markers.empty snapshot

    let live_elements (snapshot:snapshot) : (Marker.t * elt) Pvec.t =
      Pvec.fold_left (fun vec -> function
          | ((_author, marker), Live elt) -> Pvec.add_last vec (marker, elt)
          | _ -> vec) Pvec.empty snapshot

    let equal (a:snapshot) (b:snapshot) : bool =
      Pvec.equal a b ~eq:(fun
                           ((a_auth, a_mark), a_elt)
                           ((b_auth, b_mark), b_elt) ->
          Int32.equal     (a_auth) (b_auth)
          && Marker.equal (a_mark) (b_mark)
          && element_equal_ignoring_status (a_elt) (b_elt))

    let pp : Format.formatter -> snapshot -> unit =
      Pvec.pp ~sep:Fmt.(unit " -> ")
        Fmt.(pair ~sep:(unit":") (pair ~sep:comma int32 Marker.pp) pp_element)

    let to_edges (snapshot:snapshot) : Edges.t =
      Pvec.foldi_left
        (fun acc (idx:int) ((author,marker), _) ->
           match idx with
           | 0 when Marker.equal marker Marker.beginning -> acc
           | 0 -> Edges.insert { left = Marker.beginning;
                                 right = marker; author} acc
           | _ -> Edges.insert { left = snd@@fst@@Pvec.get snapshot (pred idx) ;
                                 right = marker; author} acc
        ) Edges.(empty |> insert
                   (let author, left =
                      if Pvec.is_empty snapshot
                      then (-1l), Marker.beginning
                      else fst @@ Pvec.get_last snapshot
                    in { left ;
                         right = Marker.ending ;
                         author }))
        snapshot |> fun produced_edges ->
      Logs.debug (fun m -> m "to_edges: %a produced: %a"
                     pp snapshot Edges.pp produced_edges
                 ) ;
      produced_edges

    let to_vector (snapshot:snapshot) : (author_id * E.t) Pvec.t =
      Pvec.filter_map (function
          | _, Tombstone _ -> None
          | (author, _marker), Live x -> Some (author, x)) snapshot


    let incoming_outgoing_multimap edges =
      assert (Edges.exists (fun e -> e.left = Marker.beginning) edges);
      assert (Edges.exists (fun e -> e.right = Marker.ending) edges);
      Edges.fold (fun edge (incoming, outgoing) ->
          assert (edge.left <> Marker.ending);
          assert (edge.right <> Marker.beginning);
          assert (edge.left <> edge.right);
          let incoming = match Markers.find_opt edge.right incoming with
            | None ->
              Markers.add edge.right (MarkerSet.singleton edge.left) incoming
            | Some set ->
              Markers.add edge.right (MarkerSet.add edge.left set) incoming
          in
          let outgoing = match Markers.find_opt edge.left outgoing with
            | None ->
              Markers.add edge.left (MarkerSet.singleton edge.right) outgoing
            | Some set ->
              Markers.add edge.left (MarkerSet.add edge.right set) outgoing
          in
          incoming, outgoing
        ) edges (Markers.empty, Markers.empty)

    let kahns (orig_edges: Edges.t) authors (* vertices : unit Markers.t *) =
      let (incoming_of_node, outgoing_of_node)
        : MarkerSet.t Markers.t * MarkerSet.t Markers.t =
        incoming_outgoing_multimap orig_edges
      in
      (*
L ← Empty list that will contain the sorted elements
S ← Set of all nodes with no incoming edges
while S is non-empty do
    remove a node n from S
    insert n into L
    for each node m with an edge e from n to m do
        remove edge e from the graph
        if m has no other incoming edges then
            insert m into S
if graph has edges then
    return error   (graph has at least one cycle)
else
    return L   (a topologically sorted order)
*)

      let rec for_each_node_m ~n ~(m:Marker.t) edges _S
          incoming_of_node outgoing_of_node =
        (* remove edge e from the graph *)
        let edges = Edges.filter (fun e -> e.left = n && e.right = m) edges in
        let incoming_of_node =
          Markers.update m (function
              | Some m_set -> Some (MarkerSet.remove n m_set)
              | None -> None) incoming_of_node in
        let outgoing_of_node =
          Markers.update m (function
              | Some m_set -> Some (MarkerSet.remove n m_set)
              | None -> None) outgoing_of_node in
        (* check if m has no more incoming edges: *)
        if (match Markers.find_opt m incoming_of_node with
            | Some m_set -> MarkerSet.is_empty m_set
            | None -> true)
        then begin
          (* insert m into S: *)
          Logs.debug (fun msg -> msg "adding %a to _S" Marker.pp m);
          let _S = MarkerSet.add m _S in
          edges, _S, incoming_of_node, outgoing_of_node
        end else begin
          Logs.debug (fun msg ->
              msg "NOT adding %a to _S because there are more incoming" Marker.pp m);
          edges, _S, incoming_of_node, outgoing_of_node
        end
      and while_s_nonempty edges _S _L incoming_of_node outgoing_of_node =

        (* instead of "choosing" an arbitrary {m} we pick the one with lowest
           author_id in order to guarantee consistent/convergent ordering: *)
        let n = MarkerSet.fold (fun this (old, old_author) ->
            let which =
              let this_author = Markers.find this authors in
              if 1 > Int32.compare this_author old_author
              then this, this_author else old, old_author
            in (*Logs.debug(fun m ->m"which:%a (%ld < %ld)"
                             Fmt.(pair Marker.pp int32) which
                             (Markers.find this authors)
                             (Markers.find old authors)
                         ); *)
            which ) _S (let marker = MarkerSet.choose _S in
                        marker, Markers.find marker authors
                       )
              |> fst
        in
        (* remove a node n from S: *)
        let _S = MarkerSet.remove n _S in
        (* add n to tail of L: *)
        Logs.debug (fun m -> m "adding to L: %a (other _S: %a)" Marker.pp n
                       Fmt.(list ~sep:(unit" ") Marker.pp)
                       (MarkerSet.elements _S));
        let _L = Pvec.add_last _L n in
        let edges, _S, incoming_of_node, outgoing_of_node =
          (* for each node m with an edge e from n to m: *)
          match Markers.find_opt n outgoing_of_node with
          | None -> edges, _S, incoming_of_node, outgoing_of_node
          | Some m_set ->
            Logs.debug (fun m -> m "kahn:%a has outgoing: %a"
                           Marker.pp n
                           Fmt.(list ~sep:(unit" ") Marker.pp)
                           (MarkerSet.elements m_set)
                       ) ;
            MarkerSet.fold (fun m
                             (edges, _S, incoming_of_node, outgoing_of_node) ->
                for_each_node_m ~n ~m edges _S incoming_of_node outgoing_of_node
              ) m_set (edges, _S, incoming_of_node, outgoing_of_node)
        in
        if not (MarkerSet.is_empty _S) then
          while_s_nonempty edges _S _L incoming_of_node outgoing_of_node
        else
          edges, _S, _L, incoming_of_node, outgoing_of_node
      in
      (* First, find a list of "start nodes" which have no incoming edges
         and insert them into a set S; at least one such node must exist
         in a non-empty acyclic graph: *)
      (* TODO ensure edges contains Marker.beginning *)
      let _S = MarkerSet.singleton Marker.beginning in
      if not (MarkerSet.is_empty _S) then
        let edges, _S, _L, _, _  =
          (* L ← Empty list that will contain the sorted elements *)
          let _L = Pvec.empty in
          while_s_nonempty orig_edges _S _L incoming_of_node outgoing_of_node in
        begin
          if not (Edges.is_empty edges) then
            Error (`Msg "not acyclic")
          else
            (* if graph has edges then
                 output error message (graph has at least one cycle)
               else
                 output message (proposed topologically sorted order: L) *)
            Ok _L
        end
      else Error (`Msg "edges is not empty at start")

    let of_t {edges = original_edges ; elements ; authors ; _ } : snapshot =
      let kahns_stuff () =
        match kahns original_edges authors with
        | Error `Msg x ->
          Logs.err (fun m -> m "kahns failed: %s" x) ;
          Pvec.empty
        | Ok vec ->
          Logs.debug (fun m -> m "kahn's succeeded!");
          Logs.debug (fun m -> m "%a" (Pvec.pp Marker.pp) vec) ;
          vec
      in
      let markers = kahns_stuff () in
      (*
      let longest_dag =
        (* Ensure we have nothing before BEGINNING: *)
        let () = assert(List.assoc Marker.beginning counted = 0) in
        (* Ensure we have at least one thing pointing to ENDING: *)
        let () = assert(List.assoc Marker.ending counted > 0) in

        Logs.debug (fun m ->
            m "counted vertices: @[<v>%a@]"
              Fmt.(list ~sep:(unit "@,")@@ pair ~sep pp_marker int) counted) ;
      *)
      let produced : snapshot =
        Pvec.fold_left (fun vec marker ->
            if not Marker.(equal marker beginning || equal marker ending) then
              Pvec.add_last vec ((Markers.find marker authors, marker),
                                 Markers.find marker elements)
            else vec)
          Pvec.empty markers in
      Logs.debug (fun m -> m "of_t: %a" pp produced);
      produced

    let extend_with_vector ~(author:author_id)
        (current_snapshot:snapshot) (original_vector:E.t Pvec.t) : snapshot =
      let pp_pair = Fmt.(pair ~sep:(unit ":")
                           (pair ~sep:comma int32 Marker.pp) pp_element) in
      let make_live element =
        let mark = (author, Marker.generate ()), Live element in
        Logs.debug (fun m ->
            m "Creating marker (%a)" pp_pair mark);
        mark
      in
      let with_markers = Pvec.map make_live in
      Pvec.foldi_left (fun (acc,(old_vector:elt Pvec.t)) _idx b_with_marker ->
          begin match Pvec.pop_first old_vector with
            | None -> (*done dealing with the source*)
              Logs.debug (fun m -> m "Adding b<%a> and remaining old_vec %d: %a"
                             pp_pair b_with_marker
                             (Pvec.length old_vector)
                             (Pvec.pp ~sep:Fmt.(unit", ") E.pp) old_vector);
              acc, old_vector
            | Some (vec_element, vector_tl) ->
              (* TODO make sure both are alive before all this *)
              let b_element = snd b_with_marker in
              let b_elt = (match snd b_with_marker with Live x -> x
                                                      | Tombstone x -> x) in
              if element_equal b_element (Live vec_element)
              then begin (* the subsets are equivalent: *)
                Logs.debug (fun m ->
                    m "extend_with_vector: adding equal %a element"
                      pp_pair b_with_marker);
                Pvec.add_last acc b_with_marker, vector_tl
              end else begin
                (* check if vector contains b's element
                   further down the road: *)
                begin match
                    Pvec.left_find (E.equal b_elt) vector_tl with
                | Some (next_idx, _) ->
                  (* - if it does: insert everything between here and there,
                       with new markers, increment offset *)
                  let left, right = Pvec.break_left next_idx vector_tl in
                  let right = Pvec.drop_left 1 right in
                  let new_vec_element = make_live vec_element in
                  Logs.debug (fun m -> m "insubset adding to acc: %a"
                                 pp_pair new_vec_element );
                  let acc = Pvec.add_last acc new_vec_element in
                  Logs.debug (fun m ->
                      m "extend_with_vector: adding left (%a) ; %a \
                         ; right(%a) subsets from %d"
                        (Pvec.pp E.pp) left
                        pp_pair b_with_marker
                        (Pvec.pp E.pp) right next_idx);
                  let acc = Pvec.append acc (with_markers left) in
                  let acc = Pvec.add_last acc b_with_marker in
                  let acc =
                    if b_element = Live b_elt
                    then acc
                    else Pvec.add_last acc (make_live b_elt)
                  in
                  acc , right
                | None ->
                  (*- if it doesn't: mark b as dead and insert it: *)
                  let dead_b = fst b_with_marker, Tombstone b_elt in
                  Logs.debug (fun m -> m "extend_with_vector: adding dead %a \
                                          element" pp_pair dead_b);
                  ( Pvec.add_last acc dead_b,
                    (* leave old vector intact: *) old_vector)
                end
              end
          end
        )
        (Pvec.empty, original_vector) (current_snapshot:snapshot)
      |> fun ((produced:snapshot), tl) ->
      Logs.debug (fun m -> m "produced: @[<v>%d [%a]@ \
                              tl: %d: [%a]@]"
                     (Pvec.length produced) pp produced
                     (Pvec.length tl) (Pvec.pp ~sep:Fmt.(unit">")E.pp) tl) ;
      Pvec.append produced (with_markers tl)
  end

  let update_with_vector author (t:t) vector =
    let old_snapshot = Snapshot.of_t t in
    Logs.debug (fun m -> m "update_with_vector: old_snapshot: %a"
                   Snapshot.pp old_snapshot);
    let next_snapshot =
      Snapshot.extend_with_vector ~author old_snapshot vector in
    Logs.debug (fun m -> m "update_with_vector: next_snapshot: %a"
                   Snapshot.pp next_snapshot);
    let next_elements =
      let new_elements = Snapshot.elements next_snapshot in
      (* produce a union just in case TODO this shouldn't be needed unless [t]
         contains elements not in the snapshot ... which it shouldn't.*)
      Markers.merge (fun marker a b ->
          match a, b with
          | None, elt_b -> elt_b (* new element *)

          | Some (Tombstone elt_a | Live elt_a), None ->
            Some (Tombstone elt_a) (* old dead element *)

          | Some (Tombstone _), Some (Live _b) ->
            Logs.err (fun m -> m "Illegal Tombstone->Live resurrection of %a"
                         Marker.pp marker ) ;
            failwith "TODO illegal state transition: Tombstone->Live"

          | Some (Tombstone elt_a |Live elt_a),Some ((Tombstone elt_b) as right)
          | Some (Live      elt_a), Some ((Live elt_b) as right)->
            if E.equal elt_a elt_b
            then Some right (* pick [b] to support tombstoning*)
            else begin
              Logs.err (fun m ->
                  m "TODO marker %a is not unique!" Marker.pp marker) ;
              failwith ("TODO markers must be unique") end
        )
        t.elements new_elements
    in
    Logs.debug (fun m -> m "next_snapshot: %a" Snapshot.pp next_snapshot);
    let edges_of_next_snapshot = Snapshot.to_edges next_snapshot in
    Logs.debug (fun m -> m "  ^-- edges: %a" Edges.pp edges_of_next_snapshot);
    let next_edges =
      let clean_edges next_edges old_edges =
        Edges.filter
        (fun next ->
           not @@ Edges.exists
             (fun {left;right; author = old_author;} ->
                left = next.left && right = next.right
                (* update if the old author is < than us to maintain priority*)
                && old_author < author) old_edges) next_edges
      in
      (* eliminate existing edges, ignoring author,
         to avoid taking ownership: *)
      let next_edges = (clean_edges edges_of_next_snapshot t.edges) in
      (* union to re-establish authorship of old edges: *)
      Edges.union next_edges (clean_edges t.edges next_edges)
    in
    let produced =
      let authors = (* TODO here we claim we wrote everything that isn't
                         already claimed, maybe not the greatest approach: *)
        Markers.merge (fun key _ -> function
            | None -> Some author
            | Some _ -> Some (Markers.find key t.authors)
          ) next_elements t.authors
      in
      { elements = next_elements ; edges = next_edges ; edits = Edits.empty
      ; authors } in
    Logs.debug (fun m -> m "Produced: %a" pp produced); produced

  let insert_element t ~author ~(after:Marker.t) ~before marker element =
    merge t
      { empty with
        elements = Markers.singleton marker (Live element) ;
        edges = Edges.(
            insert    {left=after ; right=marker       ; author} empty
            |> insert {left=marker; right=before; author} )
        ; authors = Markers.add marker author t.authors }

end

module UcharCRDT = struct
  module Uchar = struct include Uchar
    let pp fmt t= Fmt.pf fmt "%0x" (to_int t)
  end
  include CRDT(Uchar)
end

module CharCRDT = struct
  module Char = struct include Char
    let pp fmt = Fmt.pf fmt "%C"
  end
  include CRDT(Char)
end
