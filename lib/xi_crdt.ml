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
    let beginning : t = 0L     (* Int64.min_int *)
    let ending    : t = 0x10000000L (*Int64.max_int TODO*)
    let compare = Int64.compare
    let pp fmt num = Fmt.pf fmt "0x%02Lx" num
    let equal = Int64.equal
    let of_int64 i = i
    let generate () : t =
      let rec skip num =
        if equal num beginning || equal num ending
        then skip (Random.State.int64 prng ending) else num
      in skip beginning
  end

  let pp_elt = E.pp

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

  let kill_marker t marker =
    let next_elements = Markers.update marker (function
         | Some (Live elt) -> Some (Tombstone elt)
         | otherwise -> otherwise) t.elements in
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
    }

  module Snapshot = struct
    type snapshot = (Marker.t * element) Pvec.t

    let elements (snapshot:snapshot) : element Markers.t =
      Pvec.fold_left (fun acc (marker, element) ->
          Markers.add marker element acc) Markers.empty snapshot

    let live_elements (snapshot:snapshot) : (Marker.t * elt) Pvec.t =
      Pvec.fold_left (fun vec ->
          function | (marker, Live elt) -> Pvec.add_last vec (marker, elt)
                   | _ -> vec) Pvec.empty snapshot

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
        ) Edges.(empty |> insert { left = (if Pvec.is_empty snapshot then
                                             Marker.beginning
                                           else
                                             fst @@ Pvec.get_last snapshot) ;
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
        let group_by_marker (xxx:(Marker.t * 'b list) list)
          : (Marker.t * 'b list) list =
          let rec loop acc = function
            | [] -> acc
            | (hd_k,hd_v)::tl ->
              begin match List.assoc_opt hd_k acc with
                | None -> loop ((hd_k,hd_v)::acc) tl
                | Some old_v ->
                  loop ((hd_k, hd_v@old_v)::(List.remove_assoc hd_k acc)) tl
              end
          in loop [] xxx
        in
        let outgoing_map =
          ( Edges.fold
              (fun acc edge ->
                 if Marker.equal Marker.ending edge.left then
                   raise @@ Failure "edge has marker  < BEGINNING";
                 if Marker.equal Marker.beginning edge.right then
                   raise @@ Failure "edge has ENDING < marker";
                 if List.mem_assoc edge.left acc then
                   List.map (fun (x,y) ->
                       if Marker.equal x edge.left
                       then x,(edge.author, edge.right)::y
                       else (x,y)
                     )acc
                 else
                   (edge.left, [(edge.author, edge.right)])::acc)
              [] original_edges) @ [Marker.ending, []]
        in
        let incoming_map =
          (* TODO this implementation needs to find the LONGEST path through the DAG*)
          (* https://en.wikipedia.org/wiki/Longest_path_problem *)
          ( Edges.fold
              (fun acc edge ->
                 if Marker.equal Marker.beginning edge.right then
                   raise @@ Failure "edge has marker  < BEGINNING";
                 if Marker.equal Marker.ending edge.left then
                   raise @@ Failure "edge has ENDING < marker";
                 if List.mem_assoc edge.right acc then
                   List.map (fun (x,y) ->
                       if Marker.equal x edge.right
                       then x,(edge.author, edge.left)::y
                       else (x,y)
                            )acc
                 else
                   (edge.right, [(edge.author, edge.left)])::acc)
              [] original_edges) @ [Marker.beginning, []]
        in
        let incoming_map =
          group_by_marker incoming_map
        in
        let () = (* sanity check *)
          List.fold_left (fun seen (marker, parents) ->
              (* no self-referential elements *)
              assert(not (List.exists (fun (_author, parent) ->
                  Marker.equal marker parent
                ) parents)) ;
              (* no double entries: *)
              assert(not (List.mem marker seen));
              marker::seen
            )
            [] incoming_map ;()
        in
        let sep = Fmt.unit "," in
        Logs.debug (fun m -> m "outgoing graph: @[<v>%a@]"
                       Fmt.(list ~sep:(unit "|@,")
                            @@ pair ~sep:(unit": ") Marker.pp
                              (list ~sep:(unit"; ")
                               @@ pair ~sep
                                 (brackets int32) Marker.pp )) incoming_map) ;
        Logs.debug (fun m -> m "incoming graph: @[<v>%a@]"
                       Fmt.(list ~sep:(unit "|@,")
                            @@ pair ~sep:(unit": ") Marker.pp
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
        let max_target_length = pred @@ List.length incoming_map in
        let rec recurse visited (siblings:(author_id * Marker.t) list)
          : author_id * Marker.t list * int =
          let marker_comma_element marker_lst = List.map (fun key ->
              key,try Some (Markers.find key elements)
              with _ -> None) marker_lst in
          let pp_markers_and_elements =
            Fmt.(list ~sep @@ pair ~sep:(unit":")
                   Marker.pp @@ option pp_element) in
          List.fold_left (fun (last_auth,acc, acc_count) (author, marker) ->
              assert(not (List.length acc > max_target_length)) ;
              if true && (* Turning this off makes update_vector fail*)
                 List.length acc = max_target_length
                 && (List.rev acc |> List.hd |> Marker.equal Marker.beginning)
              then begin
                let acc_count =
                  if (List.rev acc |> List.hd |> Marker.equal Marker.beginning)
                  then acc_count
                  else -10 (* decidedly not the solution *)
                in
                (* solution found: *)
                Logs.warn (fun m -> m "Skipping marker %a:%a, solution FOUND"
                              Fmt.int32 author Marker.pp marker);
                (last_auth,acc,acc_count)
              end else begin
                assert(not @@ Markers.mem marker visited);
                begin
                  let visited = Markers.add marker true visited in
                  let points_to_me = List.assoc marker incoming_map in
                  let (_auth, child_path, child_counts) =
                    recurse visited @@ List.filter (fun (_,m) ->
                        (*not @@Marker.(equal beginning) m*) true) points_to_me in
                  Logs.debug (fun m ->
                      m "marker %a children: %d path: %a"
                        pp_markers_and_elements (marker_comma_element [marker])
                        child_counts
                        pp_markers_and_elements
                        (marker_comma_element child_path) );
                  let actual_siblings : Markers.key list =
                    let siblings = siblings in
                    Logs.debug (fun m -> m "siblings before sort: %a"
                                   pp_markers_and_elements
                                   (marker_comma_element @@ List.map (fun (_,v)->v)siblings)
                               );
                    List.filter (fun (sib_author,sibmark) ->
                        not (List.exists (Marker.equal sibmark) child_path)
                        && not (List.mem (author,marker) @@ List.assoc sibmark incoming_map)
                        && not (List.mem (sib_author,sibmark) @@ List.assoc marker incoming_map)
                      ) siblings
                    |> List.sort (*sort ascending according to author: *)
                      (fun (aaa,a_m) (bbb,b_m) ->
                         if Int32.equal aaa bbb then Marker.compare a_m b_m
                         else Int32.compare aaa bbb)
                    |> List.fold_left
                      (fun acc ((_auth,marker) as elem) ->
                         match acc with
                         (* ignore same element with higher author id:*)
                         | (_, hd)::_ when Marker.equal marker hd -> acc
                         | acc -> elem::acc ) []
                    |> List.sort (*sort decending according to author: *)
                      (fun (aaa,a_m) (bbb,b_m) ->
                         begin match
                             Marker.equal a_m Marker.beginning,
                             Marker.equal b_m Marker.beginning with
                         | true, false -> -1 (* beginning sorts first*)
                         | false, true ->  1 (* beginning sorts first *)
                         | false, false when Int32.equal aaa bbb ->
                           compare (List.assoc a_m counted)
                             (List.assoc b_m counted)
                         | false, false -> Int32.compare bbb aaa
                         | true, true -> assert false (*TODO*)
                         end)
                    |> fun x ->
                    Logs.debug (fun m -> m "SORTED SIBS: %a"
                                   Fmt.(list ~sep:(unit"; ")@@
                                        pair ~sep:(unit":") int32
                                          pp_markers_and_elements
                                       )
                                   (List.map (fun (a,m) ->
                                        a, marker_comma_element [m]
                                      )x));
                    x|> List.split |> snd
                  in
                  let this_total =
                    List.assoc marker counted +
                    List.length actual_siblings + child_counts in
                  Logs.debug (fun m -> m "this_total: %d acc_count: %d"
                                 this_total acc_count) ;
                  begin if
                    (acc_count = max_target_length && not (List.mem (last_auth,List.hd acc)@@ List.assoc Marker.ending incoming_map))
                    ||
                    (* if acc has BEGINNING and is not max_target_len then discard it:*)
                    this_total > acc_count
                    (*
                    (1+List.length child_path > List.length acc
                     || (1+List.length child_path = List.length acc
                         && author < last_auth))*)
                    (*this_total > acc_count*)
                    (*|| (this_total = acc_count && author < last_auth)*)
                    then begin
                      Logs.debug (fun m ->
                          m "child_path: %a -- Actual siblings for %a : [%a] \
                             - out of [%a]"
                            pp_markers_and_elements
                            (marker_comma_element child_path)
                            pp_markers_and_elements
                            (marker_comma_element [marker])
                            pp_markers_and_elements
                            (marker_comma_element actual_siblings)
                            Fmt.(list ~sep @@ pair ~sep:(unit":")
                                   int32 Marker.pp) siblings );
                      let acc_after_insertion =
                        let rec insert_after ~tgt ~lst acc = function
                          | [] ->
                            Logs.err (fun m ->
                                m "GOT EMPTY INSERTAFTER tgt:[%a] \
                                   lst:[%a] acc:[%a]"
                                  pp_markers_and_elements
                                  (marker_comma_element [tgt])
                                  pp_markers_and_elements
                                  (marker_comma_element lst)
                                  pp_markers_and_elements
                                  (marker_comma_element acc)
                              );
                            acc @ lst
                          | hd::tl when Marker.equal hd tgt ->
                            Logs.debug (fun m ->
                                m "insert_after: tgt:%a lst:[%a] tl:[%a] acc: [%a]"
                                  pp_markers_and_elements
                                  (marker_comma_element [tgt])
                                  pp_markers_and_elements
                                  (marker_comma_element lst)
                                  pp_markers_and_elements
                                  (marker_comma_element tl)
                                  pp_markers_and_elements
                                  (marker_comma_element acc)
                              );
                            List.rev acc @ tgt :: tl @  lst
                          | hd::tl -> insert_after ~tgt ~lst (hd::acc) tl
                        in
                        if true && not (List.exists (Marker.equal marker) child_path)
                        then
                          insert_after
                            ~tgt:marker ~lst:child_path [] actual_siblings
                        else begin
                          Logs.debug (fun m ->
                              m "skipping insert because %a is in [%a]@@[%a]"
                                pp_markers_and_elements (marker_comma_element [marker])
                                pp_markers_and_elements
                                (marker_comma_element actual_siblings)
                                pp_markers_and_elements
                                (marker_comma_element child_path)
                            );
                          (actual_siblings)@child_path
                        end
                      in
                      Logs.debug (fun m -> m "sorted after insert: %a"
                                     pp_markers_and_elements
                                     (marker_comma_element acc_after_insertion)) ;
                      if List.exists
                          (Marker.equal Marker.beginning)
                          acc_after_insertion then
                        begin
                          Logs.warn (fun m -> m "Got BEGINNING, hd: %B, acc len\
                                                 : %d, max: %d score: %d"
                                        (List.hd (List.rev acc_after_insertion)
                                         |> Marker.equal Marker.beginning)
                                        (List.length acc_after_insertion)
                                        max_target_length
                                        this_total
                                    )
                        end;
                      (author, (acc_after_insertion), this_total)
                    end else begin
                      Logs.debug (fun m -> m "rejecting @[<v>(%ld)%a \
                                              score:%d \
                                              < @,(%ld)%a score:%d@]"
                                     author
                                     Fmt.(list ~sep @@ pair ~sep:(unit":")
                                            Marker.pp @@ option pp_element)
                                     (List.map (fun key ->
                                          key,try Some (Markers.find key elements)
                                          with _ -> None) (marker::child_path))
                                     this_total
                                     last_auth
                                     Fmt.(list ~sep @@ pair ~sep:(unit":")
                                            Marker.pp @@ option pp_element)
                                     (List.map (fun key ->
                                          key,try Some (Markers.find key elements)
                                          with _ -> None) acc) acc_count );
                      (last_auth,acc,acc_count)
                    end
                  end
                end
              end
            ) (Int32.max_int, [], ~-1)
            (let rejected, siblings =
               (List.partition (fun (_,m) -> Markers.mem m visited)siblings)
             in
             if [] <> rejected then
             Logs.warn (fun m -> m "rejecting already visited siblings: %a"
                            Fmt.(list ~sep:(unit", ") (pair ~sep:(unit":")
                                                         int32 Marker.pp))
                            rejected);
             (List.sort (fun (a,_) (b,_) -> Int32.compare b a)siblings)
          )
        in
        recurse Markers.empty (List.assoc Marker.ending incoming_map)
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
      let pp_pair = Fmt.(pair ~sep:(unit ":") Marker.pp pp_element) in
      let make_live element =
        let mark = Marker.generate (), Live element in
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
        (Pvec.empty, original_vector) current_snapshot
      |> fun (produced, tl) ->
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
    let next_snapshot = Snapshot.extend_with_vector old_snapshot vector in
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
    let edges_of_next_snapshot = Snapshot.to_edges author next_snapshot in
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
    let pp fmt = Fmt.pf fmt "%C"
  end
  include CRDT(Char)
end
