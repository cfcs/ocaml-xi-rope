open Xi_crdt
module C = CharCRDT

let m = C.Marker.of_int64

(******** Alcotest testable helpers: ********)

let a_snapshot = Alcotest.testable C.Snapshot.pp C.Snapshot.equal
let a_char_vec =
  Alcotest.testable
    (Pvec.pp ~sep:Fmt.(unit "|") C.Char.pp)
    (Pvec.equal ~eq:Char.equal)

(******** QuickCheck generators: ********)

let qc_singleton =
  let open QCheck.Gen in
  (int_bound (0x01ffFFFF) >|= Int32.of_int) >>= fun author ->
  (int_bound (0x07ffFFFF) >|= Int64.of_int
   >|= C.Marker.of_int64) >>= fun marker ->
    (char >|= C.singleton author marker)

let qc_singleton_list : C.t array QCheck.Gen.t =
  QCheck.Gen.(array_size (int_bound 100) qc_singleton)

let qc_singleton_permutations : (C.t array * C.t array) QCheck.Gen.t =
  let open QCheck.Gen in
  qc_singleton_list >>= fun array_a ->
  (let array_b = Array.copy array_a in
   shuffle_a array_a >|= fun () -> array_b) >|= fun gen_b ->
  (array_a, gen_b)

(******** tests: ********)

let test_singleton_merge () : unit =
  let a = C.singleton 1l (m 24L) 'c' in
  let b = C.singleton 2l (m 20L) 'b' in
  let c = C.singleton 3l (m 22L) 'a' in
  let d = C.singleton 4l (m 23L) 'd' in
  let e = C.singleton 5l (m 30L) 'e' in
  let abcde = C.merge a b |> C.merge c |> C.merge d |> C.merge e in
  let snap_abcde = C.Snapshot.of_t abcde in
  let edcba = C.merge e d |> C.merge c |> C.merge b |> C.merge a in
  let snap_edcba = C.Snapshot.of_t edcba in

  Alcotest.(check a_snapshot) "independent of merge order"
    snap_abcde snap_edcba ;

  Alcotest.(check a_char_vec) "order is correct"
    (Pvec.of_list ['c';'b';'a';'d';'e'])
    (C.Snapshot.to_vector snap_edcba)

let test_singleton_append () : unit =
  let a = C.singleton 3l (m 10L) 'a' in
  let b = C.singleton 1l (m 20L) 'b' in
  let c = C.singleton 0l (m 30L) 'c' in
  let x_marker = m 40L in
  let x_single = C.singleton 2l x_marker 'x' in
  let x_appended = C.insert_element b ~author:1l ~after:(m 20L) x_marker 'x' in

  let snap_without_append =
    let t = C.merge a b |> C.merge c |> C.merge x_single in
    C.Snapshot.of_t t
  in

  let cbxa_vec = Pvec.of_list ['c';'b';'x';'a'] in

  let vec_of_snap snap =
    Pvec.filter (function _,C.Live _ ->true | _,C.Tombstone _ -> false) snap
    |> Pvec.map (function _,C.Live x -> x | _,_ -> failwith "vec_of_snap") in

  Alcotest.(check a_char_vec) "order is correct with merges"
    (cbxa_vec) (vec_of_snap snap_without_append) ;

  let snap_with_append =
    let cbxa = C.merge a b |> C.merge c |> C.merge x_appended in
    C.Snapshot.of_t cbxa in
  Alcotest.(check a_char_vec) "order is correct with append"
    (cbxa_vec) (vec_of_snap snap_with_append) ;

  Alcotest.(check a_snapshot) "append is equivalent to merging with (author+1)"
    snap_without_append snap_with_append


let test_update_with_vector () : unit =
  let c_a = C.singleton 1l (m 10L) 'a' in
  let v_a = Pvec.of_list ['a'] in
  let v_x = Pvec.of_list ['x'] in
  let v_xax = Pvec.of_list ['x';'a';'x'] in
  let v_xbc = Pvec.of_list ['x';'b';'c'] in
  let () =
    let c_a' = C.update_with_vector 2l c_a v_a in
    let snap_a' = C.Snapshot.of_t c_a' in
    let a_vec = C.Snapshot.to_vector snap_a' in
    Alcotest.(check a_char_vec) "{'a'} to_vector [a] produces single {'a'}"
      (Pvec.of_list ['a']) a_vec in
  let () =
    let c_x = C.update_with_vector 3l c_a v_x in
    let snap = C.Snapshot.of_t c_x in
    let x_vec = C.Snapshot.to_vector snap in
    Alcotest.(check a_char_vec) "{'a'} to_vector [x] produces single 'x'"
      (Pvec.of_list ['x']) x_vec in
  let () =
    let upd_xax = C.update_with_vector 5l c_a v_xax in
    let snap_xax = C.Snapshot.of_t upd_xax in
    let xax_vec = C.Snapshot.to_vector snap_xax in
    Alcotest.(check a_char_vec) "{'a'} to_vector [x;a;x]produces x;{a};x"
      (Pvec.of_list ['x';'a';'x']) xax_vec ;
    let upd_xaxbc = C.update_with_vector 1l upd_xax v_xbc in
    let snap_xaxbc = C.Snapshot.of_t upd_xaxbc in
    let xaxbc_vec = C.Snapshot.to_vector snap_xaxbc in
    Alcotest.(check a_char_vec)
      "{'x';'a';'x'} to_vector [x;b;c] produces x;b;c"
      (v_xbc) xaxbc_vec ;
    Alcotest.(check int)
      "{x;a;x} to_vector [x;b;c] produces 5 elements"
      5 (Pvec.length snap_xaxbc) ;
    Alcotest.(check a_char_vec)
      "{x;a;x} to_vector [x;b;c] produces [x;-;b;-;c]"
      (Pvec.of_list ['x';'-'(*'a'*);'-'(*x*);'b';'c'])
      (Pvec.map (function _,C.Live c -> c
                        | _,C.Tombstone _ -> '-') snap_xaxbc)
  in
  let () =
    let c_b = C.singleton 2l (m 20L) 'b' in
    let ab = C.merge c_a c_b in
    let pvec_ab = Pvec.of_list ['a';'b'] in
    let pvec_b = Pvec.of_list ['b'] in
    let snap_ab = C.Snapshot.of_t ab in
    let oldvec = C.Snapshot.to_vector snap_ab in
    Alcotest.(check a_char_vec)
      "merge a b |> to_vector is Pvec [a;b]" pvec_ab oldvec;
    let vec = Pvec.rem oldvec 0 in (* remove 'a' *)
    Alcotest.(check a_char_vec)
      "Pvec [a;b] rem 0 is [b]" pvec_b vec ;
    let ab_minus_a = C.update_with_vector 3l ab vec in
    let snap_ab_minus_a = C.Snapshot.of_t ab_minus_a in
    let vec_ab_minus_a = C.Snapshot.to_vector snap_ab_minus_a in
    Alcotest.(check a_char_vec)
      "ab minus a = ['b']"
      (Pvec.of_list ['b']) vec_ab_minus_a ;
    let ab_minus_a_plus_a = C.update_with_vector 4l ab_minus_a oldvec in
    let snap_ab_minus_a_plus_a = C.Snapshot.of_t ab_minus_a_plus_a in
    let restored_vec_ab = C.Snapshot.to_vector snap_ab_minus_a_plus_a in
    Alcotest.(check a_char_vec)
      "Pvec [a;b] |> rem 0 |> update_with [a] is [a;b]"
      pvec_ab restored_vec_ab ;
    ()
  in
  ()

let test_singleton_qc_merge () : unit =
  (* Consider using check_cell_exn and only report back to Alcotest at the end*)

  (* Turn off logging to avoid writing 300 MB to disk: *)
  let old_log_level = Logs.level () in
  Logs.set_level (None) ;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000
    ~name:"quickcheck permutations of singleton merges"
    (QCheck.make qc_singleton_permutations)
    (fun (a,b) ->
       let merge_and_snapshot lst =
         Array.fold_left C.merge C.empty lst |> C.Snapshot.of_t in
       let a,b = (merge_and_snapshot a), (merge_and_snapshot b) in
       if Pvec.equal ~eq:(fun (a,b) (c,d) ->
           C.Marker.equal a c && C.element_equal b d) a b
       then true
       else begin (* complain if they are not equal: *)
         Alcotest.(check a_snapshot)
           "quickcheck permutations of singleton merges"
           (a) (b) ; false
       end ) ;
  Logs.set_level old_log_level

let test_qc_update_with_vector () : unit =
  (* Turn off logging to avoid writing 300 MB to disk: *)
  let old_log_level = Logs.level () in
  Logs.set_level (None) ;
  let shuffle_ac =
    let open QCheck.Gen in
    array_size (int_bound 17) char >>= fun arr_a ->
    let arr_b = Array.copy arr_a in
    shuffle_a arr_b >|= fun () ->
    arr_a , arr_b
  in
  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000
    ~name:"quickcheck update_with_vector random permutations"
    (QCheck.make shuffle_ac)
    (fun (arr_1,arr_2) ->
       let g_vec_1, g_vec_2 = Pvec.(of_array arr_1, of_array arr_2) in
       let a = C.update_with_vector 1l C.empty g_vec_1 in
       let b = C.update_with_vector 2l a g_vec_2 in
       let vec_a = C.Snapshot.(of_t a |> to_vector) in
       let vec_b = C.Snapshot.(of_t b |> to_vector) in
       if Pvec.equal ~eq:(fun ach bch -> Char.equal ach bch) g_vec_1 vec_a
           && Pvec.equal ~eq:(fun ach bch -> Char.equal ach bch) g_vec_2 vec_b
       then true
       else begin (* complain if they are not equal: *)
         Alcotest.(check a_char_vec)
           "quickcheck update_with_vector from random"
           (g_vec_1) (vec_a) ;
         Alcotest.(check a_char_vec)
           "quickcheck update_with_vector from random updated with vec"
           (g_vec_2) (vec_b) ;
         false
       end ) ;
  Logs.set_level old_log_level


(*
this should produce 'n';'a' with 'n' having a constraint to be < 'a':
let a = Xi_crdt.CharCRDT.(let a = singleton 1l (Marker.generate())'a' in a);;
CharCRDT.(let x = update_with_vector (-2l) a Pvec.(of_list ['n';'a']) in x);

also: verify that changing 'author' does not produce a snapshot with live
elements that different from the vector.
*)

let tests : unit Alcotest.test_case list = [
  "singleton merge", `Quick, test_singleton_merge ;
  "singleton append", `Quick, test_singleton_append ;
  "update_with_vector", `Quick, test_update_with_vector ;
  "quickcheck singleton merges", `Slow, test_singleton_qc_merge ;
  "quickcheck update_with_vector", `Slow, test_qc_update_with_vector ;
]
