open Xi_crdt
module C = CharCRDT

let m = C.Marker.of_int64

(******** Alcotest testable helpers: ********)

let a_snapshot = Alcotest.testable C.Snapshot.pp C.Snapshot.equal
let a_char_vec =
  Alcotest.testable
    (Pvec.pp ~sep:Fmt.(unit "|") C.Char.pp)
    (Pvec.equal ~eq:Char.equal)

let a_author_char_vec =
  Alcotest.testable
    (Pvec.pp ~sep:Fmt.(unit "|") Fmt.(pair ~sep:comma int32 C.Char.pp))
    (Pvec.equal ~eq:(fun (a,b) (a2,b2)-> Int32.equal a a2 && Char.equal b b2))


(******** QuickCheck generators: ********)

let qc_singleton =
  let open QCheck.Gen in
  (int_bound (0x01ffFFFF) >|= Int32.of_int) >>= fun author ->
  (int_bound (0x07ffFFFF) >|= Int64.of_int
   >|= C.Marker.of_int64) >>= fun marker ->
    (char >|= C.singleton author marker)

let qc_singleton_list : C.t array QCheck.Gen.t =
  QCheck.Gen.(array_size (int_bound 300) qc_singleton)

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

  Alcotest.(check a_author_char_vec) "order is correct"
    (Pvec.of_list [1l,'c';2l,'b';3l,'a';4l,'d';5l,'e'])
    (C.Snapshot.to_vector snap_edcba)

let test_singleton_append () : unit =
  let a = C.singleton 3l (m 10L) 'a' in
  let b = C.singleton 1l (m 20L) 'b' in
  let c = C.singleton 0l (m 30L) 'c' in
  let x_marker = m 40L in
  let x_single = C.singleton 2l x_marker 'x' in
  let x_appended = C.insert_element b ~author:1l ~before:C.Marker.ending
      ~after:(m 20L) x_marker 'x' in
  let x_appended_same = C.insert_element b ~author:2l ~before:C.Marker.ending
      ~after:(m 20L) x_marker 'x' in

  let snap_without_append =
    let t = C.merge a b |> C.merge c |> C.merge x_single in
    C.Snapshot.of_t t
  in

  let cbxa_vec = Pvec.of_list ['c';'b';'x';'a'] in

  let vec_of_snap (snap:C.Snapshot.snapshot) =
    Pvec.filter (function _,C.Live _ ->true | _,C.Tombstone _ -> false) snap
    |> Pvec.map (function _,C.Live x -> x | _,_ -> failwith "vec_of_snap") in

  Alcotest.(check a_char_vec) "order is correct with merges"
    (cbxa_vec) (vec_of_snap snap_without_append) ;

  let snap_with_append =
    let cbxa = C.merge a b |> C.merge c |> C.merge x_appended in
    C.Snapshot.of_t cbxa in
  let snap_with_append_same =
    let cbxa = C.merge a b |> C.merge c |> C.merge x_appended_same in
    C.Snapshot.of_t cbxa in

  Alcotest.(check a_char_vec) "order is correct with append (author+1)"
    (cbxa_vec) (vec_of_snap snap_with_append) ;
  Alcotest.(check a_char_vec) "order is correct with append"
    (cbxa_vec) (vec_of_snap snap_with_append_same) ;


  Alcotest.(check a_snapshot) "append is equivalent to merging with"
    snap_without_append snap_with_append_same

let test_singleton_diff () : unit =
  let a = C.singleton 3l (m 10L) 'a' in
  let b = C.singleton 1l (m 20L) 'b' in
  let ab = C.merge a b in
  let ab_diff_b = C.diff ab b in
  let ab_diff_b_merge_a = C.merge a ab_diff_b in
  Alcotest.(check a_snapshot) "a+b diff b = a"
    (C.Snapshot.of_t a) (C.Snapshot.of_t ab_diff_b_merge_a)

let strip_author = Pvec.map (fun (_a,e) -> e)
let add_author author = Pvec.map (fun e -> (author, e))

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
    Alcotest.(check a_author_char_vec) "{'a'} to_vector [a] produces single {'a'}"
      (Pvec.of_list [1l, 'a']) a_vec in
  let () =
    let c_x = C.update_with_vector 3l c_a v_x in
    let snap = C.Snapshot.of_t c_x in
    let x_vec = C.Snapshot.to_vector snap in
    Alcotest.(check a_author_char_vec) "{'a'} to_vector [x] produces single 'x'"
      (Pvec.of_list [3l, 'x']) x_vec in
  let () =
    let upd_xax = C.update_with_vector 5l c_a v_xax in
    let snap_xax = C.Snapshot.of_t upd_xax in
    let xax_vec = C.Snapshot.to_vector snap_xax in
    Alcotest.(check a_author_char_vec) "{'a'} to_vector [x;a;x]produces x;{a};x"
      (Pvec.of_list [5l,'x';1l,'a';5l,'x']) xax_vec ;
    let upd_xaxbc = C.update_with_vector 1l upd_xax v_xbc in
    let snap_xaxbc = C.Snapshot.of_t upd_xaxbc in
    let xaxbc_vec = C.Snapshot.to_vector snap_xaxbc in
    Alcotest.(check a_author_char_vec)
      "{'x';'a';'x'} to_vector [x;b;c] produces x;b;c"
      (Pvec.of_list [5l,'x';1l,'b';1l,'c']) xaxbc_vec ;
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
    let snap_ab = C.Snapshot.of_t ab in
    let oldvec = C.Snapshot.to_vector snap_ab in
    Alcotest.(check a_author_char_vec)
      "merge a b |> to_vector is Pvec [a;b]"
      (Pvec.of_list [1l,'a'; 2l,'b']) oldvec;
    let vec = Pvec.rem oldvec 0 in (* remove 'a' *)
    Alcotest.(check a_author_char_vec)
      "Pvec [a;b] rem 0 is [b]" (Pvec.of_list[ 2l, 'b']) vec ;
    let ab_minus_a = C.update_with_vector 3l ab (strip_author vec) in
    let snap_ab_minus_a = C.Snapshot.of_t ab_minus_a in
    let vec_ab_minus_a = C.Snapshot.to_vector snap_ab_minus_a in
    Alcotest.(check a_author_char_vec)
      "ab minus a = ['b']"
      (Pvec.of_list [2l, 'b']) vec_ab_minus_a ;
    let ab_minus_a_plus_a = C.update_with_vector 4l
        ab_minus_a (strip_author oldvec) in
    let snap_ab_minus_a_plus_a = C.Snapshot.of_t ab_minus_a_plus_a in
    let restored_vec_ab = C.Snapshot.to_vector snap_ab_minus_a_plus_a in
    Alcotest.(check a_author_char_vec)
      "Pvec [a;b] |> rem 0 |> update_with [a] is [a;b]"
      (Pvec.of_list [4l,'a'; 2l,'b']) restored_vec_ab
  in
  let () =
    let vec_abc = Pvec.(of_list ['a';'b';'c']) in
    let abc = C.(update_with_vector 1l empty vec_abc) in
    let zero = C.update_with_vector 2l abc Pvec.empty in
    let restored_1 = C.update_with_vector 3l zero vec_abc in
    let restored_1_vec = C.Snapshot.(of_t restored_1 |> to_vector) in
    Alcotest.(check a_author_char_vec)
      "[a;b;c] -> [] -> [a;b;c]"
      (Pvec.of_list[ 3l, 'a' ; 3l, 'b'; 3l, 'c' ]) restored_1_vec ;
    let redeleted_1 = C.update_with_vector 4l restored_1 Pvec.empty in
    let restored_2 = C.update_with_vector 5l redeleted_1 vec_abc in
    let restored_2_vec = C.Snapshot.(of_t restored_2 |> to_vector) in
    Alcotest.(check a_author_char_vec)
      "[a;b;c] -> [] -> [a;b;c] -> [] -> [a;b;c]"
      (Pvec.of_list[ 5l, 'a'; 5l, 'b'; 5l, 'c']) restored_2_vec ;
    let redeleted_2 = C.update_with_vector 6l restored_2 Pvec.empty in
    let restored_3 = C.update_with_vector 7l redeleted_2 vec_abc in
    let restored_3_vec = C.Snapshot.(of_t restored_3 |> to_vector) in
    Alcotest.(check a_author_char_vec)
      "[a;b;c] -> [] -> [a;b;c] -> [] -> [a;b;c] -> [] -> [a;b;c]"
      (Pvec.of_list[ 7l, 'a'; 7l, 'b'; 7l, 'c']) restored_3_vec ;
  in
  (* TODO in demo: ab^H^Hab^H^Hab -> ba ???? WTF *)
  let () =
    let vec_ab = Pvec.of_list ['a';'b'] in
    let ab_1 = C.update_with_vector 1l C.empty vec_ab in (* [a;b] *)
    let ab_1_snap = C.Snapshot.of_t ab_1 in
    let killed_1 = Pvec.fold_left (fun t ((_, marker),_)->
        C.kill_marker t marker (* ^H^H = [] *)
      ) ab_1 ab_1_snap in
    let ab_2 = C.update_with_vector 2l killed_1 vec_ab in (* [a;b] *)
    let ab_2_snap = C.Snapshot.of_t ab_2 in
    let killed_2 = Pvec.fold_left (fun t ((_, marker),_)->
        C.kill_marker t marker (* ab^H^H = [] *)
      ) ab_2 ab_2_snap in
    let ab_3 = C.update_with_vector 2l killed_2 vec_ab in (* [a;b]*)
    let ab_3_snap = C.Snapshot.of_t ab_3 in
    let ab_3_vec = C.Snapshot.to_vector ab_3_snap in
    Alcotest.(check a_author_char_vec)
      "[a;b] -> ^H^H -> [a;b] -> ^H^H -> [a;b]"
      (Pvec.of_list[ 2l, 'a'; 2l, 'b']) ab_3_vec
  in
  let () = (* test regression *)
    let vec_123 = Pvec.of_list ['1';'2';'3'] in
    let old = C.(update_with_vector 1l empty vec_123) in
    let next = C.update_with_vector 2l old Pvec.empty in
    let last = C.update_with_vector 3l next vec_123 in
    let last_vec = C.Snapshot.(of_t last |> to_vector) in
    Alcotest.(check a_author_char_vec)
      "[1;2;3] -> [] -> [1;2;3]"
      (Pvec.of_list[ 3l,'1'; 3l,'2'; 3l,'3' ]) last_vec
  in
  let () = (* test regression *)
    let last_live c = C.Snapshot.(of_t c |> live_elements) |> Pvec.get_last in
    let vec_1 = Pvec.of_list ['1'] in
    let vec_12 = Pvec.of_list ['1';'2'] in
    let vec_123 = Pvec.of_list ['1';'2';'3'] in
    let c1 = C.update_with_vector 1l C.empty vec_1 in
    Alcotest.(check a_author_char_vec)
      "1; = [1;]"
      (Pvec.of_list[ 1l,'1' ]) C.Snapshot.(of_t c1 |> to_vector) ;
    let c2 = C.update_with_vector 2l c1 vec_12 in
    Alcotest.(check a_author_char_vec)
      "1;2 = [1;2]"
      (Pvec.of_list[1l,'1';2l,'2']) C.Snapshot.(of_t c2 |> to_vector) ;
    let c3 = C.update_with_vector 3l c2 vec_123 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 = [1;2;3]"
      (Pvec.of_list[1l,'1';2l,'2';3l,'3']) C.Snapshot.(of_t c3 |> to_vector) ;
    let c4 = C.kill_marker c3 (last_live c3 |> fst) in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H = [1;2]"
      (Pvec.of_list[1l,'1';2l,'2']) C.Snapshot.(of_t c4 |> to_vector) ;
    let c5 = C.kill_marker c4 (last_live c4 |> fst) in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H = [1;]"
      (Pvec.of_list[1l,'1';]) C.Snapshot.(of_t c5 |> to_vector) ;
    let c6 = C.kill_marker c5 (last_live c5 |> fst) in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H = []"
      Pvec.empty C.Snapshot.(of_t c6 |> to_vector) ;
    let c7 = C.update_with_vector 7l c6 vec_1 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1; = [1;]"
      (Pvec.of_list[7l,'1';]) C.Snapshot.(of_t c7 |> to_vector) ;
    let c8 = C.update_with_vector 8l c7 vec_12 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2 = [1;2]"
      (Pvec.of_list[8l,'1';8l,'2']) C.Snapshot.(of_t c8 |> to_vector) ;
    let c9 = C.update_with_vector 9l c8 vec_123 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2;3 = [1;2;3]"
      (Pvec.of_list[8l,'1';9l,'2';9l,'3']) C.Snapshot.(of_t c9 |> to_vector) ;
    let c10 = C.kill_marker c9  (last_live c9 |> fst) in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2;3 -> ^H = [1;2]"
      (Pvec.of_list[8l,'1';9l,'2']) C.Snapshot.(of_t c10 |> to_vector) ;
    let c11 = C.kill_marker c10 (last_live c10 |> fst) in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2;3 -> ^H^H = [1;]"
      (Pvec.of_list[8l,'1';]) C.Snapshot.(of_t c11 |> to_vector) ;
    let c12 = C.kill_marker c11 (last_live c11 |> fst) in
    Alcotest.(check a_author_char_vec)
      "123-> ^H^H^H -> 123 -> ^H^H^H = []"
      Pvec.empty C.Snapshot.(of_t c12 |> to_vector) ;
    let c13 = C.update_with_vector 13l c12 vec_1 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2;3 -> ^H^H^H -> 1; = [1;]"
      (Pvec.of_list[13l,'1']) C.Snapshot.(of_t c13 |> to_vector) ;
    let c14 = C.update_with_vector 14l c13 vec_12 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2;3 -> ^H^H^H -> 1;2 = [1;2]"
      (Pvec.of_list[14l,'1';14l,'2']) C.Snapshot.(of_t c14 |> to_vector) ;
    let c15 = C.update_with_vector 15l c14 vec_123 in
    Alcotest.(check a_author_char_vec)
      "1;2;3 -> ^H^H^H -> 1;2;3 -> ^H^H^H -> 1;2;3 = [1;2;3]"
      (Pvec.of_list[14l,'1';15l,'2';15l,'3'])
      C.Snapshot.(of_t c15 |> to_vector) ;
  in
  let () = (* regression *)
    for _i = 0 to 30 do
      let pv = Pvec.of_list in let uv = C.update_with_vector 1l in
      let tovec c = C.Snapshot.(of_t c |> to_vector) in
      let c1 = uv C.empty (pv['a']) in
      let c2 = uv c1 (pv['a';'b']) in
      Alcotest.(check a_char_vec) " c2: a -> ab"
        (pv ['a';'b']) (tovec c2 |> strip_author) ;
      let c3 = uv c2 (pv[]) in
      let c4 = uv c3 (pv['a']) in
      Alcotest.(check a_char_vec) " c4: a -> ab -> [] -> a"
        (pv ['a']) (tovec c4 |> strip_author);
      let c5 = uv c4 (pv['a';'b']) in
      Alcotest.(check a_char_vec) " c5: a -> ab -> [] -> a -> ab"
        (pv ['a';'b']) (tovec c5 |> strip_author);
      let c6 = uv c5 (pv[]) in
      Alcotest.(check a_char_vec) " c6: a -> ab -> [] -> a -> ab -> []"
        (pv []) (tovec c6 |> strip_author);
      let c7 = uv c6 (pv['a']) in
      Alcotest.(check a_char_vec) " c7: a -> ab -> [] -> a -> ab -> [] -> a"
        (pv ['a']) (tovec c7 |> strip_author);
      let c8 = uv c7 (pv['a';'b']) in
      Alcotest.(check a_char_vec) " c8: a -> ab -> [] -> a -> ab -> [] -> a -> \
                                   ab"
        (pv ['a';'b']) (tovec c8 |> strip_author);
      let c9 = uv c8 (pv[]) in
      Alcotest.(check a_char_vec) " c9: a -> ab -> [] -> a -> ab -> [] -> a -> \
                                   ab -> []"
        (pv []) (tovec c9 |> strip_author);
      let c10 = uv c9 (pv['a']) in
      Alcotest.(check a_char_vec) "c10: a -> ab -> [] -> a -> ab -> [] -> a -> \
                                   ab -> [] -> a"
        (pv ['a']) (tovec c10 |> strip_author);
      let c11 = uv c10 (pv['a';'b']) in
      Alcotest.(check a_char_vec) "c11: a -> ab -> [] -> a -> ab -> [] -> a -> \
                                   ab -> [] -> a -> ab"
        (pv ['a';'b'])
        (tovec c11 |> strip_author)
    done
  in
  let () = (* regression found by quickcheck *)
    let vec1 = Pvec.of_list ['a';'b';'c';'d';'e'] in
    let vec2 = Pvec.of_list ['e';'a';'d';'c';'b'] in
    Alcotest.(check a_author_char_vec)
      "update_with_vector: [] -> abcde -> eadcb"
      (Pvec.of_list[2l,'e'; 1l,'a'; 2l,'d' ; 2l,'c'; 1l,'b'])
      CharCRDT.(update_with_vector 1l empty vec1
                |> fun c1 ->
                update_with_vector 2l c1 vec2
                |> fun cf ->
                Snapshot.(of_t cf |> to_vector))
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
           C.Marker.equal (snd a) (snd c) && C.element_equal b d) a b
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
    array_size (int_bound 100) char >>= fun arr_a ->
    let arr_b = Array.copy arr_a in
    shuffle_a arr_b >|= fun () ->
    arr_a , arr_b
  in
  QCheck.Test.check_cell_exn @@ QCheck.Test.make_cell ~count:1000
    ~name:"quickcheck update_with_vector random permutations"
    (QCheck.make shuffle_ac)
    (fun (arr_1,arr_2) ->
       let g_vec_1, g_vec_2 = Pvec.(of_array arr_1, of_array arr_2) in
       let a = C.update_with_vector 1l C.empty g_vec_1 in
       let b = C.update_with_vector 2l a g_vec_2 in
       let vec_a = C.Snapshot.(of_t a |> to_vector) in
       let vec_b = C.Snapshot.(of_t b |> to_vector) in
       let vec_e_eq ach bch = Char.equal (snd ach) (snd bch) in
       if Pvec.equal ~eq:vec_e_eq (add_author 1l g_vec_1) vec_a
       && Pvec.equal ~eq:vec_e_eq (add_author 2l g_vec_2) vec_b
       then true
       else begin (* complain if they are not equal: *)
         Alcotest.(check a_author_char_vec)
           "quickcheck update_with_vector from random"
           (add_author 1l g_vec_1) (vec_a) ;
         let s_vec_1 =
           let px = Pvec.(pp ~sep:Fmt.(unit ";") (fun fmt c -> Fmt.pf fmt "%C" c)) in
           Fmt.strf "%a" px g_vec_1
         in
         Alcotest.(check a_author_char_vec)
           ("quickcheck update_with_vector from random updated with vec " ^ s_vec_1)
           (add_author 2l g_vec_2) (vec_b) ;
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

(* TODO negative test case to ensure that we catch marker id collisions *)

let tests : unit Alcotest.test_case list = [
  "singleton merge", `Quick, test_singleton_merge ;
  "singleton append", `Quick, test_singleton_append ;
  "singleton diff", `Quick, test_singleton_diff ;
  "update_with_vector", `Quick, test_update_with_vector ;
  "quickcheck singleton merges", `Slow, test_singleton_qc_merge ;
  "quickcheck update_with_vector", `Slow, test_qc_update_with_vector ;
]
