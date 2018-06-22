open Xi_crdt
module C = CharCRDT

let m = C.Marker.of_int64

let a_snapshot = Alcotest.testable C.Snapshot.pp C.Snapshot.equal
let a_char_vec =
  Alcotest.testable
    (Pvec.pp ~sep:Fmt.(unit "|") C.Char.pp)
    (Pvec.equal ~eq:Char.equal)

let qc_singleton =
  let open QCheck.Gen in
  (int_bound (0x01ffFFFF) >|= Int32.of_int) >>= fun author ->
  (int_bound (0x07ffFFFF) >|= Int64.of_int
   >|= C.Marker.of_int64) >>= fun marker ->
    (char >|= C.singleton author marker)

let qc_singleton_list : C.t list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 100) qc_singleton)

let qc_singleton_permutations : (C.t list * C.t list) QCheck.Gen.t =
  let open QCheck.Gen in
  qc_singleton_list >>= fun list_a ->
  shuffle_l list_a >|= fun list_b ->
  (list_a, list_b)

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

let test_singleton_qc_merge () : unit =
  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000
    ~name:"quickcheck permutations of singleton merges"
    (QCheck.make qc_singleton_permutations)
    (fun (a,b) ->
       let merge_and_snapshot lst =
         List.fold_left C.merge C.empty lst |> C.Snapshot.of_t in
       Alcotest.(check a_snapshot) "quickcheck permutations of singleton merges"
         (merge_and_snapshot a) (merge_and_snapshot b); true
    )

let test_singleton_append () : unit =
  let a = C.singleton 3l (m 10L) 'a' in
  let b = C.singleton 1l (m 20L) 'b' in
  let c = C.singleton 0l (m 30L) 'c' in
  let x = C.append b ~author:1l ~after:(m 20L) (m 40L)'x' in
  let snap_with_append =
    let cbxa = C.merge a b |> C.merge c |> C.merge x in
    C.Snapshot.of_t cbxa in
  let snap_without_append =
    let x = C.singleton 2l (m 40L) 'x' in
    let t = C.merge a b |> C.merge c |> C.merge x in
    C.Snapshot.of_t t
  in

  Alcotest.(check a_snapshot) "append is equivalent to merging with (author+1)"
    snap_without_append snap_with_append ;

  Alcotest.(check a_char_vec) "order is correct"
    (Pvec.of_list ['c';'b';'x';'a'])
    (C.Snapshot.to_vector snap_with_append)

let tests : unit Alcotest.test_case list = [
  "singleton merge", `Quick, test_singleton_merge ;
  "singleton append", `Quick, test_singleton_append ;
  "singleton quickcheck merge", `Slow, test_singleton_qc_merge ;
]
