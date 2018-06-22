open Xi_crdt
module C = CharCRDT

let m = C.Marker.of_int64

let a_snapshot = Alcotest.testable C.Snapshot.pp C.Snapshot.equal

let test_five_singletons () : unit =
  let a = C.singleton 1l (m 24L) 'c' in
  let b = C.singleton 2l (m 20L) 'b' in
  let c = C.singleton 3l (m 22L) 'a' in
  let d = C.singleton 4l (m 23L) 'd' in
  let e = C.singleton 5l (m 30L) 'e' in
  let abcde = C.merge a b |> C.merge c |> C.merge d |> C.merge e in
  let snap_abcde = C.Snapshot.of_t abcde in
  let edcba = C.merge e d |> C.merge c |> C.merge b |> C.merge a in
  let snap_edcba = C.Snapshot.of_t edcba in
  (* TODO verify correct order: c ; b ; a ; d ; e *)
  Alcotest.(check a_snapshot) "independent of merge order"
    snap_abcde snap_edcba

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
  (* TODO verify correct order: c;b;x;a *)
  Alcotest.(check a_snapshot) "append is equivalent to merging with (author+1)"
    snap_without_append snap_with_append


let tests : unit Alcotest.test_case list = [
  "singleton ordering", `Quick, test_five_singletons ;
  "singleton append", `Quick, test_singleton_append ;
]
