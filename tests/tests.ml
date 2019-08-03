let tests : (string * unit Alcotest.test_case list) list = [
  "boilerplate", ["foo", `Quick, (fun () -> ()) ];
  "Xi_crdt tests", Test_xi_crdt.tests ;
]

let () =
  Printexc.record_backtrace true ;
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter () ;
  Logs.(set_level @@ Some Debug) ;
  Alcotest.run "xi-rope test suite" tests
