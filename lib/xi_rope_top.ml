let printers = [
  (* TODO figure out how to install pretty-printers for functors?? *)
  (*"Xi_crdt.CRDT.pp";
  "Xi_crdt.CRDT.pp_element";
  "Xi_crdt.CRDT.Snapshot.pp";
  "Xi_crdt.CRDT.Marker.pp";
  "Xi_crdt.CRDT.Snapshot.pp";
  *)
  "Xi_crdt.CharCRDT.pp";
  "Xi_crdt.CharCRDT.pp_element";
  "Xi_crdt.CharCRDT.Snapshot.pp";
  "Xi_crdt.CharCRDT.Marker.pp";
  "Xi_crdt.CharCRDT.Snapshot.pp";

  "Xi_crdt.UcharCRDT.pp";
  "Xi_crdt.UcharCRDT.pp_element";
  "Xi_crdt.UcharCRDT.Snapshot.pp";
  "Xi_crdt.UcharCRDT.Marker.pp";
  "Xi_crdt.UcharCRDT.Snapshot.pp";
]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let () =
  if not (install_printers printers) then
    Format.eprintf "Problem installing Xi_rope-printers@."
