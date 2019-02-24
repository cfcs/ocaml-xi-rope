(* this was blatantly copied from
https://github.com/cedlemo/OCaml-Notty-introduction#update-a-terminal-interface-with-a-timeout

by cedlemo, thank you very much for this example!
*)

open Lwt.Infix
open Notty

module Term = Notty_lwt.Term

module C = Xi_crdt.UcharCRDT

let author = Random.self_init () ; Random.int32 999l
let counter = ref 0
let document = ref C.empty
let cursor = ref (0,0)
let debug = ref Pvec.empty

type edit =
  { author: C.author_id ;
    element: C.elt ;
    alive: bool;
    before: C.Marker.t ;
    after: C.Marker.t ;
    marker: C.Marker.t ;
  }
let write_mvar : edit Lwt_mvar.t = Lwt_mvar.create_empty ()

let merge_edge ~author ~after ~before marker element =
  document := C.insert_element !document ~author ~after ~before marker element ;
  debug := Pvec.add_first ("got something from peer") !debug

let strip_author = Pvec.map (fun (_a,e) -> e)

let insert_uchars ?(author=author) ?(first=fst !cursor) chars =
  let old_vec = C.Snapshot.(of_t !document |> to_vector) in
  let vec = Pvec.splice ~first
      ~into:old_vec chars in
  let old_document = !document in
  document := C.update_with_vector author !document (strip_author vec) ;
  let diff = C.diff old_document !document in
  let _ =
    (*let edit = {
      author ;
      element ;
      alive ;
      before; after; marker;
      } in ()*) ()
  in
  debug := Pvec.add_first (Fmt.strf "update_with_vector:\
                                     @[<v> %a@,old: %a@,new: %a@]"
                             Pvec.(pp ~sep:Fmt.(unit" -> ")
                                     Fmt.(pair ~sep:comma int32 C.pp_elt)) vec
                             C.pp old_document C.pp !document
                          ) !debug ;
  (* debug: *)
  let actual_vec = C.Snapshot.(of_t !document |> to_vector) in
  if Pvec.to_list vec <> Pvec.to_list actual_vec then
    debug := Pvec.add_first ("shit!!!") !debug

let delete_range first last =
  let old_vec = C.Snapshot.(of_t !document |> to_vector) in
  let vec = Pvec.rem_range ~first ~last old_vec in
  let old_document = !document in
  document := C.update_with_vector author !document (strip_author vec) ;
  debug := Pvec.add_first (Fmt.strf "delete range \\[%d;%d\\]:\
                                     @[<v> %a@,old: %a@,new: %a@]"
                             first last
                             Pvec.(pp ~sep:Fmt.(unit" -> ")
                                     Fmt.(pair ~sep:comma int32 C.pp_elt)) vec
                             C.pp old_document C.pp !document
                          ) !debug ;
  let actual_vec = C.Snapshot.(of_t !document |> to_vector) in
  if Pvec.to_list vec <> Pvec.to_list actual_vec then
    debug := Pvec.add_first ("shit failed deleting!!!") !debug

let move_cursor term = function
  | `Right ->
    cursor := (min (Term.size term |> fst) (fst !cursor +1)),(snd !cursor)
  | `Left ->
    cursor := max 0 @@ fst !cursor -1 ,snd !cursor
  | `Down ->
    cursor := fst !cursor, succ @@ snd !cursor
  | `Up ->
    cursor := fst !cursor, max 0 @@ pred @@ snd !cursor
  | `Home ->
    cursor := 0, snd !cursor

let erase_at_cursor i =
  if i < 0 then ()
  else begin
    let markers = C.Snapshot.(of_t !document |> live_elements) in
    if i >= Pvec.length markers then ()
    else
      let marker, elem = Pvec.get markers i in
      let old_document = !document in
      document := C.kill_marker !document marker ;
      debug := Pvec.add_first Fmt.(strf "erase cursor:%d deleting:%a,%a@.  \
                                         @[<v>from %a@ to   : %a@]"
                                     i C.Marker.pp marker C.pp_elt elem
                                     C.pp old_document C.pp !document
                   ) !debug
  end

let erase_before_cursor term i =
  erase_at_cursor (pred i) ;
  move_cursor term `Left

let rec increase_counter () =
  ( Lwt_unix.sleep 1.1 >|= fun () ->
    if !counter < max_int then
      incr counter
    else counter := 0
  ) >>= increase_counter

let initialize_socket ~write_mvar ~read_mvar : unit Lwt.t =
  begin Lwt.try_bind (fun () -> Lwt_unix.mkfifo "./demo.fifo" 0o600)
      (fun a -> Lwt.return a)
      (function Unix.(Unix_error (EEXIST, _, _)) -> Lwt.return_unit
              | _ -> failwith "unexpected error")
  end >>= fun () ->
  Lwt_unix.openfile "./demo.fifo" Lwt_unix.[O_RDWR] 0o000 >>= fun fd ->
  let recvbuf = Bytes.create (32+5) in (* see {handle_remote_input} *)
  let rec reader fd =
    Lwt_io.read_into_exactly fd recvbuf 0 (Bytes.length recvbuf) >>= fun () ->
    Lwt_mvar.put read_mvar (Bytes.to_string recvbuf) >>= fun () ->
    reader fd
  in
  let rec writer fd =
    Lwt_mvar.take write_mvar >>= fun (x:edit) ->
    let sendbuf = Printf.sprintf "%ld,%ld,%Ld,%Ld,%Ld."
        x.author (Uchar.to_int x.element |> Int32.of_int)
        (C.Marker.to_int64 x.marker)
        (C.Marker.to_int64 x.before)
        (C.Marker.to_int64 x.after)
                  |> Bytes.of_string
    in
    Lwt_io.write_from_exactly fd sendbuf 0 (Bytes.length sendbuf) >>= fun () ->
    writer fd
  in
  Lwt.choose [
    reader Lwt_io.(of_unix_fd ~mode:Input @@ Lwt_unix.unix_file_descr fd) ;
    writer Lwt_io.(of_unix_fd ~mode:Output @@ Lwt_unix.unix_file_descr fd)
  ]

(* Lwt_io.read_value *)

let pvec_line_wrap
    ?(offset=0) ~height ~width (orig_vec:string Pvec.t) =
  (* need two predicates:
     weight : elt -> int
     is_break : elt -> bool
     weight is the amount of printable characters
     invisible determines if the elt will end up in the output
  *)
  (* let handle_lines ?take ~drop vec =
    let rec scan ?(f_line:(int -> int -> elt Pvec.t -> unit)option)
        ~start = function
      | 0 -> Some start
      | n -> begin match Pvec.left_find ~start is_newline vec with
          | None ->
            None (* need to drop more, but there are no more lines *)
          | Some (offset, _elt) ->
            if offset - start > width then begin
              (* line too long, need to line wrap *)
              let lines_spanned = (offset - start) / width in
              if lines_spanned < n then begin
                (* need to process all of these lines, so skip ahead *)
                let () = match f_line with
                  | None -> () | Some f ->
                    for idx = 0 to lines_spanned do
                      f (start + idx*width) (start + (idx+1)*width) orig_vec
                    done
                in
                scan ?f_line ~start:(start + lines_spanned * width)
                  (n - lines_spanned)
              end else begin
                (* this is the last, so we only need to process some of these *)
                let () = match f_line with
                  | None -> () | Some f ->
                    f start (offset + n * width) orig_vec
                in
                scan ?f_line ~start:(offset + n * width) 0
              end
            end else (* normal-size line: *)
              let () =
                match f_line with
                | None -> () | Some f ->
                  f start offset orig_vec
              in
              scan ?f_line ~start:(succ offset) (pred n)
        end
    in
    match scan ~start:0 drop with
    | None -> Pvec.empty
    | Some start ->
      begin match take with
        | None -> Pvec.drop_left start orig_vec
        | Some take_lines ->
          begin match scan ?f_line ~start take_lines with
            | None -> Pvec.drop_left start orig_vec
            | Some last ->
              Pvec.range ~first:start ~last orig_vec
          end
      end
  in *)
  Pvec.fold_left
    (fun acc full_msg ->
       let strs = String.split_on_char '\n' full_msg in
       let str_vec = Pvec.of_list strs in
       Pvec.fold_left
         (fun acc el ->
            if String.length el < width then
              Pvec.add_last acc el
            else
              let rec chunker acc tl =
                let chunk_len = String.length tl in
                let take_len = min width chunk_len in
                let acc = Pvec.add_last acc (String.sub tl 0 take_len) in
                if chunk_len > width then
                  chunker acc (String.sub tl take_len (chunk_len-take_len))
                else
                  acc
              in chunker acc el
         ) acc str_vec
    )
    Pvec.empty orig_vec
  |> Pvec.drop_left offset
  |> Pvec.take_left height
  |> fun chunked_vec ->
  (*let chunked_vec = handle_lines ?f_line ~drop:offset ~take:height orig_vec in*)
  (* ^-- notty doesn't like \n *)
  chunked_vec

module Linevec = struct
  module IntSet = Set.Make(struct type t = int let compare = compare end)
  type 'a t = {
    elements: ('a * Uchar.t) Pvec.t ;
    hardbreak_vec: int Pvec.t ;
    hardbreaks: IntSet.t ; (* only changes when elements are changed *)
    softbreaks: IntSet.t ; (* changes per view, when width is changed,
                              or when elements are changed *)
  }
  let empty = { elements = Pvec.empty ; hardbreak_vec = Pvec.empty;
                hardbreaks = IntSet.empty ; softbreaks = IntSet.empty }
  let of_uchar_vec ?(softbreak_width=80) uchvec =
    Pvec.foldi_left
      (fun (softbreak_counter,t) idx ((_, uch) as elt) ->
         (* TODO substitute unhandled chars or implement the unicode standard
            for linewrapping:
            http://unicode.org/reports/tr14/  *)
         let hardbreak_vec, hardbreaks, (softbreaks, last_softbreak) =
           if uch <> Uchar.of_char '\n' then
             t.hardbreak_vec, t.hardbreaks,
             begin if idx < softbreak_width then
                 t.softbreaks, succ softbreak_counter
               else
                 IntSet.add idx t.softbreaks, 0
             end
           else
             Pvec.add_last t.hardbreak_vec idx , IntSet.add idx t.hardbreaks,
             (t.softbreaks, 0)
         in
         last_softbreak, { elements = Pvec.add_last t.elements elt ;
                           hardbreak_vec ; hardbreaks ; softbreaks }
      ) (0, empty) uchvec

  let drop_horizontal_predicate ~f offset orig_t =
    if offset = 0 then orig_t
    else
      Pvec.foldi_left
        (fun (line_offset, t) orig_idx elt ->
           let elt_offset = Pvec.length t.elements in
           if IntSet.mem orig_idx orig_t.hardbreaks then begin
             0, { elements = Pvec.add_last t.elements elt ;
                  hardbreaks = IntSet.add elt_offset t.hardbreaks;
                  hardbreak_vec = Pvec.add_last t.hardbreak_vec elt_offset ;
                  softbreaks = orig_t.softbreaks
                }
           end else
             (* it is printable: *)
             if f line_offset offset then (* need to skip this: *)
               succ line_offset, t
             else (* need to insert this: *)
               succ line_offset, { elements = Pvec.add_last t.elements elt ;
                                   hardbreaks = t.hardbreaks ;
                                   hardbreak_vec = t.hardbreak_vec ;
                                   softbreaks = t.softbreaks;
                                 }
        ) (0, empty) orig_t.elements |> snd

  let drop_horizontal_left =
    drop_horizontal_predicate ~f:(<)

  let drop_horizontal_after offset t =
    if offset = 0 then
      let len = IntSet.cardinal t.hardbreaks in
      { elements = Pvec.filter(fun (_,uch) ->Uchar.of_char '\n'=uch) t.elements;
        hardbreaks = IntSet.of_list (List.init len (fun i -> i)) ;
        hardbreak_vec = Pvec.init ~len (fun i -> i) ;
        softbreaks = t.softbreaks;
      }
    else
      drop_horizontal_predicate ~f:(>=) offset t

  let take_hardbreaks ?(offset=0) n (t:'a t) =
    if n = 0 || IntSet.cardinal t.hardbreaks < offset then empty
    else
    let return_nothing, first =
      if offset = 0 then false, 0 else
        match Pvec.get t.hardbreak_vec (pred offset) with
        | exception Invalid_argument _ when Pvec.length t.hardbreak_vec = 0 ->
          true, 0
        | exception (Not_found | Invalid_argument _)-> false, 0
        | idx -> false, succ idx in
    if return_nothing then empty
    else
    let last = match Pvec.get t.hardbreak_vec (offset+pred n) with
      | exception (Not_found | Invalid_argument _) ->
        max (pred @@ Pvec.length t.elements) 0
      | idx -> idx in
    { elements = Pvec.range ~first ~last t.elements ;
      hardbreak_vec = Pvec.filter
          (fun break -> first <= break && break <= last) t.hardbreak_vec
                      |> Pvec.map (fun old_offset -> old_offset - first) ;
      hardbreaks = begin match IntSet.split first t.hardbreaks with
        | (_ , true , right) -> IntSet.add first right
        | (_, false, right) -> right
      end |> IntSet.split last |> begin function
          | (in_range, false, _) -> in_range
          | (in_range, true, _) -> IntSet.add last in_range
        end |> IntSet.map (fun old_offset -> max 0 @@ old_offset - first) ;
      softbreaks = IntSet.map (fun old -> old - first) t.softbreaks
                   |> IntSet.filter (fun i -> i >= 0);
    }

  let softbreak_width ~initial ~width vec =
    if Pvec.length vec < width then
      IntSet.empty
    else
      let rec loop acc = function
        | 0 -> acc
        | n -> loop (IntSet.add (initial+n) acc) (n-width)
      in loop IntSet.empty (Pvec.length vec)

  let linewrap ~f ~width ~height ~line_offset (t:'a t) =
    let rec loop (x,y) elt img =
      let img = I.uchar (f (fst elt)) x y (snd elt) img in
      loop (x,y) elt img
    in
    loop (0,0) Notty.I.empty ;
    let x = take_hardbreaks ~offset:line_offset height in
    ()

end

let render term =
  let width, height = Term.size term in
  let vec = C.Snapshot.(of_t !document |> to_vector) in
  let wrapped =
    let buf = Buffer.create (Pvec.length vec) in
    Pvec.fold_left (fun () (_, uchar) ->
        (* ^-- TODO here we throw away author info*)
        (* TODO ideally our line wrapping function would work on unicode
           graphemes, or at the very least on unicode codepoints.*)
        Uutf.Buffer.add_utf_8 buf uchar) () vec ;
    Buffer.contents buf |> Pvec.singleton |>
    pvec_line_wrap ~width ~height |>
    Pvec.fold_left (fun img str ->
        Uutf.String.fold_utf_8 (fun acc _idx -> function
            | `Malformed _ -> failwith "malformed"
            | `Uchar c -> Pvec.add_last acc c) Pvec.empty str
        |> Pvec.to_array |> fun x -> I.uchars A.(fg red) x
                                     |> fun i -> I.(img <-> i)
      ) Notty.I.empty
  in
  let img = I.(wrapped
               <-> strf "cursor: %a" Fmt.(pair ~sep:(unit",") int int) !cursor
              ) in
  let dbg_img =
    let offset = snd !cursor in
    pvec_line_wrap ~offset:0 ~height:(height-6) ~width !debug
    |> Pvec.fold_left (fun acc msg ->
        I.(acc <-> strf "%s" msg)
      ) (I.strf "Debug:")
  in
  Term.image term I.(img <-> dbg_img)
(*  ;
    let vec2 =
    let buf = Buffer.create (Pvec.length vec) in
    Pvec.fold_left (fun () uchar -> Uutf.Buffer.add_utf_8 buf uchar) () vec;
    Buffer.contents buf |> Pvec.singleton in
  let wrapped = pvec_line_wrap ~width ~height:2 vec in

    Pvec.fold_left (fun acc msg ->
      I.(acc <-> strf "%s" msg)
    ) (I.strf "Debug:")
    (chunked_vec)*)


let timer () = Lwt_unix.sleep 0.5 >|= fun () -> `Timer

let event term = Lwt_stream.get (Term.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let rec loop term (e, t) =
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) ->
      Lwt.return_unit
  | `Timer ->
      render term >>= fun () ->
      loop term (e, timer ())
  | `Mouse ((`Press _|`Drag), (x, y), _) ->
    cursor := (x,y) ;
    render term >>= fun () ->
    loop term (event term, t)
  | `Resize (x,y) ->
    cursor := min x (fst !cursor) , min y (snd !cursor) ;
    render term >>= fun () ->
    loop term (event term, t)
  | `Key (`Home, _) ->
    move_cursor term `Home ;
    loop term (event term, t)
  | `Key (`Delete, _) ->
    erase_at_cursor (fst !cursor) ;
    loop term (event term, t)
  | `Key (`Backspace, _) ->
    erase_before_cursor term (fst !cursor) ;
    loop term (event term, t)

  | `Key (`Arrow (`Right|`Left|`Down|`Up as direction), _) ->
    move_cursor term direction ;
    loop term (event term, t)
  | `Key (`ASCII ch, mods) when List.mem `Ctrl mods ->
    begin match ch with
      | 'U' -> delete_range 0 (pred @@ fst !cursor) ;
        cursor := (0, snd !cursor)
      | 'K' ->
        delete_range (fst !cursor) (fst (Term.size term))
      | 'A' ->
        move_cursor term `Home
      | 'E' ->
        cursor := (fst (Term.size term), snd !cursor)
      | _ -> ()
    end ;
    render term >>= fun () ->
    loop term (event term, t)
  | `Key (`ASCII ch, _) ->
    insert_uchars @@ Pvec.singleton (author, Uchar.of_char ch) ;
    move_cursor term `Right ;
    render term >>= fun () ->
    loop term (event term, t)
  | `Key (`Uchar ch, _) ->
    insert_uchars @@ Pvec.singleton (author, ch) ;
    move_cursor term `Right ;
    render term >>= fun () ->
    loop term (event term, t)
  | `Key (`Enter, _) ->
    insert_uchars @@ Pvec.singleton (author, Uchar.of_char '\n') ;
    cursor := (0, succ @@ snd !cursor) ;
    Notty_unix.show_cursor ~cap:Notty.Cap.ansi ~fd:stdout true ;
    Notty_unix.show_cursor ~cap:Notty.Cap.ansi ~fd:stderr true ;

    render term >>= fun () ->
    loop term (event term, t)
  | `Key (_ as _key,_) ->
    assert(_key <> `Enter);
    debug := Pvec.add_first "unhandled `Key" !debug ;
    loop term (event term, t)
  | `Paste _ ->
    debug := Pvec.add_first "unhandled `Paste" !debug ;
    loop term (event term, t)

  | _ -> loop term (event term, t)

let interface () =
  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false });
  let term    = Term.create () in
  Notty_unix.show_cursor ~cap:Notty.Cap.ansi ~fd:stdout true ;
  Notty_unix.show_cursor ~cap:Notty.Cap.ansi ~fd:stderr true ;
  loop term (event term, timer ())

let handle_remote_input mvar =
  let rec loop () : unit Lwt.t =
    Lwt_mvar.take mvar >>= fun input ->
    Scanf.sscanf input "%ld,%ld,%Ld,%Ld,%Ld." (* 4+4 +8+8+8 = 32 *)
      (* echo -n '33,0098,00001239,000000000,268435456.' >> demo.fifo *)
      (fun author uch marker incoming_edge outgoing_edge ->
         let uch = Uchar.of_int (Int32.to_int uch) in
         let before = C.Marker.of_int64 outgoing_edge
         and after  = C.Marker.of_int64 incoming_edge
         and marker = C.Marker.of_int64 marker in
         merge_edge ~author ~before ~after marker uch ;
         debug := Pvec.add_first "fuck" !debug
      ) ;
    loop ()
  in loop ()

let main () =
  let read_mvar = Lwt_mvar.create_empty () in
  Lwt.choose [
    initialize_socket ~read_mvar ~write_mvar ;
    increase_counter ();
    handle_remote_input read_mvar ;
    interface () ;
  ]

let () = Lwt_main.run (main ())
