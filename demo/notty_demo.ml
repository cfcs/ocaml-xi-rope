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
let write_mvar : C.diff Lwt_mvar.t = Lwt_mvar.create_empty ()

let merge_edge ~author ~after ~before marker element =
  document := C.insert_element !document ~author ~after ~before marker element ;
  debug := Pvec.add_first ("got something from peer") !debug

let strip_author = Pvec.map (fun (_a,e) -> e)

let insert_uchars ?(author=author) ?(first=fst !cursor) chars =
  (* resolve cursor to position in Linevec
     to see if cursor is at the end of a lin, according to some offset?
  *)
  let _ = author (*TODO allow overriding author*) in
  let old_live = C.Snapshot.(of_t !document |> live_elements) in
  let first_e =
    if Pvec.length old_live <= first
    then (try fst @@ Pvec.get_last old_live with _ -> C.Marker.beginning)
    else (try fst@@Pvec.get old_live (pred first) with _ -> C.Marker.beginning)
  in
  let last_e =
    if Pvec.length old_live <= first then C.Marker.ending else
      fst @@ Pvec.get old_live first in
  assert (first_e <> C.Marker.ending);
  assert (last_e <> C.Marker.beginning);
  let newdoc, _, _ = Pvec.fold_left (fun (doc,after,before) (author,uchar) ->
      let this = C.Marker.generate () in
      debug := Pvec.add_last !debug
          (Fmt.strf "this:%a after:%a before:%a"
             C.Marker.pp this
             C.Marker.pp after
             C.Marker.pp before) ;
      let doc =
        C.insert_element doc ~author ~after ~before this uchar in
      doc, this, before
      (* room for optimization: generate id of next and use that as "before" *)
    ) (!document, first_e, last_e) chars in
  let old_document = !document in
  let diff = C.diff !document newdoc in
  document := newdoc ;
  let vec = C.Snapshot.(of_t !document |> to_vector) in
  let () = Lwt.async (fun () -> Lwt_mvar.put write_mvar diff) in
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
  let diff = C.diff old_document !document in
  let () = Lwt.async (fun () -> Lwt_mvar.put write_mvar diff) in
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
  let rec writer fd =
    Lwt_mvar.take write_mvar >>= fun (x:C.diff) ->
    let open Sexplib.Conv in
    let sexp_of_marker m = C.Marker.to_int64 m |> sexp_of_int64 in
    let sexp_of_markerset sexp_of_elt m =
      C.Markers.to_seq m |> List.of_seq
      |> sexp_of_list (sexp_of_pair sexp_of_marker sexp_of_elt)
    in
    let sexp_of_element = let open Sexplib.Sexp in function
      | C.Live e -> List [Atom "+"; sexp_of_int (Uchar.to_int e)]
      | C.Tombstone e -> List [Atom "-";sexp_of_int (Uchar.to_int e)]
    in
    let sexp_of_edges e =
      C.Edges.to_seq e |> List.of_seq
      |> sexp_of_list (fun (edge:C.edge) ->
          sexp_of_list sexp_of_marker [edge.left ; edge.right])
    in
    let elements = sexp_of_markerset sexp_of_element x.C.elements in
    let edges = sexp_of_edges x.C.edges in
    let edits =
      let e = x.C.edits in
      C.Edits.to_seq e |> List.of_seq
      |> sexp_of_list (fun (x:C.edit) ->
          sexp_of_list (fun a -> a) [
            sexp_of_markerset sexp_of_element x.deleted ;
            sexp_of_markerset sexp_of_element x.inserted ;
            sexp_of_edges x.edges ;
            sexp_of_int32 x.author
          ]
        )
    in
    let authors = sexp_of_markerset sexp_of_int32 x.C.authors in
    let sendbuf =
      let open Sexplib.Sexp in
      (to_string @@
      List [
        Atom "diff" ;
        List [Atom "elements"; elements];
        List [Atom "edges"; edges];
        List [Atom "edits"; edits];
        List [Atom "authors"; authors]
      ]) ^ "\n" |> Bytes.of_string
    in
    Lwt_io.write_from_exactly fd sendbuf 0 (Bytes.length sendbuf) >>= fun () ->
    writer fd
  in
  let rec reader fd =
    Lwt_io.read_line fd >>= fun line ->
    Lwt_mvar.put read_mvar line >>= fun () ->
    reader fd
  in
  let sockaddr = Lwt_unix.ADDR_INET (
      Unix.inet_addr_of_string "127.0.0.1",7777) in
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let be_server () : unit Lwt.t =
    (* when the server receives, it should forward I guess.
       Or example should use multicast? *)
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true ;
    Lwt_unix.bind socket sockaddr >>= fun () ->
    Lwt_unix.listen socket 1 ;
    let rec serve () =
      Lwt_unix.accept socket >>= fun (fd, _addr) ->
      Lwt.async (fun () ->
          Lwt.choose [
        reader Lwt_io.(of_unix_fd ~mode:Input @@ Lwt_unix.unix_file_descr fd) ;
        writer Lwt_io.(of_unix_fd ~mode:Output @@ Lwt_unix.unix_file_descr fd)
      ]) ; serve ()
    in serve ()
  in
  Lwt.try_bind be_server (fun () -> Lwt.return_unit)
    (fun _ ->
       Lwt_unix.connect socket sockaddr >>= fun () ->
       let fd = Lwt_unix.unix_file_descr socket in
       Lwt.choose [
         reader Lwt_io.(of_unix_fd ~mode:Input fd) ;
         writer Lwt_io.(of_unix_fd ~mode:Output fd)
       ]
    )

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
  |> Pvec.rev
  |> fun chunked_vec ->
  (*let chunked_vec = handle_lines ?f_line ~drop:offset ~take:height orig_vec in*)
  (* ^-- notty doesn't like \n *)
  chunked_vec

module Linevec = struct
  module IntSet = struct
    include Set.Make(struct type t = int let compare = compare end)
    let pp ppf v =
      Fmt.pf ppf "%d: {@[" (cardinal v) ;
      fold (fun elt () -> Fmt.pf ppf "%d," elt) v () ;
      Fmt.pf ppf "@]}"
  end
  type 'a t = {
    elements: ('a * Uchar.t) Pvec.t ;
    hardbreak_vec: int Pvec.t ;
    hardbreaks: IntSet.t ; (* only changes when elements are changed *)
    softbreaks: IntSet.t ; (* changes per view, when width is changed,
                              or when elements are changed *)
  }
  let empty = { elements = Pvec.empty ; hardbreak_vec = Pvec.empty;
                hardbreaks = IntSet.empty ; softbreaks = IntSet.empty }

  (*let chars, t = Notty_demo.Linevec.of_uchar_vec ~softbreak_width:5
    (Pvec.map (fun a -> 2, Uchar.of_char a)
    @@ Pvec.of_string "1\n2\n33\n444\n5\n6\n7\n8\n9\na\nxyzabc\ndef")
    in chars, Fmt.pf Fmt.stdout "%a" Notty_demo.Linevec.pp t,
    (Notty_demo.Linevec.fold_breaks ~max:1000 t
    ~f:(fun ac b -> Pvec.map (fun (_,ch) ->Uchar.to_char ch) ac
    |> Pvec.to_list |> fun xyz -> xyz::b) []) *)

  let pp ppf t =
    let uchar_pp ppf v = Fmt.pf ppf "%C" (Uchar.to_char v) in
    let sep = Fmt.unit ", " in
    Fmt.pf ppf "@[<v>{@[<v>elements: %a@,\
                hardbreak_vec: %a@,\
                hardbreaks: %a@,\
                softbreaks: %a@,@]}@]\n%!"
      (Pvec.pp ~sep Fmt.(pair ~sep:(unit":") int uchar_pp)) t.elements
      (Pvec.pp ~sep Fmt.int) t.hardbreak_vec
      IntSet.pp t.hardbreaks
      IntSet.pp t.softbreaks

  let of_uchar_vec ?(softbreak_width=80) uchvec =
    Pvec.foldi_left
      (fun (softbreak_counter,t) idx ((_, uch) as elt) ->
         (* TODO substitute unhandled chars or implement the unicode standard
            for linewrapping:
            http://unicode.org/reports/tr14/  *)
         let hardbreak_vec, hardbreaks, (softbreaks, last_softbreak) =
           if uch <> Uchar.of_char '\n' then
             t.hardbreak_vec, t.hardbreaks,
             begin if softbreak_counter < softbreak_width then
                 t.softbreaks, succ softbreak_counter
               else
                 IntSet.add (pred idx) t.softbreaks, 1 (* <- softbreak_counter*)
             end
           else
             Pvec.add_last t.hardbreak_vec idx , IntSet.add idx t.hardbreaks,
             (t.softbreaks,
              0 (* <- softbreak_counter *)
             )
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

  let drop_horizontal_left i t =
    drop_horizontal_predicate ~f:(<) i t

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

  let fold_breaks ?(offset=0) ~max (t:_ t) ~f acc =
    let breaks = IntSet.union t.softbreaks t.hardbreaks in
    IntSet.fold (fun elt (idx,last_break,acc) ->
        let acc =
          if idx < offset || max < idx then
            acc
          else begin
            let last = if IntSet.mem elt t.hardbreaks then elt-1 else elt
            and first = if IntSet.mem last_break t.hardbreaks then
                last_break +1 else last_break+1 in
            let line =
              if last < first
              then Pvec.empty
              else Pvec.range ~first ~last t.elements
            in f line acc
          end
        in
        (idx + 1, elt, acc)
        ) breaks (0, -1, acc)
    |> function
    | 0, -1, acc -> f t.elements acc
    | _idx, n, acc when n < max ->
      let leftover = Pvec.range
          ~first:(n+1)
          ~last:(Pvec.length t.elements-1) t.elements in
      f leftover acc
    | (_idx,_last_break,acc) ->
      acc

  (*
  let splice_vec ~x ~y t =
    (* find relevant hardbreak *)
    let hardbreak =
      if Pvec.is_empty t.hardbreak_vec then
        0
      else
      if Pvec.length t.hardbreak_vec <= y then
        Pvec.get_last t.hardbreak_vec
      else
        Pvec.get t.hardbreak_vec y
    in
    let _softbreak = () in ()
*)

  let linewrap ~f ~width ~height ~line_offset (_t:_ t) =
    let _TODO = f, width in
    (*let rec loop (x,y) elt img =
      let img = I.uchar (f (fst elt)) (snd elt) x y img in
      loop (x,y) elt img
    in
    loop (0,0) Notty.I.empty ;
    let x = take_hardbreaks ~offset:line_offset height in
    *)
    let _win = take_hardbreaks ~offset:line_offset height in
    ()

end

let render term =
  let width, height = Term.size term in
  let vec = C.Snapshot.(of_t !document |> to_vector) in
  (* TODO ideally our line wrapping function would work on unicode
           graphemes, or at the very least on unicode codepoints.*)
  let wrapped =
    let by_author vec =
      Pvec.fold_left (fun i (author, b) ->
          let colors = [| A.red ; A.blue ; A.green|] in
          let c = A.fg colors.((Int32.to_int author mod 3))in
          Notty.I.(i <|> uchars c [|b|])
        )I.empty vec
    in
    let _, linevec = Linevec.of_uchar_vec ~softbreak_width:20 vec in
    Linevec.fold_breaks ~offset:0 ~max:10 linevec
      ~f:(fun line acc ->
          Notty.I.(by_author line
          (*Notty.(I.(
              uchars A.(fg red) (Pvec.map (fun (_author_id,b) -> b) line
                                 |> Pvec.to_array)*)
          <-> acc)
        ) Notty.I.empty
  in
  let img = I.(wrapped
               <-> strf "cursor: %a" Fmt.(pair ~sep:(unit",") int int) !cursor
              ) in
  let dbg_img =
    let offset = snd !cursor in
    pvec_line_wrap ~offset ~height:(height-6) ~width !debug
    |> Pvec.fold_left (fun acc msg ->
        I.(acc <-> strf "%s" msg)
      ) (I.strf "Debug:")
  in
  Term.image term I.(img <-> strf "---" <-> dbg_img) >>= fun () ->
  Notty_lwt.Term.cursor term (Some (!cursor))
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
    Notty_lwt.Term.cursor term (Some (!cursor)) >>= fun () ->
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

let diff_of_sexp str =
  let open Sexplib in
  let open Rresult in
  begin match Sexp.of_string str with
    | List [Atom "diff" ;
            List [Atom "elements"; elements] ;
            List [Atom "edges"; edges] ;
            List [Atom "edits"; edits] ;
            List [Atom "authors"; authors]
           ] -> Ok (elements, edges, edits, authors)
    | _ -> Error "not a diff"
  end >>= fun (elements, edges, edits, authors) ->
  let marker_of_sexp m = Conv.int64_of_sexp m |> C.Marker.of_int64 in
  let markerset_of_sexp elt_of_sexp ms =
    Conv.list_of_sexp (Conv.pair_of_sexp marker_of_sexp elt_of_sexp) ms
    |> List.to_seq |> C.Markers.of_seq
  in
  let edge_of_sexp e =
    let left, right = Conv.pair_of_sexp marker_of_sexp marker_of_sexp e in
    {C.left; right} in
  let edges_of_sexp e =
    Conv.list_of_sexp edge_of_sexp e |> List.to_seq |> C.Edges.of_seq
  in
  let element_of_sexp = let open Sexp in let open Conv in function
    | List [Atom "+"; i] -> C.Live (Uchar.of_int @@ int_of_sexp i)
    | List [Atom "-"; i] -> C.Tombstone (Uchar.of_int @@ int_of_sexp i)
    | _ -> failwith "error"
  in
  let edit_of_sexp = let open Sexp in function
      | List [deleted; inserted ; edges; author] ->
        { C.undo_group_id = () ;
          deleted = markerset_of_sexp element_of_sexp deleted ;
          inserted = markerset_of_sexp element_of_sexp inserted ;
          edges = edges_of_sexp edges ;
          author = Conv.int32_of_sexp author ; }
      | _ -> failwith "error2"
  in
  let elements = markerset_of_sexp element_of_sexp elements in
  let edges = edges_of_sexp edges in
  let edits = Conv.list_of_sexp edit_of_sexp edits
              |> List.to_seq |> C.Edits.of_seq in
  let authors = markerset_of_sexp Conv.int32_of_sexp authors in
  Ok { C.elements ; edges; edits; authors }

let handle_remote_input mvar =
  let rec loop () : unit Lwt.t =
    Lwt_mvar.take mvar >>= fun input ->
    begin try
        let peer_diff : C.diff = diff_of_sexp input |> Rresult.R.get_ok in
        document := C.merge_diff !document peer_diff ;
      with
      | Failure err -> debug := Pvec.add_first err !debug ;
    end ;
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
