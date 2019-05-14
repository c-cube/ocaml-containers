(* search for first occurrence of pat in s *)
let rec search pat s pos =
  let rec compare i =
    if i >= String.length pat
    then true
    else if pat.[i] = s.[pos+i]
    then compare (i+1)
    else false
  in
  if pos > String.length s - String.length pat
  then raise Not_found
  else if compare 0
  then pos
  else search pat s (pos+1)
;;

(* search all non-overlapping occurrences of pat in s *)
let search_all pat s =
  let rec search_rest acc pos =
    let next =
      try Some (search pat s pos) with
          Not_found -> None
    in
    match next with
      | None -> acc
      | Some pos -> search_rest (pos::acc) (pos + String.length pat)
  in
  List.rev (search_rest [] 0)
;;

(* replace first occurrence of pat with subst in s *)
let replace_first pat subst s =
  let pos = search pat s 0 in
  let patl = String.length pat
  and substl = String.length subst in
  let buf = Bytes.create (String.length s - patl + substl) in
  Bytes.blit_string s 0 buf 0 pos;
  Bytes.blit_string subst 0 buf pos substl;
  Bytes.blit_string
    s   (pos + patl)
    buf (pos + substl)
    (String.length s - pos - patl);
  Bytes.unsafe_to_string buf
;;

(* replace all occurrences of pat with subst in s *)
let replace_all pat subst s =
  let pos = search_all pat s in
  let patl = String.length pat
  and substl = String.length subst in
  let len = String.length s + List.length pos * (substl - patl) in
  let buf = Bytes.create len in
  let rec loop src_pos dst_pos = function
    | [] ->
      Bytes.blit_string s src_pos buf dst_pos (String.length s - src_pos)
    | pat_pos :: tail ->
      let headl = pat_pos - src_pos in
      Bytes.blit_string s src_pos buf dst_pos headl;
      Bytes.blit_string subst 0 buf (dst_pos + headl) substl;
      loop
        (src_pos + headl + patl)
        (dst_pos + headl + substl)
        tail
  in loop 0 0 pos;
  Bytes.unsafe_to_string buf
;;

let match_closeparen s i =
  assert (s.[i] = ')');
  let rec loop i count =
    match s.[i] with
      | '(' when count = 0 -> i
      | '(' -> loop (i-1) (count-1)
      | ')' -> loop (i-1) (count+1)
      | _ -> loop (i-1) count
  in loop (i-1) 0
;;

let slurp_file file =
  let ch = open_in file in
  let buf = Buffer.create (min 1024 (in_channel_length ch)) in
  try
    while true do Buffer.add_channel buf ch 4096 done;
    assert false
  with
    | End_of_file ->
      close_in ch;
      Bytes.unsafe_to_string (Buffer.to_bytes buf)
;;

let () =
  assert (Array.length Sys.argv = 3);
  let   labelled_filename = Sys.argv.(1) in (* CCArrayLabels.mli *)
  let unlabelled_filename = Sys.argv.(2) in (* CCArray.ml *)
  let labelled_name = (* ArrayLabels *)
    assert (labelled_filename.[0] = 'C' && labelled_filename.[1] = 'C');
    let dot = String.rindex labelled_filename '.' in
    String.sub labelled_filename 2 (dot - 2)
  in
  let unlabelled_name = (* Array *)
    replace_first "Labels" "" labelled_name
  in
  let labelled_text = slurp_file labelled_filename in
  let lexbuf = Lexing.from_string labelled_text in
  Location.init lexbuf labelled_filename;
  let labelled_ast = Parse.interface lexbuf in
  (* stack of replacements to perform on the labelled_text.
   * perform them in one run later so that the character counts
   * won't be affected by earlier replacements. *)
  let replacements = ref [] in
  (* function removing '~' from docstring attributes where appropriate. *)
  let strip_attributes labels attributes =
    List.iter
      begin function
        | ({ Asttypes.txt = "ocaml.doc"; _ },
           Parsetree.PStr [{pstr_loc =
                              { loc_start = {pos_cnum = start; _}
                              ; loc_end = {pos_cnum = stop; _}
                              ; _}
                           ; _
                           }]) ->
          let docstring =
            List.fold_left
              (fun docstring label ->
                 replace_all ("~" ^ label) label docstring)
              (String.sub labelled_text start (stop-start))
              labels
          in
          replacements := (start, stop-start, docstring) :: !replacements
        | _ -> ()
      end
      attributes
  in
  let iterator =
    let open Ast_iterator in
    let open Parsetree in
    { Ast_iterator.default_iterator with
      value_description = begin fun iterator
        { pval_name = { txt = _name; _ }
        ; pval_type
        ; pval_prim = _
        ; pval_attributes
        ; pval_loc
        } ->
        let rec loop = function
          (* match function type with label *)
          | { ptyp_desc = Ptyp_arrow (Labelled label, left, right)
            ; ptyp_loc = {loc_start = {Lexing.pos_cnum = start; _}; _}
            ; ptyp_attributes
            ; _}
            when
              (* check that the argument type is not marked with [@keep_label] *)
              List.for_all
                (fun ({Asttypes.txt; _}, _) -> txt <> "keep_label")
                left.ptyp_attributes
            ->
            assert (label = String.sub labelled_text start (String.length label));
            let colon = String.index_from labelled_text start ':' in
            (* remove label *)
            replacements := (start, colon+1-start, "") :: !replacements;
            (* remove labels from associated docstrings *)
            strip_attributes [label] ptyp_attributes;
            label :: loop right
          | { ptyp_desc = Ptyp_arrow (_, _left, right); _} ->
            loop right
          | _ -> []
        in
        let labels = loop pval_type in
        strip_attributes labels pval_attributes;
        iterator.attributes iterator pval_attributes;
        iterator.location iterator pval_loc;
        iterator.typ iterator pval_type;
      end
    ; attribute = begin fun iterator
        ({Asttypes.txt
         ; loc =
             { loc_start = {pos_cnum = start; _}
             ; loc_end = {pos_cnum = stop; _}
             ; _} as loc
         }, _) ->
        if txt = "keep_label"
        then begin
          (* start and stop positions mark the location of only the label name.
           * Therefore search for enclosing brackets. *)
          let start = String.rindex_from labelled_text start '['
          and stop  = String. index_from labelled_text stop  ']' in
          (* remove leading ' ', too *)
          let start =
            if labelled_text.[start-1] = ' ' then start-1 else start
          in
          (* if a closing paren follows, remove this and the matching paren,
           * this will hopefully be the right thing to do. *)
          let stop =
            if labelled_text.[stop+1] = ')'
            then
              let openp = match_closeparen labelled_text (stop+1) in
              replacements := (openp, 1, "") :: !replacements;
              stop+1
            else
              stop
          in
          replacements := (start, stop-start+1, "") :: !replacements;
        end;
        iterator.location iterator loc
      end
    }
  in
  iterator.signature iterator labelled_ast;

  (* sort replacements in ascending order. *)
  let replacements =
    List.sort (fun (p1,_,_) (p2,_,_) -> compare p1 p2) !replacements
  in

  (* perform the replacements by blitting to a buffer. *)
  let unlabelled_text = Buffer.create (String.length labelled_text) in
  List.fold_left begin fun start (pos,len,subst) ->
    assert (pos >= start);
    Buffer.add_substring unlabelled_text labelled_text start (pos - start);
    Buffer.add_string unlabelled_text subst;
    pos+len
  end
    0
    replacements
  |> fun start ->
  Buffer.add_substring unlabelled_text
    labelled_text start (String.length labelled_text - start);

  let unlabelled_text =
    Buffer.contents unlabelled_text
    (* ArrayLabels -> Array *)
    |> replace_all labelled_name unlabelled_name
  in

  let out = open_out unlabelled_filename in
  output_string out (
    "(*  AUTOGENERATED FROM " ^
    labelled_filename
    ^ "  *)\n\n");
  output_string out unlabelled_text;
  close_out out;
;;
