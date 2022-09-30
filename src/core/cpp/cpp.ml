module C = Configurator.V1

type conf = { os_type: string; major: int; minor: int }
type comp = Le | Ge
type condition = Version of comp * int * int | Os_type of string

type line =
  | If of condition
  | Elseif of condition
  | Else
  | Endif
  | Raw of string
  | Eol
  | Eof

let prefix ~pre s =
  let len = String.length pre in
  if len > String.length s then
    false
  else (
    let rec check i =
      if i = len then
        true
      else if String.unsafe_get s i <> String.unsafe_get pre i then
        false
      else
        check (i + 1)
    in
    check 0
  )

let get_tag_from_opt s pos =
  let rec get_start pos =
    let p = String.index_from s pos '[' in
    if p > String.length s - 5 then
      raise_notrace Not_found
    else if s.[p + 1] = '@' && s.[p + 2] = '@' && s.[p + 3] = '@' then
      p
    else
      get_start (p + 1)
  in
  try
    let start = get_start pos in
    Some (get_start pos, String.index_from s (start + 4) ']')
  with Not_found -> None

let split_trim s c =
  try
    let p = String.index s c in
    ( String.trim (String.sub s 0 p),
      String.trim (String.sub s (p + 1) (String.length s - p - 1)) )
  with Not_found -> s, ""

let eval ~conf = function
  | Os_type ty -> conf.os_type = ty
  | Version (op, i, j) ->
    (match op with
    | Le -> (conf.major, conf.minor) <= (i, j)
    | Ge -> (conf.major, conf.minor) >= (i, j))

let preproc_lines ~file ~conf (ic : in_channel) : unit =
  let pos = ref 0 in
  let fail msg =
    failwith (Printf.sprintf "at line %d in '%s': %s" !pos file msg)
  in
  let pp_pos () = Printf.printf "#%d %S\n" !pos file in

  let parse_condition condition =
    flush_all ();
    match split_trim condition ' ' with
    | "le", value -> Scanf.sscanf value "%d.%d" (fun x y -> Version (Le, x, y))
    | "ge", value -> Scanf.sscanf value "%d.%d" (fun x y -> Version (Ge, x, y))
    | "os", value -> Os_type (String.lowercase_ascii value)
    | _ -> failwith (Printf.sprintf "Syntax error condition: %s" condition)
  in

  let rec parse_from line pos =
    match get_tag_from_opt line pos with
    | None -> [ Raw (String.sub line pos (String.length line - pos)); Eol ]
    | Some (s, e) ->
      let tag = String.sub line (s + 4) (e - s - 4) |> String.trim in
      flush_all ();
      let op, rest = split_trim tag ' ' in
      let next_token =
        match op with
        | "if" -> If (parse_condition rest)
        | "elif" -> Elseif (parse_condition rest)
        | "else_" -> Else
        | "endif" -> Endif
        | _ -> Raw (String.sub line s (e - s + 1))
      in
      if s = 0 && s = String.length line then
        [ next_token ]
      else
        next_token :: parse_from line (e + 1)
  in

  let parse_line () : line list =
    match input_line ic with
    | exception End_of_file -> [ Eof ]
    | line -> parse_from line 0
  in

  let get_next =
    let q = Queue.create () in
    fun () ->
      try Queue.pop q
      with Queue.Empty ->
        List.iter (fun x -> Queue.push x q) (parse_line ());
        Queue.pop q
  in

  (* entry point *)
  let rec top () =
    match get_next () with
    | Eof -> ()
    | If condition ->
      if eval ~conf condition then (
        pp_pos ();
        cat_block ()
      ) else
        skip_block ~elseok:true ()
    | Raw s ->
      print_string s;
      top ()
    | Eol ->
      print_newline ();
      top ()
    | Elseif _ | Else | Endif -> fail "unexpected elseif|else|endif"
  (* current block is the valid one *)
  and cat_block () =
    match get_next () with
    | Eof -> fail "unexpected EOF"
    | If _ -> fail "nested if not supported"
    | Raw s ->
      print_string s;
      cat_block ()
    | Eol ->
      print_newline ();
      cat_block ()
    | Endif ->
      flush_all ();
      pp_pos ();
      top ()
    | Elseif _ | Else -> skip_block ~elseok:false ()
  (* skip current block.
     @param elseok if true, we should evaluate "elseif" *)
  and skip_block ~elseok () =
    match get_next () with
    | Eof -> fail "unexpected EOF"
    | If _ -> fail "nested if not supported"
    | Raw _ | Eol -> skip_block ~elseok ()
    | Endif ->
      pp_pos ();
      top ()
    | Elseif condition ->
      if elseok && eval ~conf condition then (
        pp_pos ();
        cat_block ()
      ) else
        skip_block ~elseok ()
    | Else ->
      if elseok then (
        pp_pos ();
        cat_block ()
      ) else
        skip_block ~elseok ()
  in
  top ()

let () =
  let t0 = Unix.gettimeofday () in
  let file = Sys.argv.(1) in
  let c = C.create "main" in
  let version = C.ocaml_config_var_exn c "version" in
  let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
  let os_type = String.lowercase_ascii (C.ocaml_config_var_exn c "os_type") in

  let ic = open_in file in
  preproc_lines ~file ~conf:{ os_type; major; minor } ic;

  Printf.printf "(* file preprocessed in %.3fs *)\n" (Unix.gettimeofday () -. t0);
  ()
