module C = Configurator.V1

type op =
  | Le
  | Ge
  | Gt
  | Lt

type line =
  | If of op * int * int
  | Elseif of op * int * int
  | Else
  | Endif
  | Raw of string
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

let eval ~major ~minor op i j =
  match op with
  | Le -> (major, minor) <= (i, j)
  | Lt -> (major, minor) < (i, j)
  | Ge -> (major, minor) >= (i, j)
  | Gt -> (major, minor) > (i, j)

let preproc_lines ~file ~major ~minor (ic : in_channel) : unit =
  let pos = ref 0 in
  let fail msg =
    failwith (Printf.sprintf "at line %d in '%s': %s" !pos file msg)
  in
  let pp_pos () = Printf.printf "#%d %S\n" !pos file in

  let parse_line () : line =
    match input_line ic with
    | exception End_of_file -> Eof
    | line ->
      let line' = String.trim line in
      incr pos;
      if line' <> "" && line'.[0] = '[' then
        if prefix line' ~pre:"[@@@ifle" then
          Scanf.sscanf line' "[@@@ifle %d.%d]" (fun x y -> If (Le, x, y))
        else if prefix line' ~pre:"[@@@iflt" then
          Scanf.sscanf line' "[@@@iflt %d.%d]" (fun x y -> If (Lt, x, y))
        else if prefix line' ~pre:"[@@@ifge" then
          Scanf.sscanf line' "[@@@ifge %d.%d]" (fun x y -> If (Ge, x, y))
        else if prefix line' ~pre:"[@@@ifgt" then
          Scanf.sscanf line' "[@@@ifgt %d.%d]" (fun x y -> If (Gt, x, y))
        else if prefix line' ~pre:"[@@@elifle" then
          Scanf.sscanf line' "[@@@elifle %d.%d]" (fun x y -> Elseif (Le, x, y))
        else if prefix line' ~pre:"[@@@elifge" then
          Scanf.sscanf line' "[@@@elifge %d.%d]" (fun x y -> Elseif (Ge, x, y))
        else if prefix line' ~pre:"[@@@eliflt" then
          Scanf.sscanf line' "[@@@eliflt %d.%d]" (fun x y -> Elseif (Lt, x, y))
        else if prefix line' ~pre:"[@@@elifge" then
          Scanf.sscanf line' "[@@@elifge %d.%d]" (fun x y -> Elseif (Ge, x, y))
        else if line' = "[@@@else_]" then
          Else
        else if line' = "[@@@endif]" then
          Endif
        else
          Raw line
      else
        Raw line
  in

  (* entry point *)
  let rec top () =
    match parse_line () with
    | Eof -> ()
    | If (op, i, j) ->
      if eval ~major ~minor op i j then (
        pp_pos ();
        cat_block ()
      ) else
        skip_block ~elseok:true ()
    | Raw s ->
      print_endline s;
      top ()
    | Elseif _ | Else | Endif -> fail "unexpected elseif|else|endif"
  (* current block is the valid one *)
  and cat_block () =
    match parse_line () with
    | Eof -> fail "unexpected EOF"
    | If _ -> fail "nested if not supported"
    | Raw s ->
      print_endline s;
      cat_block ()
    | Endif ->
      pp_pos ();
      top ()
    | Elseif _ | Else -> skip_block ~elseok:false ()
  (* skip current block.
     @param elseok if true, we should evaluate "elseif" *)
  and skip_block ~elseok () =
    match parse_line () with
    | Eof -> fail "unexpected EOF"
    | If _ -> fail "nested if not supported"
    | Raw _ -> skip_block ~elseok ()
    | Endif ->
      pp_pos ();
      top ()
    | Elseif (op, i, j) ->
      if elseok && eval ~major ~minor op i j then (
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

  let ic = open_in file in
  preproc_lines ~file ~major ~minor ic;

  Printf.printf "(* file preprocessed in %.3fs *)\n" (Unix.gettimeofday () -. t0);
  ()
