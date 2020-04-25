let str_sub ?(offset=0) ~sub:s' s =
  let open String in
  let rec aux i =
    i<length s && (aux_sub i 0 || aux (i+1))
  and aux_sub i j =
    if j = length s' then true
    else if i+j >= length s then false
    else get s (i+j) = get s' j && aux_sub i (j+1)
  in
  aux offset

let is_suffix ~sub s =
  str_sub ~offset:(String.length s - String.length sub) ~sub s

let is_code file = is_suffix ~sub:".ml" file || is_suffix ~sub:".mli" file

let do_not_test file =
  assert (not (is_suffix ~sub:"make.ml" file));
  str_sub ~sub:"Labels.ml" file ||
  is_suffix ~sub:"containers.ml" file ||
  is_suffix ~sub:"_top.ml" file ||
  is_suffix ~sub:"mkflags.ml" file ||
  is_suffix ~sub:"mkshims.ml" file ||
  is_suffix ~sub:"unlabel.ml" file ||
  is_suffix ~sub:"utop.ml" file

let prefix = "src"
let dirs = List.map (fun s-> Filename.concat prefix s)

let list_files dir : string list =
  let rec f ~prefix acc file =
    let file = Filename.concat prefix file in
    if Sys.is_directory file then (
      Array.fold_left (f ~prefix:file) acc (Sys.readdir file)
    ) else (
      if is_code file && not (do_not_test file) then file :: acc else acc
    )
  in
  f ~prefix:"" [] dir

let run_qtest target dirs =
  let files =
    dirs
    |> List.map list_files
    |> List.flatten
    |> List.map (Printf.sprintf "'%s'")
    |> String.concat " "
  in
  let cmd =
    Printf.sprintf "qtest extract --preamble 'open CCShims_;; open CCFun;;' -o %S %s 2>/dev/null"
      target files
  in
  exit (Sys.command cmd)

let () =
  let target = ref "" in
  let dirs = ref [] in
  Arg.parse ["-target", Arg.Set_string target, " set target"]
    (fun d -> dirs := d :: !dirs) "make.ml -target file dir+";
  if !target="" then failwith "please specify a target";
  if Sys.command "which qtest > /dev/null" <> 0 then (
    (* create empty file *)
    let out = open_out !target in
    output_string out "";
    close_out out;
  ) else (
    run_qtest !target !dirs
  )
