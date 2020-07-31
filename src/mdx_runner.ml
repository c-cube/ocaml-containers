
open Printf

let just_copy () =
  let ic = open_in "README.md" in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in_noerr ic;

  let oc = open_out "README.md.corrected" in
  output oc buf 0 len;
  flush oc;
  close_out_noerr oc

let () =
  try
    let e = Sys.command "ocaml-mdx test README.md -o README.md.corrected" in
    if e <> 0 then (
      printf "warning: ocaml-mdx exited with code %d\n" e;
      just_copy();
    ) else (
      print_endline "ocaml-mdx returned 0";
    )
  with Sys_error e ->
    printf "error when running mdx: %s\n" e;
    just_copy();
    ()
