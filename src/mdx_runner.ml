
open Printf

let just_copy () =
  CCIO.with_out "README.md.corrected" (fun oc ->
      CCIO.with_in "README.md" (fun ic ->
          CCIO.copy_into ic oc))

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
