

let () =
  if Array.length Sys.argv <> 2 then failwith "usage: id_sexp file";
  let f = Sys.argv.(1) in
  let s = Sexp.parse_l_file f in
  match s with
  | `Ok l ->
      List.iter
        (fun s -> Format.printf "@[%a@]@." Sexp.print s)
        l
  | `Error msg ->
      Format.printf "error: %s@." msg
