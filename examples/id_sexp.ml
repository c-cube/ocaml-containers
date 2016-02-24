
let pp_sexp s = match s with
  | `Ok l ->
      List.iter
        (fun s -> Format.printf "@[%a@]@." CCSexpM.print s)
        l
  | `Error msg ->
      Format.printf "error: %s@." msg

let () =
  match Sys.argv with
  | [| _ |] ->
      let s = CCSexpM.parse_chan_list stdin in
      pp_sexp s
  | [| _; file |] ->
      let s = CCSexpM.parse_file_list file in
      pp_sexp s
  | _ -> failwith "usage: id_sexp [file]"
