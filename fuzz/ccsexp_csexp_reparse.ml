
let gen_sexp =
  let open! Crowbar in
  let (>|=) = map in
  fix
    (fun self ->
       choose [
         ((bytes :: [] : _ gens) >|= fun s -> `Atom s);
         ((list self :: []) >|= fun l -> `List l);
       ])

let () =
  Crowbar.add_test ~name:"ccsexp_csexp_reparse" [ gen_sexp ]
    (fun s ->
       let str = CCCanonical_sexp.to_string s in
       match CCCanonical_sexp.parse_string_list str with
         | Ok [s2] -> assert (s = s2)
         | Ok _ -> failwith "wrong number of sexps"
         | Error e -> failwith e)
