
module C = Configurator.V1

let () =
  C.main ~name:"mkflags" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor =
      Scanf.sscanf version "%u.%u"
        (fun major minor -> major, minor)
    in
    let after_4_3 = (major, minor) >= (4, 3) in
    let sexp =
      if after_4_3 then (
        ["-O3"; "-unbox-closures"; "-unbox-closures-factor"; "20"; "-color"; "always"]
      ) else (
        []
      ) in
    C.Flags.write_sexp "flambda.flags" sexp
  )
