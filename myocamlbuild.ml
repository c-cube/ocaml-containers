(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin;;

dispatch
  (MyOCamlbuildBase.dispatch_combine [
    begin function
    | After_rules ->
      (* replace with Ocamlbuild_cppo.dispatch when 4.00 is not supported
         anymore *)
      let dep = "%(name).cppo.ml" in
      let prod1 = "%(name: <*> and not <*.cppo>).ml" in
      let prod2 = "%(name: <**/*> and not <**/*.cppo>).ml" in
      let f prod env _build =
        let dep = env dep in
        let prod = env prod in
        let tags = tags_of_pathname prod ++ "cppo" in
        Cmd (S[A "cppo"; T tags; S [A "-o"; P prod]; P dep ])
      in
      rule "cppo1" ~dep ~prod:prod1 (f prod1) ;
      rule "cppo2" ~dep ~prod:prod2 (f prod2) ;
      pflag ["cppo"] "cppo_D" (fun s -> S [A "-D"; A s]) ;
      pflag ["cppo"] "cppo_U" (fun s -> S [A "-U"; A s]) ;
      pflag ["cppo"] "cppo_I" (fun s ->
        if Pathname.is_directory s then S [A "-I"; P s]
        else S [A "-I"; P (Pathname.dirname s)]
      ) ;
      pdep ["cppo"] "cppo_I" (fun s ->
        if Pathname.is_directory s then [] else [s]) ;
      flag ["cppo"; "cppo_q"] (A "-q") ;
      flag ["cppo"; "cppo_s"] (A "-s") ;
      flag ["cppo"; "cppo_n"] (A "-n") ;
      pflag ["cppo"] "cppo_x" (fun s -> S [A "-x"; A s]);
      (* end replace *)

      let ocaml_version =
        Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun major minor patchlevel ->
        (* e.g. #define OCAML_VERSION 040201 *)
        Printf.sprintf "OCAML_VERSION %d" (major * 10000 + minor * 100 + patchlevel))
      in
      flag ["cppo"] & S[A"-D"; A ocaml_version]
    | _ -> ()
    end;
    dispatch_default
  ])
