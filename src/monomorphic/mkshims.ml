
module C = Configurator.V1

let shims_pre_408 = "module Stdlib = Pervasives"
let shims_post_408 = "module Stdlib = Stdlib"

let () =
  C.main ~name:"mkshims" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
    print_endline (if (major, minor) >= (4,8) then shims_post_408 else shims_pre_408))
