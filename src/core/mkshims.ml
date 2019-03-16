
module C = Configurator.V1

let shims_pre_408 = "module Stdlib = Pervasives"

let shims_post_408 = "module Stdlib = Stdlib"

let shims_fmt_pre_408 = "
include Format
let cc_update_funs funs f1 f2 =
  let open Format in
  {
    funs with
    mark_open_tag = f1 funs.mark_open_tag;
    mark_close_tag = f2 funs.mark_close_tag;
  }

"
let shims_fmt_post_408 = "
open Format

[@@@ocaml.warning \"-3\"]

let pp_open_tag = pp_open_tag
let pp_close_tag = pp_close_tag
let pp_get_formatter_tag_functions = pp_get_formatter_tag_functions
let pp_set_formatter_tag_functions = pp_set_formatter_tag_functions

let cc_update_funs funs f1 f2 =
  let open Format in
  {
    funs with
    mark_open_tag = f1 ~or_else:funs.mark_open_tag;
    mark_close_tag = f2 ~or_else:funs.mark_close_tag;
  }
"

let shims_list_pre_408 = "
  include List
  type +'a t = 'a list
"
let shims_list_post_408 = "include List"

let write_file f s =
  let out = open_out f in
  output_string out s; flush out; close_out out

let () =
  C.main ~name:"mkshims" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
    write_file "CCShims_.ml" (if (major, minor) >= (4,8) then shims_post_408 else shims_pre_408);
    write_file "CCShimsList_.ml" (if (major, minor) >= (4,8) then shims_list_post_408 else shims_list_pre_408);
    write_file "CCShimsFormat_.ml" (if (major, minor) >= (4,8) then shims_fmt_post_408 else shims_fmt_pre_408);
  )
