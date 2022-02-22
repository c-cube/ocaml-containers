module C = Configurator.V1

let write_file f s =
  let out = open_out f in
  output_string out s; flush out; close_out out

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

let shims_let_op_pre_408 =
  "
   (** glue code for let-operators on OCaml < 4.08 (auto generated) *)
   module type S = sig type 'a t_let end
   module Make(X:sig type 'a t end) = struct type 'a t_let = 'a X.t end

   module type S2 = sig type ('a,'b) t_let2 end
   module Make2(X:sig type ('a,'b) t end) = struct type ('a,'b) t_let2 = ('a,'b) X.t end
"
let shims_let_op_post_408 =
  " (** glue code for let-operators on OCaml >= 4.08 (auto generated) *)
    module type S = sig
      type 'a t_let
      val (let+) : 'a t_let -> ('a -> 'b) -> 'b t_let
      val (and+) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
      val (let*) : 'a t_let -> ('a -> 'b t_let) -> 'b t_let
      val (and*) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
    end
   module Make(X:sig
    type 'a t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val monoid_product : 'a t -> 'b t -> ('a * 'b) t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    end) : S with type 'a t_let = 'a X.t = struct
      type 'a t_let = 'a X.t
      let (let+) = X.(>|=)
      let (and+) = X.monoid_product
      let (let*) = X.(>>=)
      let (and*) = X.monoid_product
  end[@@inline]

    module type S2 = sig
      type ('a,'e) t_let2
      val (let+) : ('a,'e) t_let2 -> ('a -> 'b) -> ('b,'e) t_let2
      val (and+) : ('a,'e) t_let2 -> ('b,'e) t_let2 -> ('a * 'b, 'e) t_let2
      val (let*) : ('a,'e) t_let2 -> ('a -> ('b,'e) t_let2) -> ('b,'e) t_let2
      val (and*) : ('a,'e) t_let2 -> ('b,'e) t_let2 -> ('a * 'b,'e) t_let2
    end

   module Make2(X:sig
    type ('a,'b) t
    val (>|=) : ('a,'e) t -> ('a -> 'b) -> ('b,'e) t
    val monoid_product : ('a,'e) t -> ('b,'e) t -> ('a * 'b, 'e) t
    val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
    end) : S2 with type ('a,'e) t_let2 = ('a,'e) X.t = struct
      type ('a,'e) t_let2 = ('a,'e) X.t
      let (let+) = X.(>|=)
      let (and+) = X.monoid_product
      let (let*) = X.(>>=)
      let (and*) = X.monoid_product
  end[@@inline]
"

let shims_let_op_list_pre_408 =
  "
   (** glue code for let-operators on OCaml < 4.08 (auto generated) *)
   module type S = sig end
   module Make(X:sig end) = struct end
"

let () =
  C.main ~name:"mkshims" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
    write_file "CCShimsFormat_.ml" (if (major, minor) >= (4,8) then shims_fmt_post_408 else shims_fmt_pre_408);
  )
