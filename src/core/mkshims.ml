module C = Configurator.V1

let write_file f s =
  let out = open_out f in
  output_string out s; flush out; close_out out

let shims_pre_407 = "module Stdlib = Pervasives"

let shims_post_407 = "module Stdlib = Stdlib"

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

let shims_fun_pre_408 = "
  external id : 'a -> 'a = \"%identity\"
  let flip f x y = f y x
  let const x _ = x
  let negate f x = not (f x)
  let protect ~finally f =
    try
      let x= f() in
      finally();
      x
    with e ->
      finally();
      raise e

"
let shims_fun_mli_pre_408 = "
  (** This is an API imitating the new standard Fun module *)
  external id : 'a -> 'a = \"%identity\"
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val const : 'a -> _ -> 'a
  val negate : ('a -> bool) -> 'a -> bool

  val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
  (* this doesn't have the exact same semantics as the stdlib's finally.
      It will not attempt to catch exceptions raised from [finally] at all. *)
"

let shims_fun_post_408 = "include Fun"
let shims_fun_mli_post_408 = "include module type of Fun"

let shims_list_pre_408 = "
  include List
  type +'a t = 'a list
"
let shims_list_post_408 = "include List"

let shims_array_pre_406 = "
  include Array
  module Floatarray = struct type t = float array end
  type 'a t = 'a array
  "

let shims_array_label_pre_406 = "
  include ArrayLabels
  module Floatarray = CCShimsArray_.Floatarray
  type 'a t = 'a array
  "

let shims_array_label_406_408 = "
  include (ArrayLabels : module type of ArrayLabels with module Floatarray = Array.Floatarray)
  type 'a t = 'a array
  "

let shims_array_406_408 = "
  include Array
  type 'a t = 'a array
"
let shims_array_post_408 = "include Array"
let shims_array_label_post_408 = "include (ArrayLabels : module type of ArrayLabels with module Floatarray = Array.Floatarray)"

let shims_let_op_pre_408 =
  "
   module type S = sig type 'a t_let end
   module Make(X:sig type 'a t end) = struct type 'a t_let = 'a X.t end

   module type S2 = sig type ('a,'b) t_let2 end
   module Make2(X:sig type ('a,'b) t end) = struct type ('a,'b) t_let2 = ('a,'b) X.t end
"
let shims_let_op_post_408 =
  "
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

let () =
  C.main ~name:"mkshims" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
    write_file "CCShims_.ml" (if (major, minor) >= (4,7) then shims_post_407 else shims_pre_407);
    write_file "CCShimsList_.ml" (if (major, minor) >= (4,8) then shims_list_post_408 else shims_list_pre_408);
    write_file "CCShimsArray_.ml"
      (if (major, minor) >= (4,8) then shims_array_post_408
       else if (major, minor) >= (4,6) then shims_array_406_408
       else shims_array_pre_406);
    write_file "CCShimsArrayLabels_.ml"
      (if (major, minor) >= (4,8) then shims_array_label_post_408
       else if (major, minor) >= (4,6) then shims_array_label_406_408
       else shims_array_label_pre_406);
    write_file "CCShimsFormat_.ml" (if (major, minor) >= (4,8) then shims_fmt_post_408 else shims_fmt_pre_408);
    write_file "CCShimsFun_.ml" (if (major, minor) >= (4,8) then shims_fun_post_408 else shims_fun_pre_408);
    write_file "CCShimsFun_.mli" (if (major, minor) >= (4,8) then shims_fun_mli_post_408 else shims_fun_mli_pre_408);
    write_file "CCShimsMkLet_.ml" (if (major, minor) >= (4,8) then shims_let_op_post_408 else shims_let_op_pre_408);
  )
