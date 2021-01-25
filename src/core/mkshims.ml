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
  (** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html} Documentation for the standard Array module}*)

  module Floatarray = struct type t = float array end
  type 'a t = 'a array
  "

let shims_array_label_pre_406 = "
  include ArrayLabels
  (** {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/ArrayLabels.html} Documentation for the standard ArrayLabels module}*)

  module Floatarray = CCShimsArray_.Floatarray
  type 'a t = 'a array
  "

let shims_array_label_406_408 = "
  include (ArrayLabels : module type of ArrayLabels with module Floatarray = Array.Floatarray)
  (** {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/ArrayLabels.html} Documentation for the standard ArrayLabels module}*)

  type 'a t = 'a array
  "

let shims_array_406_408 = "
  include Array
  (** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html} Documentation for the standard Array module}*)

  type 'a t = 'a array
"
let shims_array_post_408 = "
  include Array
  (** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html} Documentation for the standard Array module}*)
"
let shims_array_label_post_408 = "
  include (ArrayLabels : module type of ArrayLabels with module Floatarray = Array.Floatarray)
  (** {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/ArrayLabels.html} Documentation for the standard ArrayLabels module}*)
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
let shims_let_op_list_post_408 =
  "module type S = sig
   val (and&) : 'a list -> 'b list -> ('a * 'b) list
   (** [(and&)] is {!combine_shortest}.
      It allows to perform a synchronized product between two lists,
        stopping gently at the shortest. Usable both with [let+] and [let*].
    {[
        # let f xs ys zs =
            let+ x = xs
            and& y = ys
            and& z = zs in
            x + y + z;;
        val f : int list -> int list -> int list -> int list = <fun>
        # f [1;2] [5;6;7] [10;10];;
        - : int list = [16; 18]
    ]}
    @since 3.1
  *)
  end

  module Make(X:sig
    val combine_shortest : 'a list -> 'b list -> ('a*'b) list
  end) = struct
    let (and&) = X.combine_shortest
  end
"

let shims_int_pre_408 = ""
let shims_int_post_408 = "
  include Int
  (** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Int.html} Documentation for the standard Int module}*)
"

let shims_int_non_64bit = "
(* we use the simple version for non-64 bits. *)
let popcount (b:int) : int =
  let rec loop count x =
    if x=0 then count
    else loop (count+1) (x land (x-1))
  in
  loop 0 b

"

(* 64 bits: include basic version *)
let shims_int_64bit =
shims_int_non_64bit ^ "
(*
  from https://en.wikipedia.org/wiki/Hamming_weight

  //This uses fewer arithmetic operations than any other known
  //implementation on machines with slow multiplication.
  //It uses 17 arithmetic operations.
  int popcount_2(uint64_t x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits
    x += x >>  8;  //put count of each 16 bits into their lowest 8 bits
    x += x >> 16;  //put count of each 32 bits into their lowest 8 bits
    x += x >> 32;  //put count of each 64 bits into their lowest 8 bits
    return x & 0x7f;
  }

   m1 = 0x5555555555555555
   m2 = 0x3333333333333333
   m4 = 0x0f0f0f0f0f0f0f0f
*)
let popcount_64_ (b:int) : int =
  let b = b - ((b lsr 1) land 0x5555555555555555) in
  let b = (b land 0x3333333333333333) + ((b lsr 2) land 0x3333333333333333) in
  let b = (b + (b lsr 4)) land 0x0f0f0f0f0f0f0f0f in
  let b = b + (b lsr 8) in
  let b = b + (b lsr 16) in
  let b = b + (b lsr 32) in
  b land 0x7f

(* pick at runtime, see:
  - https://github.com/c-cube/ocaml-containers/issues/346
  - https://github.com/ocsigen/js_of_ocaml/issues/1079
  *)
let popcount =
  if Sys.int_size = 63 then popcount_64_ else popcount
"

let shims_either_pre_412 = "
  type ('a, 'b) t = Left of 'a | Right of 'b
  "

let shims_either_post_412 = "
  type ('a, 'b) t = ('a, 'b) Either.t = Left of 'a | Right of 'b
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
    write_file "CCShimsMkLetList_.ml" (if (major, minor) >= (4,8) then shims_let_op_list_post_408 else shims_let_op_list_pre_408);
    (* see if we target native 64 bits (rather than 32 bits or jsoo or sth else) *)
    let int_size =
      try C.ocaml_config_var_exn c "int_size" |> int_of_string
      with e ->
        let n = Sys.int_size in (* default to current version *)
        Printf.eprintf "cannot obtain target int_size:\n%s\ndefaulting to %d\n%!"
          (Printexc.to_string e) n;
        n
    in
    write_file "CCShimsInt_.ml"
      ((if (major, minor) >= (4,8) then shims_int_post_408 else shims_int_pre_408)
      ^ if int_size=63 then shims_int_64bit else shims_int_non_64bit);
    write_file "CCShimsEither_.ml"
      (if (major, minor) >= (4,12) then shims_either_post_412
       else shims_either_pre_412);
  )
