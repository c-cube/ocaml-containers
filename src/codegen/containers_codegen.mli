
(** {1 Code generators}

    The code generator library is designed to be used from a build system
    (for example, from [dune]) to generate efficient code for features
    that are harder to provide at runtime.

    The idea is that the build system should invoke some OCaml script
    that depends on [containers.codegen]; the script uses the DSL below
    to describe what code to generate (e.g. a description of a bitfield type)
    and emits a [.ml] file (and possibly a [.mli] file).

    For example, the build script might contain:

{[
module CG = Containers_codegen
let () =
  let module B = CG.Bitfield in
  let b = B.make ~name:"t" () in
  B.field_bit b "x";
  B.field_bit b "y";
  B.field_bit b "z";
  B.field_int b ~width:5 "foo";

  CG.emit_file "foo.mli" (B.gen_mli b);
  CG.emit_file "foo.ml" (B.gen_ml b);
  ()
]}

    and this will produce [foo.ml] and [foo.mli] with a bitfield containing
    [x], [y], and [z].

*)

module Fmt = CCFormat

type code

(** {2 Representation of OCaml code} *)
module Code : sig
  type t = code

  val pp : t Fmt.printer
  val to_string : t -> string

  val mk_pp : unit Fmt.printer -> t
  val mk_str : string -> t
  val in_struct : string -> t list -> t
  val in_sig : string -> t list -> t
end

(** {2 Generate efficient bitfields that fit in an integer} *)
module Bitfield : sig
  type t

  val make :
    ?emit_failure_if_too_wide:bool ->
    name:string ->
    unit -> t
  (** Make a new bitfield with the given name.
      @param name the name of the generated type
      @param emit_failure_if_too_wide if true, generated code includes a runtime
      assertion that {!Sys.int_size} is wide enough to support this type *)

  val field_bit : t -> string -> unit
  (** [field_bit ty name] adds a field of size [1] to the bitfield [ty],
      with name [name]. The generate code will provide get/set for
      a boolean. *)

  val field_int : t -> width:int -> string -> unit
(** [field_int ty name ~width] adds a field of size [width] to
    the bitfield with name [name].
    The accessors will be for integers of [width] bits, and the
    setter might assert that the provided integer fits. *)

  val total_width : t -> int
  (** Total width in bits of the given bitfield. *)

  val gen_mli : t -> code
  (** Generate code for the type signature for the given bitfield *)

  val gen_ml : t -> code
  (** Generate code for the implementation for the given bitfield *)
end

val emit_file : string -> code list -> unit
(** [emit_file file cs] emits code fragments [cs] into the given file
    at path [file] *)

val emit_chan : out_channel -> code list -> unit

val emit_string : code list -> string

