
(** {1 Code generators} *)

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
  val field_int : t -> width:int -> string -> unit

  val total_width : t -> int

  val gen_mli : t -> code
  val gen_ml : t -> code
end

val emit_file : string -> code list -> unit
val emit_chan : out_channel -> code list -> unit
val emit_string : code list -> string

