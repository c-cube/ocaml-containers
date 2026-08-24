(** CBOR encoder/decoder.

  The type is chosen to be compatible with ocaml-cbor.
  See {{: https://www.rfc-editor.org/rfc/rfc8949.html} the RFC}.

  {b note} this is experimental.

  @since 3.9
  *)

type t =
  [ `Null
  | `Undefined
  | `Simple of int
  | `Bool of bool
  | `Int of int64
  | `Float of float
  | `Bytes of string
  | `Text of string
  | `Array of t list
  | `Map of (t * t) list
  | `Tag of int * t
  ]

val pp_diagnostic : t CCFormat.printer
val to_string_diagnostic : t -> string

val encode : ?buf:Buffer.t -> t -> string
(** @raise Invalid_argument if a simple value is outside the range 0 to 255. *)

val decode : string -> (t, string) result
(** Decode the value at the beginning of the string.
    This ignores trailing bytes if the value is only a prefix of the string. *)

val decode_exn : string -> t
(** Like {!decode}.
    @raise Failure if the string isn't valid *)
