(** Basic Bencode decoder/encoder.

    See https://en.wikipedia.org/wiki/Bencode .

    @since NEXT_RELEASE *)

module Str_map : module type of Map.Make(String)

type t =
  | Int of int64
  | String of string
  | List of t list
  | Map of t Str_map.t

val equal : t -> t -> bool

val hash : t -> int

val pp_debug : Format.formatter -> t -> unit
(** Printer for diagnostic/human consumption *)

val to_string_debug : t -> string

val int : int -> t

val int64 : int64 -> t

val string : string -> t

val list : t list -> t

val map_of_list : (string * t) list -> t

val map : t Str_map.t -> t

(** Encoding *)
module Encode : sig
  val to_string : t -> string

  val to_buffer : Buffer.t -> t -> unit

  val to_chan : out_channel -> t -> unit

  val to_fmt : Format.formatter -> t -> unit
end

(** Decoding *)
module Decode : sig
  val of_string : string -> t option

  val of_string_exn : string -> t
  (** Parse string.
      @raise Failure if the string is not valid bencode. *)
end
