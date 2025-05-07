(** LEB128 encoding and decoding.

    See https://en.wikipedia.org/wiki/LEB128 . *)

module Byte_slice = CCByte_slice
module Byte_buffer = CCByte_buffer

module Decode : sig
  val decode_zigzag : int64 -> int64
  (** Turn an unsigned integer into a signed one.

      See https://en.wikipedia.org/wiki/Variable-length_quantity#Zigzag_encoding
  *)

  val skip : Byte_slice.t -> int -> int
  (** [skip slice off] reads an integer at offset [off], and returns how many
      bytes the integer occupies. *)

  val u64 : Byte_slice.t -> int -> int64 * int
  (** [u64 slice off] reads an integer at offset [off], and returns a pair
      [v, n_consumed]. [v] is the read integer, [n_consumed] is the number of
      bytes consumed during reading. *)

  val i64 : Byte_slice.t -> int -> int64 * int
  (** Read a signed int64 by reading a u64 and zigzag decoding it *)

  val int_truncate : Byte_slice.t -> int -> int * int
  (** Like {!i64} but truncates to integer. Returns a pair [v, n_consumed]. *)

  val uint_truncate : Byte_slice.t -> int -> int * int
  (** Like {!u64} but truncates to integer. *)
end

module Encode : sig
  val encode_zigzag : int64 -> int64
  (** Turn a signed int64 into a u64 via zigzag encoding. *)

  val u64 : Byte_buffer.t -> int64 -> unit
  (** Write a unsigned int *)

  val i64 : Byte_buffer.t -> int64 -> unit
  (** Write a signed int via zigzag encoding *)

  val uint : Byte_buffer.t -> int -> unit
  (** Turn an uint into a u64 and write it *)

  val int : Byte_buffer.t -> int -> unit
  (** Turn an int into a int64 and write it *)
end
