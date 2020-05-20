(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Int64}

    Helpers for 64-bit integers.

    This module provides operations on the type int64 of signed 64-bit integers.
    Unlike the built-in int type, the type int64 is guaranteed to be exactly
    64-bit wide on all platforms. All arithmetic operations over int64 are taken
    modulo 2{^64}.

    Performance notice: values of type int64 occupy more memory space than values
    of type int, and arithmetic operations on int64 are generally slower than
    those on int. Use int64 only when the application requires exact 64-bit arithmetic.

    @since 0.13 *)

include module type of struct include Int64 end

val ( + ) : t -> t -> t
(** [x + y] is the sum of [x] and [y]. 
    Addition. *)

val ( - ) : t -> t -> t
(** [x - y] is the difference of [x] and [y].
    Subtraction. *)

val ( ~- ) : t -> t
(** [~- x] is the negation of [x].
    Unary negation. *)

val ( * ) : t -> t -> t
(** [ x * y] is the product of [x] and [y].
    Multiplication. *)

val ( / ) : t -> t -> t
(** [x / y] is the integer quotient of [x] and [y].
    Integer division.  Raise [Division_by_zero] if the second
    argument [y] is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Stdlib.(/)}. *)

val ( mod ) : t -> t -> t
(** [x mod y] is the integer remainder of [x / y].
    If [y <> zero], the result of [x mod y] satisfies the following property:
    [x = ((x / y) * y) + (x mod y)].
    If [y = 0], [x mod y] raises [Division_by_zero]. *)

val ( land ) : t -> t -> t
(** [x land y] is the bitwise logical and of [x] and [y]. *)

val ( lor ) : t -> t -> t
(** [x lor y] is the bitwise logical or of [x] and [y]. *)

val ( lxor ) : t -> t -> t
(** [x lxor y] is the bitwise logical exclusive or of [x] and [y]. *)

val lnot : t -> t
(** [lnot x] is the bitwise logical negation of [x] (the bits of [x] are inverted). *)

val ( lsl ) : t -> int -> t
(** [ x lsl y] shifts [x] to the left by [y] bits, filling in with zeroes.
    The result is unspecified if [y < 0] or [y >= 64]. *)

val ( lsr ) : t -> int -> t
(** [x lsr y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 64]. *)

val ( asr ) : t -> int -> t
(** [x asr y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)

(** Infix operators
    @since 2.1 *)
module Infix : sig
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (~-) : t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (mod) : t -> t -> t
  val (land) : t -> t -> t
  val (lor) : t -> t -> t
  val (lxor) : t -> t -> t
  val lnot : t -> t
  val (lsl) : t -> int -> t
  val (lsr) : t -> int -> t
  val (asr) : t -> int -> t
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (>) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (<) : t -> t -> bool
end

include module type of Infix

val hash : t -> int
(** [hash x] computes the hash of [x].
    Like {!Stdlib.abs (to_int x)}. *)
  
(** {2 Conversion} *)

val of_string : string -> t option
(** [of_string s] is the safe version of {!of_string_exn}. *)

val of_string_opt : string -> t option
(** [of_string_opt s] is an alias to {!of_string}.
    @since 2.1 *)

val of_string_exn : string -> t
(** [of_string_exn s] converts the given string [s] into a 64-bit integer.
    Alias to {!Int64.of_string}.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*CCInt64.max_int+1]]. If the input exceeds {!CCInt64.max_int}
    it is converted to the signed integer
    [CCInt64.min_int + input - CCInt64.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Raise [Failure "Int64.of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int64]. *)
