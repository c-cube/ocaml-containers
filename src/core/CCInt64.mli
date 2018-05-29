(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Int64}

    Helpers for 64-bit integers

    @since 0.13 *)

include module type of struct include Int64 end

val (+) : t -> t -> t
(** Addition. *)

val (-) : t -> t -> t
(** Subtraction. *)

val (~-) : t -> t
(** Unary negation. *)

val ( * ) : t -> t -> t
(** Multiplication. *)

val (/) : t -> t -> t
(** Integer division.  Raise [Division_by_zero] if the second
    argument is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}. *)

val (mod) : t -> t -> t
(** Integer remainder.
    If [y = 0], [x mod y] raises [Division_by_zero]. *)

val abs : t -> t
(** Return the absolute value of its argument. *)

val max_int : t
(** The greatest representable 64-bit integer, 2{^63} - 1 = [9_223_372_036_854_775_807]. *)

val min_int : t
(** The smallest representable 64-bit integer, -2{^63} = [-9_223_372_036_854_775_808]. *)

val (land) : t -> t -> t
(** Bitwise logical and. *)

val (lor) : t -> t -> t
(** Bitwise logical or. *)

val (lxor) : t -> t -> t
(** Bitwise logical exclusive or. *)

val lnot : t -> t
(** Bitwise logical negation. *)

val (lsl) : t -> int -> t
(** [ x lsl y] shifts [x] to the left by [y] bits, filling in with zeroes.
    The result is unspecified if [y < 0] or [y >= 64]. *)

val (lsr) : t -> int -> t
(** [x lsr y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 64]. *)

val (asr) : t -> int -> t
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

val equal : t -> t -> bool
(** The equal function for 64-bit integers.
    Like {!Pervasives.(=) x y)}. *)

val compare : t -> t -> int
(** The comparison function for 64-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [CCInt64] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val hash : t -> int
(** Like {!Pervasives.abs (to_int x)}. *)

(** {2 Conversion} *)

val to_int : t -> int
(** Convert the given 64-bit integer (type [int64]) to an
    integer (type [int]).  On 64-bit platforms, the 64-bit integer
    is taken modulo 2{^63}, i.e. the high-order bit is lost
    during the conversion.  On 32-bit platforms, the 64-bit integer
    is taken modulo 2{^31}, i.e. the top 33 bits are lost
    during the conversion. *)

val of_int : int -> t
(** Alias to {!Int64.of_int}.
    NOTE: used to return an option, but the function actually never fails. *)

val of_int_exn : int -> t
(** Alias to {!Int64.of_int}.
    @deprecated since 2.1, use {!Int64.of_int} instead. *)

val to_int32 : t -> int32
(** Convert the given 64-bit integer (type [int64]) to a
    32-bit integer (type [int32]). The 64-bit integer
    is taken modulo 2{^32}, i.e. the top 32 bits are lost
    during the conversion.  *)

val of_int32 : int32 -> t
(** Alias to {!Int64.of_int32}.
    NOTE: use to return an option, but the function actually never fails. *)

val of_int32_exn : int32 -> t
(** Alias to {!Int64.of_int32}.
    @deprecated since 2.1, use {!Int64.of_int32} instead. *)

val to_nativeint : t -> nativeint
(** Convert the given 64-bit integer (type [int64]) to a
    native integer.  On 32-bit platforms, the 64-bit integer
    is taken modulo 2{^32}.  On 64-bit platforms,
    the conversion is exact. *)

val of_nativeint : nativeint -> t
(** Alias to {!Int64.of_nativeint}.
    NOTE: use to return an option, but the function actually never fails. *)

val of_nativeint_exn : nativeint -> t
(** Alias to {!Int64.of_nativeint}.
    @deprecated since 2.1, use {!Int64.of_nativeint} instead. *)

val to_float : t -> float
(** Convert the given 64-bit integer to a floating-point number. *)

val of_float : float -> t
(** Alias to {!Int64.of_float}.
    Convert the given floating-point number to a 64-bit integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range \[{!CCInt64.min_int}, {!CCInt64.max_int}\].
    NOTE: used to return an option, but the function never fails. *)

val of_float_exn : float -> t
(** Alias to {!Int64.of_float}.
    @deprecated since 2.1, use {!Int64.of_float} instead. *)

val to_string : t -> string
(** Return the string representation of its argument, in decimal. *)

val of_string : string -> t option
(** Safe version of {!of_string_exn}. *)

val of_string_opt : string -> t option
(** Alias to {!of_string}.
    @since 2.1 *)

val of_string_exn : string -> t
(** Alias to {!Int64.of_string}.
    Convert the given string to a 64-bit integer.
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
