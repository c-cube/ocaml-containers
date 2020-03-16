(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Nativeint}

    Helpers for processor-native integers

    This module provides operations on the type [nativeint] of signed 32-bit integers 
    (on 32-bit platforms) or signed 64-bit integers (on 64-bit platforms). 
    This integer type has exactly the same width as that of a pointer type in the C compiler. 
    All arithmetic operations over nativeint are taken modulo 2{^32} or 2{^64} depending 
    on the word size of the architecture.

    Performance notice: values of type [nativeint] occupy more memory space than values of type [int], 
    and arithmetic operations on [nativeint] are generally slower than those on [int]. 
    Use [nativeint] only when the application requires the extra bit of precision over the [int] type.

    @since 2.1 *)

include module type of struct include Nativeint end

val ( + ) : t -> t -> t
(** Addition. *)

val ( - ) : t -> t -> t
(** Subtraction. *)

val ( ~- ) : t -> t
(** Unary negation. *)

val ( * ) : t -> t -> t
(** Multiplication. *)

val ( / ) : t -> t -> t
(** Integer division.  Raise [Division_by_zero] if the second
    argument is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Stdlib.(/)}. *)

val ( mod ) : t -> t -> t
(** [x mod y ] is the integer remainder.
    If [y <> zero], the result of [x mod y] satisfies the following properties:
    [zero <= x mod y < abs y] and
    [x = ((x / y) * y) + (x mod y)].
    If [y = 0], [x mod y] raises [Division_by_zero]. *)

val ( land ) : t -> t -> t
(** Bitwise logical and. *)

val ( lor ) : t -> t -> t
(** Bitwise logical or. *)

val ( lxor ) : t -> t -> t
(** Bitwise logical exclusive or. *)

val lnot : t -> t
(** Bitwise logical negation. *)

val ( lsl ) : t -> int -> t
(** [ x lsl y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= bitsize], where [bitsize] is [32] on a 32-bit platform
    and [64] on a 64-bit platform. *)

val ( lsr ) : t -> int -> t
(** [x lsr y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

val ( asr ) : t -> int -> t
(** [x asr y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

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

val hash : t -> int
(** Like {!Stdlib.abs (to_int x)}. *)

(** {2 Conversion} *)

val to_int : t -> int
(** Convert the given native integer (type [nativeint]) to an
    integer (type [int]). The high-order bit is lost
    during the conversion. *)

val of_int : int -> t
(** Alias to {!Nativeint.of_int}.
    Convert the given integer (type [int]) to a native integer (type [nativeint]). *)

val to_float : t -> float
(** Convert the given native integer to a floating-point number. *)

val of_float : float -> t
(** Alias to {!Nativeint.of_float}.
    Convert the given floating-point number to a native integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation, the number
    is outside the range \[{!CCNativeint.min_int}, {!CCNativeint.max_int}\]. *)

val to_string : t -> string
(** Return the string representation of its argument, in decimal. *)

val of_string_exn : string -> t
(** Alias to {!Nativeint.of_string}.
    Convert the given string to a native integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*CCNativeint.max_int+1]]. If the input exceeds {!CCNativeint.max_int}
    it is converted to the signed integer
    [CCInt64.min_int + input - CCNativeint.max_int - 1].

    Raise [Failure "Nativeint.of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [nativeint]. *)

val of_string : string -> t option
(** Safe version of {!of_string_exn}.
    Like {!of_string_exn}, but return [None] instead of raising. *)

val of_string_opt : string -> t option
(** Alias to {!of_string}. *)
