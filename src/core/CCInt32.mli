(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Int32}

    Helpers for 32-bit integers.

    This module provides operations on the type int32 of signed 32-bit integers.
    Unlike the built-in int type, the type int32 is guaranteed to be exactly
    32-bit wide on all platforms. All arithmetic operations over int32 are taken
    modulo 2{^32}.

    Performance notice: values of type int32 occupy more memory space than values
    of type int, and arithmetic operations on int32 are generally slower than
    those on int. Use int32 only when the application requires exact 32-bit arithmetic.

    @since 2.1 *)

include module type of struct include Int32 end
(** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Int32.html} Documentation for the standard Int32 module}*)

val min : t -> t -> t
(** [min x y] returns the minimum of the two integers [x] and [y].
    @since 3.0 *)

val max : t -> t -> t
(** [max x y] returns the maximum of the two integers [x] and [y].
    @since 3.0 *)

val hash : t -> int
(** [hash x] computes the hash of [x].
    Like {!Stdlib.abs (to_int x)}. *)

val sign : t -> int
(** [sign x] return [0] if [x = 0], [-1] if [x < 0] and [1] if [x > 0].
    Same as [compare x zero].
    @since 3.0*)

val pow : t -> t -> t
(** [pow base exponent] returns [base] raised to the power of [exponent].
    [pow x y = x^y] for positive integers [x] and [y].
    Raises [Invalid_argument] if [x = y = 0] or [y] < 0.
    @since 0.11 *)

val floor_div : t -> t -> t
(** [floor_div x n] is integer division rounding towards negative infinity.
    It satisfies [x = m * floor_div x n + rem x n].
    @since 3.0 *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a
type 'a iter = ('a -> unit) -> unit


val range_by : step:t -> t -> t -> t iter
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    Use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0].
    @since 3.0 *)

val range : t -> t -> t iter
(** [range i j] iterates on integers from [i] to [j] included . It works
    both for decreasing and increasing ranges.
    @since 3.0 *)

val range' : t -> t -> t iter
(** [range' i j] is like {!range} but the second bound [j] is excluded.
    For instance [range' 0 5 = Iter.of_list [0;1;2;3;4]].
    @since 3.0 *)

val random : t -> t random_gen
val random_small : t random_gen
val random_range : t -> t -> t random_gen


(** {2 Conversion} *)

val of_string : string -> t option
(** [of_string s] is the safe version of {!of_string_exn}.
    Like {!of_string_exn}, but return [None] instead of raising. *)

val of_string_opt : string -> t option
(** [of_string_opt s] is an alias to {!of_string}. *)

val of_string_exn : string -> t
(** [of_string_exn s] converts the given string [s] into a 32-bit integer.
    Alias to {!Int32.of_string}.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*CCInt32.max_int+1]]. If the input exceeds {!CCInt32.max_int}
    it is converted to the signed integer
    [CCInt32.min_int + input - CCInt32.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Raise [Failure "Int32.of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int32]. *)

val to_string_binary : t -> string
(** [to_string_binary x] returns the string representation of the integer [x], in binary.
    @since 3.0 *)


(** {2 Printing} *)

val pp : t printer
(** [pp ppf x] prints the integer [x] on [ppf].
    @since 3.0 *)

val pp_binary : t printer
(** [pp_binary ppf x] prints [x] on [ppf].
    Print as "0b00101010".
    @since 3.0 *)


(** {2 Infix Operators} *)

module Infix : sig
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
      If [y <> zero], the result of [x mod y] satisfies the following properties:
      [zero <= x mod y < abs y] and
      [x = ((x / y) * y) + (x mod y)].
      If [y = 0], [x mod y] raises [Division_by_zero]. *)

  val ( ** ) : t -> t -> t
  (** Alias to {!pow}
      @since 3.0 *)

  val (--) : t -> t -> t iter
  (** Alias to {!range}.
      @since 3.0 *)

  val (--^) : t -> t -> t iter
  (** Alias to {!range'}.
      @since 3.0 *)

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
      The result is unspecified if [y < 0] or [y >= 32]. *)

  val ( lsr ) : t -> int -> t
  (** [x lsr y] shifts [x] to the right by [y] bits.
      This is a logical shift: zeroes are inserted in the vacated bits
      regardless of the sign of [x].
      The result is unspecified if [y < 0] or [y >= 32]. *)

  val ( asr ) : t -> int -> t
  (** [x asr y] shifts [x] to the right by [y] bits.
      This is an arithmetic shift: the sign bit of [x] is replicated
      and inserted in the vacated bits.
      The result is unspecified if [y < 0] or [y >= 32]. *)

  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (>) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (<) : t -> t -> bool
end

include module type of Infix
