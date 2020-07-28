
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Int functions} *)

include module type of CCShimsInt_

type t = int

val zero : t
(** [zero] is the integer [0].
    @since 3.0 *)

val one : t
(** [one] is the integer [1].
    @since 3.0 *)

val minus_one : t
(** [minus_one] is the integer [-1].
    @since 3.0 *)

val add : t -> t -> t
(** [add x y] is [x + y].
    @since 3.0 *)

val sub : t -> t -> t
(** [sub x y] is [x - y].
    @since 3.0 *)

val mul : t -> t -> t
(** [mul x y] is [x * y].
    @since 3.0 *)

val div : t -> t -> t
(** [div x y] is [x / y]
    @since 3.0 *)

val succ : t -> t
(** [succ x] is [x + 1].
    @since 3.0 *)

val pred : t -> t
(** [pred x] is [x - 1].
    @since 3.0 *)

val abs : t -> t
(** [abs x] is the absolute value of [x]. It is [x] if [x] is positive
    and [neg x] otherwise.
    @since 3.0 *)

val max_int : t
(** [max_int] is the maximum integer.
    @since 3.0 *)

val min_int : t
(** [min_int] is the minimum integer.
    @since 3.0 *)

val compare : t -> t -> int
(** [compare x y] is the comparison function for integers
    with the same specification as {!Stdlib.compare}. *)

val equal : t -> t -> bool
(** [equal x y] is [true] iff [x] and [y] are equal.
    Equality function for integers. *)

val hash : t -> int
(** [hash x] computes the hash of [x]. *)

val sign : t -> int
(** [sign x] return [0] if [x = 0], [-1] if [x < 0] and [1] if [x > 0].
    Same as [compare x 0].*)

val neg : t -> t
(** [neg x] is [- x].
    Unary negation.
    @since 0.5 *)

val pow : t -> t -> t
(** [pow base exponent] returns [base] raised to the power of [exponent].
    [pow x y = x^y] for positive integers [x] and [y].
    Raises [Invalid_argument] if [x = y = 0] or [y] < 0.
    @since 0.11 *)

val floor_div : t -> t -> t
(** [floor_div x n] is integer division rounding towards negative infinity.
    It satisfies [x = m * floor_div x n + rem x n].
    @since 1.2 *)

val rem : t -> t -> t
(** [rem x n] is the remainder of dividing [x] by [n], with the same
    sign as [n].
    @since 1.2 *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a
type 'a iter = ('a -> unit) -> unit

val random : int -> t random_gen
val random_small : t random_gen
val random_range : int -> int -> t random_gen

val pp : t printer
(** [pp ppf x] prints the integer [x] on [ppf]. *)

val to_float : t -> float
(** [to_float] is the same as [float_of_int]
    @since 3.0*)

val of_float : float -> t
(** [to_float] is the same as [int_of_float]
    @since 3.0*)

val to_string : t -> string
(** [to_string x] returns the string representation of the integer [x], in signed decimal.
    @since 0.13 *)

val of_string : string -> t option
(** [of_string s] converts the given string [s] into an integer.
    Safe version of {!of_string_exn}.
    @since 0.13 *)

val of_string_exn : string -> t
(** [of_string_exn s] converts the given string [s] to an integer.
    Alias to {!int_of_string}.
    @raise Failure in case of failure.
    @since 3.0 *)

val of_float : float -> t
(** [of_float x] converts the given floating-point number [x] to an integer.
    Alias to {!int_of_float}.
    @since 3.0 *)

val pp_binary : t printer
(** [pp_binary ppf x] prints [x] on [ppf].
    Print as "0b00101010".
    @since 0.20 *)

val to_string_binary : t -> string
(** [to_string_binary x] returns the string representation of the integer [x], in binary.
    @since 0.20 *)

val min : t -> t -> t
(** [min x y] returns the minimum of the two integers [x] and [y].
    @since 0.17 *)

val max : t -> t -> t
(** [max x y] returns the maximum of the two integers [x] and [y].
    @since 0.17 *)

val range_by : step:t -> t -> t -> t iter
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    Use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0].
    @since 1.2 *)

val range : t -> t -> t iter
(** [range i j] iterates on integers from [i] to [j] included . It works
    both for decreasing and increasing ranges.
    @since 1.2 *)

val range' : t -> t -> t iter
(** [range' i j] is like {!range} but the second bound [j] is excluded.
    For instance [range' 0 5 = Iter.of_list [0;1;2;3;4]].
    @since 1.2 *)

val popcount : t -> int
(** Number of bits set to 1
    @since 3.0 *)

val logand : t -> t -> t
(** [logand] is the same as [(land)].
    @since 3.0 *)

val logor : t -> t -> t
(** [logand] is the same as [(lor)].
    @since 3.0 *)

val logxor : t -> t -> t
(** [logxor] is the same as [(lxor)].
    @since 3.0 *)

val lognot : t -> t
(** [logand] is the same as [lnot].
    @since 3.0 *)

val shift_left : t -> int -> t
(** [shift_left] is the same as [(lsl)].
    @since 3.0 *)

val shift_right : t -> int -> t
(** [shift_right] is the same as [(asr)].
    @since 3.0 *)

val shift_right_logical : t -> int -> t
(** [shift_right_logical] is the same as [(lsr)].
    @since 3.0 *)

(** {2 Infix Operators}

    @since 0.17 *)

module Infix : sig
  val (=) : t -> t -> bool
  (** @since 0.17 *)

  val (<>) : t -> t -> bool
  (** @since 0.17 *)

  val (<) : t -> t -> bool
  (** @since 0.17 *)

  val (>) : t -> t -> bool
  (** @since 0.17 *)

  val (<=) : t -> t -> bool
  (** @since 0.17 *)

  val (>=) : t -> t -> bool
  (** @since 0.17 *)

  val (--) : t -> t -> t iter
  (** Alias to {!range}.
      @since 1.2 *)

  val (--^) : t -> t -> t iter
  (** Alias to {!range'}.
      @since 1.2 *)

  val (+) : t -> t -> t (** @since 2.1 *)

  val (-) : t -> t -> t (** @since 2.1 *)

  val (~-) : t -> t (** @since 2.1 *)

  val ( * ) : t -> t -> t (** @since 2.1 *)

  val (/) : t -> t -> t (** @since 2.1 *)

  val ( ** ) : t -> t -> t (** @since 2.4 *)

  val (mod) : t -> t -> t (** @since 2.1 *)

  val (land) : t -> t -> t (** @since 2.1 *)

  val (lor) : t -> t -> t (** @since 2.1 *)

  val (lxor) : t -> t -> t (** @since 2.1 *)

  val lnot : t -> t (** @since 2.1 *)

  val (lsl) : t -> int -> t (** @since 2.1 *)

  val (lsr) : t -> int -> t (** @since 2.1 *)

  val (asr) : t -> int -> t (** @since 2.1 *)

end

include module type of Infix
