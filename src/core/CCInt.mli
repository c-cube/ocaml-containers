
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Int functions} *)

type t = int

val compare : t -> t -> int
(** The comparison function for integers with the same specification as {!Pervasives.compare}. *)

val equal : t -> t -> bool
(** Equality function for integers. *)

val hash : t -> int

val sign : t -> int
(** [sign i] is one of [-1, 0, 1]. *)

val neg : t -> t
(** Unary negation. [neg i = - i].
    @since 0.5 *)

val pow : t -> t -> t
(** [pow base exponent] returns [base] raised to the power of [exponent].
    [pow a b = a^b] for positive integers [a] and [b].
    Raises [Invalid_argument] if [a = b = 0] or [b] < 0.
    @since 0.11 *)

val floor_div : t -> t -> t
(** [floor_div a n] is integer division rounding towards negative infinity.
    It satisfies [a = m * floor_div a n + rem a n].
    @since 1.2 *)

val rem : t -> t -> t
(** [rem a n] is the remainder of dividing [a] by [n], with the same
    sign as [n].
    @since 1.2 *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a
type 'a sequence = ('a -> unit) -> unit

val random : int -> t random_gen
val random_small : t random_gen
val random_range : int -> int -> t random_gen

val pp : t printer

val to_string : t -> string
(** Return the string representation of its argument, in signed decimal.
    @since 0.13 *)

val of_string : string -> t option
(** @since 0.13 *)

val pp_binary : t printer
(** Print as "0b00101010".
    @since 0.20 *)

val to_string_binary : t -> string
(** @since 0.20 *)

val min : t -> t -> t
(** The minimum of two integers.
    @since 0.17 *)

val max : t -> t -> t
(** The maximum of two integers.
    @since 0.17 *)

val range_by : step:t -> t -> t -> t sequence
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    Use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0].
    @since 1.2 *)

val range : t -> t -> t sequence
(** [range i j] iterates on integers from [i] to [j] included . It works
    both for decreasing and increasing ranges.
    @since 1.2 *)

val range' : t -> t -> t sequence
(** Like {!range} but the second bound is excluded.
    For instance [range' 0 5 = Iter.of_list [0;1;2;3;4]].
    @since 1.2 *)

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

  val (--) : t -> t -> t sequence
  (** Alias to {!range}.
      @since 1.2 *)

  val (--^) : t -> t -> t sequence
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
