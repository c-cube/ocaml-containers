
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic operations on floating-point numbers}
    @since 0.6.1 *)

open CCShims_

type t = float
type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

val nan : t
(** Equal to {!Stdlib.nan}. *)

val max_value : t
(** Positive infinity. Equal to {!Stdlib.infinity}. *)

val min_value : t
(** Negative infinity. Equal to {!Stdlib.neg_infinity}. *)

val max_finite_value : t
(** Equal to {!Stdlib.max_float}. *)

val epsilon : t
(** The smallest positive float x such that [1.0 +. x <> 1.0].
    Equal to {!Stdlib.epsilon_float}. *)

val is_nan : t -> bool
(** [is_nan f] returns [true] if f is NaN, [false] otherwise. *)

val add : t -> t -> t
(** Equal to [(+.)]. *)

val sub : t -> t -> t
(** Equal to [(-.)]. *)

val neg : t -> t
(** Equal to [(~-.)]. *)

val abs : t -> t
(** The absolute value of a floating-point number.
    Equal to {!Stdlib.abs_float}. *)

val scale : t -> t -> t
(** Equal to [( *. )]. *)

val min : t -> t -> t

val max : t -> t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val pp : t printer

val hash : t -> int

val random : t -> t random_gen
val random_small : t random_gen
val random_range : t -> t -> t random_gen

val fsign : t -> t
(** [fsign x] is one of [-1., -0., +0., +1.], or [nan] if [x] is NaN.
    @since 0.7 *)

val round : t -> t
(** [round f] returns the closest integer value, either above or below.
    @since 0.20 *)

exception TrapNaN of string

val sign_exn : t -> int
(** [sign_exn x] will return the sign of [x] as [1, 0] or [-1], or raise an
    exception [TrapNaN] if [x] is NaN.
    Note that infinities have defined signs in OCaml.
    @since 0.7 *)

val to_int : t -> int
(** Alias to {!int_of_float}.
    Unspecified if outside of the range of integers. *)

val of_int : int -> t
(** Alias to {!float_of_int}. *)

val to_string : t -> string

val of_string_exn : string -> t
(** Alias to {!float_of_string}.
    @raise Failure in case of failure.
    @since 1.2 *)

val of_string : string -> t
(** Alias to {!float_of_string}.
    @deprecated since 1.2, use {!of_string_exn} instead.
    @raise Failure in case of failure. *)

val equal_precision : epsilon:t -> t -> t -> bool
(** Equality with allowed error up to a non negative epsilon value. *)

val classify : t -> fpclass
(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite or nan (not a number). *)

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

  val (+) : t -> t -> t
  (** Addition.
      @since 2.1 *)

  val (-) : t -> t -> t
  (** Subtraction.
      @since 2.1 *)

  val (~-) : t -> t
  (** Unary negation.
      @since 2.1 *)

  val ( * ) : t -> t -> t
  (** Multiplication.
      @since 2.1 *)

  val (/) : t -> t -> t
  (** Division.
      @since 2.1 *)
end

include module type of Infix
