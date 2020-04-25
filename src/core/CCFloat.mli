
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
(** [nan] is Not a Number (NaN). Equal to {!Stdlib.nan}. *)

val max_value : t
(** [max_value] is Positive infinity. Equal to {!Stdlib.infinity}. *)

val min_value : t
(** [min_value] is Negative infinity. Equal to {!Stdlib.neg_infinity}. *)

val max_finite_value : t
(** [max_finite_value] is the largest finite float value. Equal to {!Stdlib.max_float}. *)

val epsilon : t
(** [epsilon] is the smallest positive float x such that [1.0 +. x <> 1.0].
    Equal to {!Stdlib.epsilon_float}. *)

val is_nan : t -> bool
(** [is_nan f] returns [true] if f is NaN, [false] otherwise. *)

val add : t -> t -> t
(** [add x y] is equal to [x +. y]. *)

val sub : t -> t -> t
(** [sub x y] is equal to [x -. y]. *)

val neg : t -> t
(** [neg x] is equal to [~-. x]. *)

val abs : t -> t
(** [abs x] is the absolute value of the floating-point number [x].
    Equal to {!Stdlib.abs_float}. *)

val scale : t -> t -> t
(** [scale x y] is equal to [x *. y]. *)

val min : t -> t -> t
(** [min x y] returns the min of the two given values [x] and [y]. *)

val max : t -> t -> t
(** [max x y] returns the max of the two given values [x] and [y]. *)

val equal : t -> t -> bool
(** [equal x y] is [true] if [x] and [y] are the same. *)

val compare : t -> t -> int
(** [compare x y] is {!Stdlib.compare x y}. *)

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
(** [round x] returns the closest integer value, either above or below.
    For [n + 0.5], [round] returns [n].
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

val of_string_opt : string -> t option
(** @since NEXT_RELEASE *)

val equal_precision : epsilon:t -> t -> t -> bool
(** Equality with allowed error up to a non negative epsilon value. *)

val classify : t -> fpclass
(** [classify x] returns the class of the given floating-point number [x]:
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
