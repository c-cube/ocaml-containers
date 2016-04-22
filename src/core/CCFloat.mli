
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Float functions}
@since 0.6.1 *)

type t = float
type fpclass = Pervasives.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

val nan : t

val max_value : t
val min_value : t

val max_finite_value : t

val epsilon : float

val is_nan : t -> bool

val add : t -> t -> t

val sub : t -> t -> t

val neg : t -> t

val abs : t -> t

val scale : t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val equal : t -> t -> bool

val compare : float -> float -> int

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val pp : t printer
val print : t formatter

val hash : t -> int

val random : t -> t random_gen
val random_small : t random_gen
val random_range : t -> t -> t random_gen

val fsign : t -> float
(** [fsign x] is one of [-1., -0., +0., +1.], or [nan] if [x] is NaN.
    @since 0.7 *)

exception TrapNaN of string
val sign_exn : t -> int
(** [sign_exn x] will return the sign of [x] as [1, 0] or [-1], or raise an
    exception [TrapNaN] if [x] is a NaN.
    Note that infinities have defined signs in OCaml.
    @since 0.7 *)

val to_int : t -> int
val of_int : int -> t

val to_string : t -> string
val of_string : string -> t


val equal_precision : epsilon:t -> t -> t -> bool
(** Equality with allowed error up to a non negative epsilon value *)

val classify : float -> fpclass

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
end

include module type of Infix
