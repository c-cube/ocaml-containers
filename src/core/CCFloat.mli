(* This file is free software, part of containers. See file "license" for more details. *)

(** Basic operations on floating-point numbers
    @since 0.6.1 *)

include module type of Float

val max_value : t
(** [max_value] is Positive infinity. Equal to {!Stdlib.infinity}. *)

val min_value : t
(** [min_value] is Negative infinity. Equal to {!Stdlib.neg_infinity}. *)

val max_finite_value : t
(** [max_finite_value] is the largest finite float value. Equal to {!Stdlib.max_float}. *)

val scale : t -> t -> t
(** [scale x y] is equal to [x *. y]. *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val pp : t printer
val random : t -> t random_gen
val random_small : t random_gen
val random_range : t -> t -> t random_gen

val fsign : t -> t
(** [fsign x] is one of [-1., -0., +0., +1.], or [nan] if [x] is NaN.
    @since 0.7 *)

exception TrapNaN of string

val sign_exn : t -> int
(** [sign_exn x] will return the sign of [x] as [1, 0] or [-1], or raise an
    exception [TrapNaN] if [x] is NaN.
    Note that infinities have defined signs in OCaml.
    @since 0.7 *)

val of_string_exn : string -> t
(** Alias to {!float_of_string}.
    @raise Failure in case of failure.
    @since 1.2 *)

val equal_precision : epsilon:t -> t -> t -> bool
(** Equality with allowed error up to a non negative epsilon value. *)

val classify : t -> fpclass
(** [classify x] returns the class of the given floating-point number [x]:
    normal, subnormal, zero, infinite or nan (not a number). *)

(** {2 Infix Operators}

    @since 0.17 *)

module Infix : sig
  val ( = ) : t -> t -> bool
  (** @since 0.17 *)

  val ( <> ) : t -> t -> bool
  (** @since 0.17 *)

  val ( < ) : t -> t -> bool
  (** @since 0.17 *)

  val ( > ) : t -> t -> bool
  (** @since 0.17 *)

  val ( <= ) : t -> t -> bool
  (** @since 0.17 *)

  val ( >= ) : t -> t -> bool
  (** @since 0.17 *)

  val ( + ) : t -> t -> t
  (** Addition.
      @since 2.1 *)

  val ( - ) : t -> t -> t
  (** Subtraction.
      @since 2.1 *)

  val ( ~- ) : t -> t
  (** Unary negation.
      @since 2.1 *)

  val ( * ) : t -> t -> t
  (** Multiplication.
      @since 2.1 *)

  val ( / ) : t -> t -> t
  (** Division.
      @since 2.1 *)
end

include module type of Infix
