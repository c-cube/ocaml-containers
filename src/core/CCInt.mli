
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Int functions} *)

type t = int

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val sign : t -> int
(** [sign i] is one of [-1, 0, 1] *)

val neg : t -> t
(** [neg i = - i]
    @since 0.5 *)

val pow : t -> t -> t
(** [pow a b = a^b] for positive integers [a] and [b].
    Raises [Invalid_argument] if [a = b = 0] or [b] < 0.
    @since 0.11 *)

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val random : int -> t random_gen
val random_small : t random_gen
val random_range : int -> int -> t random_gen

val pp : t printer
val print : t formatter

val to_string : t -> string
(** @since 0.13 *)

val of_string : string -> t option
(** @since 0.13 *)

val min : t -> t -> t
(** @since 0.17 *)

val max : t -> t -> t
(** @since 0.17 *)

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
