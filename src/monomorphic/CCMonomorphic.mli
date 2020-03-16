
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Shadow unsafe functions and operators from Stdlib} *)
(** @since 2.0 *)

val (=) : int -> int -> bool
val (<>) : int -> int -> bool
val (<) : int -> int -> bool
val (>) : int -> int -> bool
val (<=) : int -> int -> bool
val (>=) : int -> int -> bool

val compare : int -> int -> int
val min : int -> int -> int
val max : int -> int -> int

(** {2 Infix operators for Floats} *)

val (=.) : float -> float -> bool (** @since 2.1 *)

val (<>.) : float -> float -> bool (** @since 2.1 *)

val (<.) : float -> float -> bool (** @since 2.1 *)

val (>.) : float -> float -> bool (** @since 2.1 *)

val (<=.) : float -> float -> bool (** @since 2.1 *)

val (>=.) : float -> float -> bool (** @since 2.1 *)

(** {2 Shadow Dangerous Operators} *)

val (==) : [`Consider_using_CCEqual_physical]
[@@ocaml.deprecated "Please use CCEqual.physical or Stdlib.(==) instead."]

(** @since 2.1 *)
val (!=) : [`Consider_using_CCEqual_physical]
[@@ocaml.deprecated "Please use [not CCEqual.physical] or Stdlib.(!=) instead."]
