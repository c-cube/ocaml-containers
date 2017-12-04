
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Shadow unsafe functions and operators from Pervasives} *)
(** @since NEXT_RELEASE *)

val (=) : int -> int -> bool
val (<>) : int -> int -> bool
val (<) : int -> int -> bool
val (>) : int -> int -> bool
val (<=) : int -> int -> bool
val (>=) : int -> int -> bool

val compare : int -> int -> int
val min : int -> int -> int
val max : int -> int -> int
