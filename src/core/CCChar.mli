
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

  @since NEXT_RELEASE *)

type t = char

val equal : t -> t -> bool
val compare : t -> t -> int

val print : Format.formatter -> t -> unit

