(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

  @since 0.14 *)

type t = char

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit
