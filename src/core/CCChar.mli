(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

    @since 0.14 *)

type t = char

val equal : t -> t -> bool
val compare : t -> t -> int

val lowercase_ascii : t -> t
(** See {!Char}
    @since 0.20 *)

val uppercase_ascii : t -> t
(** See {!Char}
    @since 0.20 *)

val of_int_exn : int -> t
(** Alias to {!Char.chr}
    @raise Invalid_argument if the int is not within [0,...,255]
    @since 1.0 *)

val of_int : int -> t option
(** Safe version of {!of_int}
    @since 1.0 *)

val to_int : t -> int
(** Alias to {!Char.code}
    @since 1.0 *)

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit
