(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

    @since 0.14 *)

include module type of struct include Char end

val equal : t -> t -> bool
(** The equal function for chars. *)

val compare : t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val lowercase_ascii : t -> t
(** Convert the given character to its equivalent lowercase character,
    using the US-ASCII character set.
    @since 0.20 *)

val uppercase_ascii : t -> t
(** Convert the given character to its equivalent uppercase character,
    using the US-ASCII character set.
    @since 0.20 *)

val of_int_exn : int -> t
(** Alias to {!Char.chr}.
    Return the character with the given ASCII code.
    @raise Invalid_argument if the int is not within [0,...,255].
    @since 1.0 *)

val of_int : int -> t option
(** Safe version of {!of_int_exn}.
    @since 1.0 *)

val to_int : t -> int
(** Alias to {!Char.code}.
    Return the ASCII code of the argument.
    @since 1.0 *)

val pp_buf : Buffer.t -> t -> unit
(** Renamed from [pp] since 2.0. *)

val pp : Format.formatter -> t -> unit
(** Renamed from [print] since 2.0. *)
