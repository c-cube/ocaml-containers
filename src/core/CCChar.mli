(* This file is free software, part of containers. See file "license" for more details. *)

(** Utils around char

    @since 0.14 *)

(** @inline *)
include module type of struct
  include Char
end

val compare : t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val of_int_exn : int -> t
(** Alias to {!Char.chr}.
    Return the character with the given ASCII code.
    @raise Invalid_argument if the int is not within [0 … 255].
    @since 1.0 *)

val of_int : int -> t option
(** Safe version of {!of_int_exn}.
    @since 1.0 *)

val to_int : t -> int
(** Alias to {!Char.code}.
    Return the ASCII code of the argument.
    @since 1.0 *)

val to_string : t -> string
(** [to_string c] returns a string containing [c]
    @since 2.7 *)

val pp_buf : Buffer.t -> t -> unit
(** Renamed from [pp] since 2.0. *)

val pp : Format.formatter -> t -> unit
(** Renamed from [print] since 2.0. *)

val is_uppercase_ascii : t -> bool
(** [is_uppercase_ascii c] is true exactly when  [c] is an
    uppercase ASCII character, i.e. ['\064'] < [c] < ['\091'].
    @since 3.16 *)

val is_lowercase_ascii : t -> bool
(** [is_lowercase_ascii c] is true exactly when [c] is a
    lowercase ASCII character, i.e. ['\096'] < [c] < ['\123'].
    @since 3.16 *)

val is_letter_ascii : t -> bool
(** [is_letter_ascii c] is true exactly when [c] is an ASCII
    letter, i.e. [is_uppercase_ascii c || is_lowercase_ascii c].
    @since 3.16 *)

val is_digit_ascii : t -> bool
(** [is_digit_ascii c] is true exactly when [c] is an
    ASCII digit, i.e. ['\047'] < [c] < ['\058'].
    @since 3.16 *)

val is_whitespace_ascii : t -> bool
(** [is_whitespace_ascii c] is true exactly when [c] is an ASCII
    whitespace character as defined by Unicode, i.e. either [c = ' ']
    or ['\008'] < [c] < ['\014'].
    @since 3.16 *)

(** {2 Infix Operators}

    @since 3.3 *)

module Infix : sig
  val ( = ) : t -> t -> bool
  (** @since 3.3 *)

  val ( <> ) : t -> t -> bool
  (** @since 3.3 *)

  val ( < ) : t -> t -> bool
  (** @since 3.3 *)

  val ( > ) : t -> t -> bool
  (** @since 3.3 *)

  val ( <= ) : t -> t -> bool
  (** @since 3.3 *)

  val ( >= ) : t -> t -> bool
  (** @since 3.3 *)
end

include module type of Infix
