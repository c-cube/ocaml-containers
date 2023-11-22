(* This file is free software, part of containers. See file "license" for more details. *)

(** Basic Bool functions *)

include module type of Bool
(** @inline *)

val of_int : int -> t
(** [of_int i] is the same as [i <> 0]
    @since 2.7 *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : t printer
