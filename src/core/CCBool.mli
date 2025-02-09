(* This file is free software, part of containers. See file "license" for more details. *)

(** Basic Bool functions *)

include module type of Bool
(** @inline *)

val if_then : (unit -> 'a) -> t -> 'a option
(** [if_then f x] is [Some (f ())] if [x] is true and None otherwise.
    @since 3.13 *)

val if_then_else : (unit -> 'a) -> (unit -> 'a) -> t -> 'a
(** [if_then_else f g x] is [f ()] if [x] is true and [g ()] otherwise.
    @since 3.13 *)

val of_int : int -> t
(** [of_int i] is the same as [i <> 0]
    @since 2.7 *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : t printer
