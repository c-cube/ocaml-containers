(* This file is free software, part of containers. See file "license" for more details. *)

(** Basic Bool functions *)

type t = bool

val compare : t -> t -> int
(** [compare b1 b2] is the total ordering on booleans [b1] and [b2], similar to {!Stdlib.compare}. *)

val equal : t -> t -> bool
(** [equal b1 b2] is [true] if [b1] and [b2] are the same. *)

val if_then : (unit -> 'a) -> t -> 'a option
(** [if_then f x] is [Some (f ())] if [x] is true and None otherwise.
    @since 3.13 *)

val if_then_else : (unit -> 'a) -> (unit -> 'a) -> t -> 'a
(** [if_then_else f g x] is [f ()] if [x] is true and [g ()] otherwise.
    @since 3.13 *)

val to_int : t -> int
(** [to_int true = 1], [to_int false = 0].
    @since 2.7 *)

val of_int : int -> t
(** [of_int i] is the same as [i <> 0]
    @since 2.7 *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : t printer
