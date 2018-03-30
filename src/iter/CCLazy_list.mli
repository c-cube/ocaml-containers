
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Lazy List}

    @since 0.17 *)

type +'a t = 'a node lazy_t
and +'a node =
  | Nil
  | Cons of 'a * 'a t

val empty : 'a t
(** Empty list. *)

val return : 'a -> 'a t
(** Return a computed value. *)

val is_empty : _ t -> bool
(** Evaluate the head. *)

val length : _ t -> int
(** [length l] returns the number of elements in [l], eagerly (linear time).
    Caution, will not terminate if [l] is infinite. *)

val cons : 'a -> 'a t -> 'a t

val head : 'a t -> ('a * 'a t) option
(** Evaluate head, return it, or [None] if the list is empty. *)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Lazy map. *)

val filter : f:('a -> bool) -> 'a t -> 'a t
(** Filter values.
    @since 0.18 *)

val take : int -> 'a t -> 'a t
(** Take at most n values.
    @since 0.18 *)

val append : 'a t -> 'a t -> 'a t
(** Lazy concatenation. *)

val flat_map : f:('a -> 'b t) -> 'a t -> 'b t
(** Monadic flatten + map. *)

val default : default:'a t -> 'a t -> 'a t
(** Choice operator.
    @since 2.1 *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (<|>) : 'a t -> 'a t -> 'a t
  (** Alias to {!default}.
      @since 2.1 *)
end

include module type of Infix

type 'a gen = unit -> 'a option

val of_gen : 'a gen -> 'a t

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val to_list_rev  : 'a t -> 'a list

val to_gen : 'a t -> 'a gen
