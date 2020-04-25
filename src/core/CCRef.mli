
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 References}
    @since 0.9 *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a ord = 'a -> 'a -> int
type 'a eq = 'a -> 'a -> bool
type 'a iter = ('a -> unit) -> unit

type 'a t = 'a ref

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform the value. *)

val create : 'a -> 'a t
(** Alias to {!ref}. *)

val iter : ('a -> unit) -> 'a t -> unit
(** Call the function on the content of the reference. *)

val update : ('a -> 'a) -> 'a t -> unit
(** Update the reference's content with the given function. *)

val incr_then_get : int t -> int
(** [incr_then_get r] increments [r] and returns its new value, think [++r].
    @since 0.17 *)

val get_then_incr : int t -> int
(** [get_then_incr r] increments [r] and returns its old value, think [r++].
    @since 0.17 *)

val swap : 'a t -> 'a t -> unit
(** [swap t1 t2] puts [!t2] in [t1] and [!t1] in [t2].
    @since 1.4 *)

val compare : 'a ord -> 'a t ord

val equal : 'a eq -> 'a t eq

val to_list : 'a t -> 'a list

val to_iter : 'a t -> 'a iter
(** @since 3.0 *)

val pp : 'a printer -> 'a t printer
