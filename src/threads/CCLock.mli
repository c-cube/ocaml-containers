
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around Mutex}

    A value wrapped into a Mutex, for more safety.

    @since 0.8 *)

type 'a t
(** A value surrounded with a lock *)

val create : 'a -> 'a t
(** Create a new protected value *)

val with_lock : 'a t -> ('a -> 'b) -> 'b
(** [with_lock l f] runs [f x] where [x] is the value protected with
    the lock [l], in a critical section. If [f x] fails, [with_lock l f]
    fails too but the lock is released *)

(** Type allowing to manipulate the lock as a reference
    @since 0.13 *)
module LockRef : sig
  type 'a t

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val update : 'a t -> ('a -> 'a) -> unit
end

val with_lock_as_ref : 'a t -> f:('a LockRef.t -> 'b) -> 'b
(** [with_lock_as_ref l f] calls [f] with a reference-like object
    that allows to manipulate the value of [l] safely.
    The object passed to [f] must not escape the function call
    @since 0.13 *)

val update : 'a t -> ('a -> 'a) -> unit
(** [update l f] replaces the content [x] of [l] with [f x], atomically *)

val update_map : 'a t -> ('a -> 'a * 'b) -> 'b
(** [update_map l f] computes [x', y = f (get l)], then puts [x'] in [l]
    and returns [y]
    @since 0.16 *)

val mutex : _ t -> Mutex.t
(** Underlying mutex *)

val get : 'a t -> 'a
(** Get the value in the lock. The value that is returned isn't protected! *)

val set : 'a t -> 'a -> unit
(** Atomically set the value
    @since 0.13 *)

val incr : int t -> unit
(** Atomically increment the value
    @since 0.13 *)

val decr : int t -> unit
(** Atomically decrement the value
    @since 0.13 *)

val incr_then_get : int t -> int
(** [incr_then_get x] increments [x], and return its new value
    @since 0.16 *)

val get_then_incr : int t -> int
(** [get_then_incr x] increments [x], and return its previous value
    @since 0.16 *)

val decr_then_get : int t -> int
(** [decr_then_get x] decrements [x], and return its new value
    @since 0.16 *)

val get_then_decr : int t -> int
(** [get_then_decr x] decrements [x], and return its previous value
    @since 0.16 *)

val get_then_set : bool t -> bool
(** [get_then_set b] sets [b] to [true], and return the old value
    @since 0.16 *)

val get_then_clear : bool t -> bool
(** [get_then_clear b] sets [b] to [false], and return the old value
    @since 0.16 *)
