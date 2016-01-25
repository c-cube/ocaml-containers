(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Semaphores}

    @since 0.13 *)

type t
(** A semaphore *)

val create : int -> t
(** [create n] creates a semaphore with initial value [n]
    @raise Invalid_argument if [n <= 0] *)

val get : t -> int
(** Current value *)

val acquire : int -> t -> unit
(** [acquire n s] blocks until [get s >= n], then atomically
    sets [s := !s - n] *)

val release : int -> t -> unit
(** [release n s] atomically sets [s := !s + n] *)

val with_acquire : n:int -> t -> f:(unit -> 'a) -> 'a
(** [with_acquire ~n s ~f] first acquires [s] with [n] units,
    calls [f ()], and then release [s] with [n] units.
    Safely release the semaphore even if [f ()] fails *)

val wait_until_at_least : n:int -> t -> f:(unit -> 'a) -> 'a
(** [wait_until_at_least ~n s ~f] waits until [get s >= n], then calls [f ()]
    and returns its result. Doesn't modify the semaphore. *)
