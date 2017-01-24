
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Blocking Queue}

    This queue has a limited size. Pushing a value on the queue when it
    is full will block.

    @since 0.16 *)

type 'a t
(** Safe-thread queue for values of type ['a] *)

val create : int -> 'a t
(** Create a new queue of size [n]. Using [n=max_int] amounts to using
    an infinite queue (2^61 items is a lot to fit in memory); using [n=1]
    amounts to using a box with 0 or 1 elements inside.
    @raise Invalid_argument if [n < 1] *)

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] into [q], blocking if the queue is full *)

val take : 'a t -> 'a
(** Take the first element, blocking if needed *)

val push_list : 'a t -> 'a list -> unit
(** Push items of the list, one by one *)

val take_list : 'a t -> int -> 'a list
(** [take_list n q] takes [n] elements out of [q] *)

val try_take : 'a t -> 'a option
(** Take the first element if the queue is not empty, return [None]
    otherwise *)

val try_push : 'a t -> 'a -> bool
(** [try_push q x] pushes [x] into [q] if [q] is not full, in which
    case it returns [true].
    If it fails because [q] is full, it returns [false] *)

val peek : 'a t -> 'a option
(** [peek q] returns [Some x] if [x] is the first element of [q],
    otherwise it returns [None] *)

val size : _ t -> int
(** Number of elements currently in the queue *)

val capacity : _ t -> int
(** Number of values the queue can hold *)

