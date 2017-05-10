
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional queues (fifo)} *)

(** Simple implementation of functional queues
    @since NEXT_RELEASE *)

type +'a t
(** Queue containing elements of type 'a *)

val empty : 'a t

val is_empty : 'a t -> bool

val push : 'a -> 'a t -> 'a t
(** Push element at the end of the queue *)

val snoc : 'a t -> 'a -> 'a t
(** Flip version of {!push} *)

val peek : 'a t -> 'a option
(** First element of the queue *)

val peek_exn : 'a t -> 'a
(** Same as {!peek} but
    @raise Invalid_argument if the queue is empty *)

val pop : 'a t -> ('a * 'a t) option
(** Get and remove the first element *)

val pop_exn : 'a t -> ('a * 'a t)
(** Same as {!pop}, but fails on empty queues.
    @raise Invalid_argument if the queue is empty *)

val junk : 'a t -> 'a t
(** Remove first element. If the queue is empty, do nothing. *)

val append : 'a t -> 'a t -> 'a t
(** Append two queues. Elements from the second one come
    after elements of the first one.
    Linear in the size of the second queue. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map values *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val size : 'a t -> int
(** Number of elements in the queue (linear in time) *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val iter : ('a -> unit) -> 'a t -> unit

type 'a sequence = ('a -> unit) -> unit
val to_seq : 'a t -> 'a sequence
val of_seq : 'a sequence -> 'a t

