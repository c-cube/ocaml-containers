
(** Thread-safe queue.

    We draw inspiration from "Implementing Lock-Free Queues", Valois 1994
*)

type 'a t

val create : dummy:'a -> unit -> 'a t
(** [create ~dummy ()] creates a new queue.
    @param dummy an element used to fill slots. It will not be released
    to the GC before the queue itself is
*)

val push : 'a t -> 'a -> unit
(** Push an element. Will not block. *)

val pop_nonblock : 'a t -> 'a option
(** pop the first element, or return [None]. *)

(** Blocking queue.

    This couples the non-blocking queue {!_ t} above,
    with mutex/condition for the blocking case.
*)
module Blocking : sig
  type 'a t

  val create : dummy:'a -> unit -> 'a t

  val push : 'a t -> 'a -> unit

  val pop_nonblock : 'a t -> 'a option

  (* FIXME *)
  val pop_block : 'a t -> 'a
end
