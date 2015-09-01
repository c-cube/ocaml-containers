
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Threads}

    {b status: unstable}
    @since NEXT_RELEASE *)

type t = Thread.t

val spawn : (unit -> 'a) -> t
(** [spawn f] creates a new thread that runs [f ()] *)

val detach : (unit -> 'a) -> unit
(** [detach f] is the same as [ignore (spawn f)] *)

(** {2 Array of threads} *)
module Arr : sig
  val spawn : int -> (int -> 'a) -> t array
  (** [A.spawn n f] creates an array [res] of length [n], such that
      [res.(i) = spawn (fun () -> f i)] *)

  val join : t array -> unit
  (** [A.join a] joins every thread in [a] *)
end

(** {2 Single-Use Barrier} *)

module Barrier : sig
  type t
  (** Barrier, used to synchronize threads *)

  val create : unit -> t
  (** Create a barrier *)

  val reset : t -> unit
  (** Reset to initial (non-triggered) state *)

  val wait : t -> unit
  (** [wait b] waits for barrier [b] to be activated by [activate b].
      All threads calling this wait until [activate b] is called.
      If [b] is already activated, [wait b] does nothing *)

  val activate : t -> unit
  (** [activate b] unblocks all threads that were waiting on [b] *)

  val activated : t -> bool
  (** [activated b] returns [true] iff [activate b] was called, and [reset b]
      was not called since. In other words, [activated b = true] means
      [wait b] will not block. *)
end

(** {2 Blocking Queue}

    This queue has a limited size. Pushing a value on the queue when it
    is full will block *)
module Queue : sig
  type 'a t
  (** Safe-thread queue for values of type ['a] *)

  val create : int -> 'a t
  (** Create a new queue of size [n]. Using [n=max_int] amounts to using
      an infinite queue (2^61 items is a lot to fit in memory).
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
end

