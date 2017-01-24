(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Threads}

    {b status: unstable}
    @since 0.13 *)

type t = Thread.t

val spawn : (unit -> _) -> t
(** [spawn f] creates a new thread that runs [f ()] *)

val spawn1 : ('a -> _) -> 'a -> t
(** [spawn1 f x] is like [spawn (fun () -> f x)].
    @since 0.16 *)

val spawn2 : ('a -> 'b -> _) -> 'a -> 'b -> t
(** [spawn2 f x y] is like [spawn (fun () -> f x y)].
    @since 0.16 *)

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
