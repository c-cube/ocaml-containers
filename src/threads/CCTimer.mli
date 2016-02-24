
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Event timer}

    Used to be part of [CCFuture]
    @since 0.16 *)

type t
(** A scheduler for events. It runs in its own thread. *)

val create : unit -> t
(** A new timer. *)

val set_exn_handler : t -> (exn -> unit) -> unit
(** [set_exn_handler timer f] registers [f] so that any exception
    raised by a task scheduled in [timer] is given to [f] *)

exception Stopped

val after : t -> float -> f:(unit -> _) -> unit
(** Call the callback [f] after the given number of seconds.
    @raise Stopped if the timer was stopped *)

val at : t -> float -> f:(unit -> _) -> unit
(** Create a future that evaluates to [()] at the given Unix timestamp
    @raise Stopped if the timer was stopped *)

exception ExitEvery

val every : ?delay:float -> t -> float -> f:(unit -> _) -> unit
(** [every timer n ~f] calls [f ()] every [n] seconds.
    [f()] can raise ExitEvery to stop the cycle.
    @param delay if provided, the first call to [f ()] is delayed by
      that many seconds.
    @raise Stopped if the timer was stopped *)

val stop : t -> unit
(** Stop the given timer, cancelling pending tasks. Idempotent.
    From now on, calling most other operations on the timer will raise Stopped. *)

val active : t -> bool
(** Returns [true] until [stop t] has been called. *)
