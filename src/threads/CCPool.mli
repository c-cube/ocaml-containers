
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Thread Pool, and Futures}

    Renamed and heavily updated from [CCFuture].
    @since 0.16 *)

type +'a state =
  | Done of 'a
  | Waiting
  | Failed of exn

module type PARAM = sig
  val max_size : int
  (** Maximum number of threads in the pool. *)
end

exception Stopped

(** {2 Create a new Pool} *)
module Make(P : PARAM) : sig
  val run : (unit -> _) -> unit
  (** [run f] schedules [f] for being executed in the thread pool. *)

  val run1 : ('a -> _) -> 'a -> unit
  (** [run1 f x] is similar to [run (fun () -> f x)]. *)

  val run2 : ('a -> 'b -> _) -> 'a -> 'b -> unit

  val run3 : ('a -> 'b -> 'c -> _) -> 'a -> 'b -> 'c -> unit

  val set_exn_handler : (exn -> unit) -> unit

  val active : unit -> bool
  (** [active ()] is true as long as [stop()] has not been called yet. *)

  val stop : unit -> unit
  (** After calling [stop ()], most functions will raise Stopped.
      This has the effect of preventing new tasks from being executed. *)

  (** {4 Futures}

      The futures are registration points for callbacks, storing a {!state},
      that are executed in the pool using {!run}. *)
  module Fut : sig
    type 'a t
    (** A future value of type ['a] *)

    type 'a future = 'a t

    (** {2 Constructors} *)

    val return : 'a -> 'a t
    (** Future that is already computed. *)

    val fail : exn -> 'a t
    (** Future that fails immediately. *)

    val make : (unit -> 'a) -> 'a t
    (** Create a future, representing a value that will be computed by
          the function. If the function raises, the future will fail. *)

    val make1 : ('a -> 'b) -> 'a -> 'b t

    val make2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t

    (** {2 Basics} *)

    val get : 'a t -> 'a
    (** Blocking get: wait for the future to be evaluated, and get the value,
        or the exception that failed the future is returned.
        Raise e if the future failed with e. *)

    val state : 'a t -> 'a state
    (** State of the future. *)

    val is_done : 'a t -> bool
    (** Is the future evaluated (success/failure)? *)

    (** {2 Combinators} *)

    val on_success : 'a t -> ('a -> unit) -> unit
    (** Attach a handler to be called upon success.
        The handler should not call functions on the future.
        Might be evaluated now if the future is already done. *)

    val on_failure : _ t -> (exn -> unit) -> unit
    (** Attach a handler to be called upon failure.
        The handler should not call any function on the future.
        Might be evaluated now if the future is already done. *)

    val on_finish : 'a t -> ('a state -> unit) -> unit
    (** Attach a handler to be called when the future is evaluated.
        The handler should not call functions on the future.
        Might be evaluated now if the future is already done. *)

    val flat_map : ('a -> 'b t) -> 'a t -> 'b t
    (** Monadic combination of futures. *)

    val and_then : 'a t -> (unit -> 'b t) -> 'b t
    (** Wait for the first future to succeed, then launch the second. *)

    val sequence_a : 'a t array -> 'a array t
    (** Future that waits for all previous futures to terminate. If any future
        in the array fails, [sequence_a l] fails too. *)

    val map_a : ('a -> 'b t) -> 'a array -> 'b array t
    (** [map_a f a] maps [f] on every element of [a], and will return
        the array of every result if all calls succeed, or an error otherwise. *)

    val sequence_l : 'a t list -> 'a list t
    (** Future that waits for all previous futures to terminate. If any future
        in the list fails, [sequence_l l] fails too. *)

    val map_l : ('a -> 'b t) -> 'a list -> 'b list t
    (** [map_l f l] maps [f] on every element of [l], and will return
        the list of every result if all calls succeed, or an error otherwise. *)

    val choose_a : 'a t array -> 'a t
    (** Choose among those futures (the first to terminate). Behaves like
        the first future that terminates, by failing if the future fails. *)

    val choose_l : 'a t list -> 'a t
    (** Choose among those futures (the first to terminate). Behaves like
        the first future that terminates, by failing if the future fails. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** Map the value inside the future. The function doesn't run in its
        own task; if it can take time, use {!flat_map} or {!map_async}. *)

    val map_async : ('a -> 'b) -> 'a t -> 'b t
    (** Map the value inside the future, to be computed in a separated job. *)

    val monoid_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Cartesian product of the content of these futures.
        @since 2.8 *)

    val app : ('a -> 'b) t -> 'a t -> 'b t
    (** [app f x] applies the result of [f] to the result of [x]. *)

    val app_async : ('a -> 'b) t -> 'a t -> 'b t
    (** [app_async f x] applies the result of [f] to the result of [x], in
        a separated job scheduled in the pool. *)

    val sleep : float -> unit t
    (** Future that returns with success in the given amount of seconds. Blocks
        the thread! If you need to wait on many events, consider
        using {!CCTimer}. *)

    module Infix : sig
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      val (>>) : 'a t -> (unit -> 'b t) -> 'b t
      val (>|=) : 'a t -> ('a -> 'b) -> 'b t
      val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

      (** Let operators on OCaml >= 4.08.0, nothing otherwise
          @since 2.8 *)
      include CCShimsMkLet_.S with type 'a t_let := 'a t
    end

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>) : 'a t -> (unit -> 'b t) -> 'b t

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (** Alias to {!map}. *)

    val (<*>): ('a -> 'b) t -> 'a t -> 'b t
    (** Alias to {!app}. *)

    (** Let operators on OCaml >= 4.08.0, nothing otherwise
        @since 2.8 *)
    include CCShimsMkLet_.S with type 'a t_let := 'a t
  end
end
