(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Futures for concurrency} *)

type 'a state =
  | Done of 'a
  | Waiting
  | Failed of exn

type 'a t
(** A future value of type 'a *)

type 'a future = 'a t

(** {2 Constructors} *)

val return : 'a -> 'a t
(** Future that is already computed *)

val fail : exn -> 'a t
(** Future that fails immediately *)

val make : (unit -> 'a) -> 'a t
(** Create a future, representing a value that will be computed by
      the function. If the function raises, the future will fail. *)

val make1 : ('a -> 'b) -> 'a -> 'b t
val make2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t

(** {2 Basics} *)

val get : 'a t -> 'a
(** Blocking get: wait for the future to be evaluated, and get the value,
    or the exception that failed the future is returned.
    @raise e if the exception failed with e *)

val state : 'a t -> 'a state
(** State of the future *)

val is_done : 'a t -> bool
(** Is the future evaluated (success/failure)? *)

(** {2 Combinators} *)

val on_success : 'a t -> ('a -> unit) -> unit
(** Attach a handler to be called upon success *)

val on_failure : _ t -> (exn -> unit) -> unit
(** Attach a handler to be called upon failure *)

val on_finish : 'a t -> ('a state -> unit) -> unit
(** Attach a handler to be called when the future is evaluated *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Monadic combination of futures *)

val and_then : 'a t -> (unit -> 'b t) -> 'b t
(** Wait for the first future to succeed, then launch the second *)

val sequence : 'a t list -> 'a list t
(** Future that waits for all previous sequences to terminate. If any future
    in the list fails, [sequence l] fails too. *)

val choose : 'a t list -> 'a t
(** Choose among those futures (the first to terminate). Behaves like
    the first future that terminates, by failing if the future fails *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Maps the value inside the future. The function doesn't run in its
    own task; if it can take time, use {!flat_map} *)

(** {2 Helpers} *)

val spawn_process : ?stdin:string -> cmd:string -> unit ->
                    (int * string * string) t
(** Spawn a sub-process with the given command [cmd] (and possibly input);
      returns a future containing (returncode, stdout, stderr) *)

val sleep : float -> unit t
(** Future that returns with success in the given amount of seconds *)

(** {2 Event timer} *)

module Timer : sig
  type t
  (** A scheduler for events. It runs in its own thread. *)

  val create : unit -> t
  (** A new timer. *)

  val after : t -> float -> unit future
  (** Create a future that waits for the given number of seconds, then
      awakens with [()] *)

  val at : t -> float -> unit future
  (** Create a future that evaluates to [()] at the given Unix timestamp *)

  val stop : t -> unit
  (** Stop the given timer, cancelling pending tasks *)
end

module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> (unit -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
end

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>>) : 'a t -> (unit -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t

(** {2 Low level} *)

val stop_pool : unit -> unit
(** Stop the thread pool *)
