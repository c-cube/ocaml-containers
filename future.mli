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

type 'a t
  (** A future value of type 'a *)

exception SendTwice
  (** Exception raised when a future is evaluated several time *)

(** {2 MVar: a zero-or-one element thread-safe box} *)

module MVar : sig
  type 'a t

  val empty : unit -> 'a t
    (** Create an empty box *)

  val full : 'a -> 'a t
    (** Create a full box *)

  val is_empty : _ t -> bool
    (** Is the box currently empty? *)

  val take : 'a t -> 'a
    (** Take value out of the box. Wait if necessary *)

  val put : 'a t -> 'a -> unit
    (** Put a value in the box. Waits if the box is already empty *)

  val update : 'a t -> ('a -> 'a) -> 'a * 'a
    (** Use given function to atomically update content, and return
        the previous value and the new one *)

  val peek : 'a t -> 'a
    (** Look at the value, without removing it *)
end

(** {2 Thread pool} *)
module Pool : sig
  type t
    (** A pool of threads *)

  val create : ?timeout:float -> size:int -> t
    (** Create a pool with at most the given number of threads. [timeout]
        is the time after which idle threads are killed. *)

  val size : t -> int
    (** Current size of the pool *)

  val run : t -> (unit -> unit) -> unit
    (** Run the function in the pool *)

  val finish : t -> unit
    (** Kill threads in the pool *)
end

val default_pool : Pool.t
  (** Pool of threads that is used by default. Growable if needed. *)

(** {2 Basic low-level Future functions} *)

val make : Pool.t -> 'a t
  (** Create a future, representing a value that is not known yet. *)

val get : 'a t -> 'a
  (** Blocking get: wait for the future to be evaluated, and get the value,
      or the exception that failed the future is returned *)

val send : 'a t -> 'a -> unit
  (** Send a result to the future. Will raise SendTwice if [send] has
      already been called on this future before *)

val fail : 'a t -> exn -> unit
  (** Fail the future by raising an exception inside it *)

val is_done : 'a t -> bool
  (** Is the future evaluated (success/failure)? *)

(** {2 Combinators *)

val on_success : 'a t -> ('a -> unit) -> unit
  (** Attach a handler to be called upon success *)

val on_failure : _ t -> (exn -> unit) -> unit
  (** Attach a handler to be called upon failure *)

val on_finish : _ t -> (unit -> unit) -> unit
  (** Attach a handler to be called when the future is evaluated *)

val flatMap : ?pool:Pool.t -> ('a -> 'b t) -> 'a t -> 'b t
  (** Monadic combination of futures *)

val andThen : ?pool:Pool.t -> 'a t -> (unit -> 'b t) -> 'b t
  (** Wait for the first future to succeed, then launch the second *)

val sequence : ?pool:Pool.t -> 'a t list -> 'a list t
  (** Future that waits for all previous sequences to terminate *)

val choose : ?pool:Pool.t -> 'a t list -> 'a t
  (** Choose among those futures (the first to terminate) *)

val map : ?pool:Pool.t -> ('a -> 'b) -> 'a t -> 'b t
  (** Maps the value inside the future *)

(** {2 Future constructors} *)

val return : 'a -> 'a t
  (** Future that is already computed *)

val spawn : ?pool:Pool.t -> (unit -> 'a) -> 'a t
  (** Spawn a thread that wraps the given computation *)

val spawn_process : ?pool:Pool.t -> ?stdin:string -> cmd:string ->
                    (int * string * string) t
  (** Spawn a sub-process with the given command [cmd] (and possibly input);
      returns a future containing (returncode, stdout, stderr) *)

val sleep : ?pool:Pool.t -> float -> unit t
  (** Future that returns with success in the given amount of seconds *)

(** {2 Event timer} *)

module Timer : sig
  type t
    (** A scheduler for events *)

  val create : ?pool:Pool.t -> unit -> t
    (** A timer that runs tasks in the given thread pool *)

  val schedule_at : t -> float -> (unit -> unit) -> unit
    (** [schedule_at s t act] will run [act] at the Unix echo [t] *)

  val schedule_in : t -> float -> (unit -> unit) -> unit
    (** [schedule_in s d act] will run [act] in [d] seconds *)

  val stop : t -> unit
    (** Stop the given timer, cancelling pending tasks *)
end


module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> (unit -> 'b t) -> 'b t
end

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>>) : 'a t -> (unit -> 'b t) -> 'b t
