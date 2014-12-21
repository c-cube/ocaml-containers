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

  val update : 'a t -> ('a -> 'a * 'b) -> 'b
    (** Use given function to atomically update content, and return a value *)

  val peek : 'a t -> 'a
    (** Look at the value, without removing it *)
end

(** {2 Signature} *)

module type S = sig
  type 'a t
    (** A future value of type 'a *)

  val run : (unit -> unit) -> unit
    (** Use the underlying thread pool to run this job *)

  val finish : unit -> unit
    (** Kill threads in the pool. The pool won't be usable any more. *)

  (** {2 Basic low-level Future functions} *)

  type 'a state =
    | NotKnown
    | Success of 'a
    | Failure of exn

  val state : 'a t -> 'a state
    (** Current state of the future *)

  val is_done : 'a t -> bool
    (** Is the future evaluated (success/failure)? *)

  (** {2 Combinators} *)

  val on_success : 'a t -> ('a -> unit) -> unit
    (** Attach a handler to be called upon success *)

  val on_failure : _ t -> (exn -> unit) -> unit
    (** Attach a handler to be called upon failure *)

  val on_finish : _ t -> (unit -> unit) -> unit
    (** Attach a handler to be called when the future is evaluated *)

  val flatMap : ('a -> 'b t) -> 'a t -> 'b t
    (** Monadic combination of futures *)

  val andThen : 'a t -> (unit -> 'b t) -> 'b t
    (** Wait for the first future to succeed, then launch the second *)

  val sequence : 'a t list -> 'a list t
    (** Future that waits for all previous sequences to terminate *)

  val choose : 'a t list -> 'a t
    (** Choose among those futures (the first to terminate) *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** Maps the value inside the future *)

  (** {2 Future constructors} *)

  val return : 'a -> 'a t
    (** Future that is already computed *)

  val spawn : (unit -> 'a) -> 'a t
    (** Spawn a thread that wraps the given computation *)

  val spawn_process : ?stdin:string -> cmd:string ->
                      (int * string * string) t
    (** Spawn a sub-process with the given command [cmd] (and possibly input);
        returns a future containing (returncode, stdout, stderr) *)

  val sleep : float -> unit t
    (** Future that returns with success in the given amount of seconds *)

  (** {2 Event timer} *)

  module Timer : sig
    val at : float -> (unit -> unit) -> unit
      (** [schedule_at ~at act] will run [act] at the Unix echo [at] *)

    val after : float -> (unit -> unit) -> unit
      (** [schedule_after ~after act] will run [act] in [after] seconds *)
  end

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>) : 'a t -> (unit -> 'b t) -> 'b t
  end

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> (unit -> 'b t) -> 'b t
end

(** {2 Functor} *)

module type CONFIG = sig
  val min_size : int  (** Minimum (initial) number of threads *)
  val max_size : int  (** Maximum number of active threads *)
end

module DefaultConfig : CONFIG

module Make(C : CONFIG) : S

(** Standard (default) pool *)
module Std : S
