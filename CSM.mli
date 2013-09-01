(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 Composable State Machines} *)

(** This module defines state machines that should help design applications
    with a more explicit control of state (e.g. for networking applications.
    It is {b not} thread-safe.
*)

(** {2 Basic interface} *)

type 'state t
  (** State machine, whose states are of the type 'state,
      and that changes state upon events of the type 'event. *)

type 'a transition =
  | TransitionTo of 'a
  | TransitionStay
  (** A transition of a state machine whose states are
      of type 'a *)

val create : ?root:bool ->
             init:'state ->
             trans:('state -> 'event -> 'state transition) ->
            'state t * ('event -> unit)
  (** Creation of a state machine with an initial state and a
      given transition function. [root] specifies whether the FSM should
      be a GC root and stay alive (default false).
      This creates both a state machine and a way to send
      events to it. *)

val state : 'state t -> 'state
  (** Current state of a machine *)

val id : _ t -> int
  (** Unique ID of a state machine *)

val eq : _ t -> _ t -> bool

val hash : _ t -> int

val compare : _ t -> _ t -> int

val register_while : 'state t -> ('state -> 'state -> bool) -> unit
  (** The given callback will be called upon every state change of
      the given state machine with both the old and the new states,
      while it returns [true]. When it returns [false], the
       callback will no longer be referenced nor called.
  *)

val register : 'state t -> ('state -> 'state -> unit) -> unit
  (** Register the given callback forever. *)

val connect : 'a t -> ('a -> unit) -> unit
  (** [connect st sink] connects state changes of [st] to the sink. The
      sink is given only the new state of [st]. *)

(** {2 Combinators} *)

val map : 'a t -> ('a -> 'b) -> 'b t
  (** Map the states from the given state machine to new states. *)

val filter : 'a t -> ('a -> bool) -> 'a t
  (** [filter st p] behaves like [st], but only keeps transitions
      {b to} states that satisfy the given predicate. *)

val seq_list : 'state t list -> 'state list t
  (** Aggregate of the states of several machines *)

(** {2 GC behavior} *)

val make_root : _ t -> unit
  (** Make the given state machine alive w.r.t. the GC. It will not be
      collected *)

val remove_root : _ t -> unit
  (** The given state machine is no longer a GC root. *)

(** {2 Unix wrappers} *)

module Unix : sig
  type fd_state =
    | FD_wait of Unix.file_descr
    | FD_ready_read of Unix.file_descr
    | FD_ready_write of Unix.file_descr
    | FD_exc_condition of Unix.file_descr

  val select : Unix.file_descr list ->
               Unix.file_descr list ->
               Unix.file_descr list ->
               float ->
               (fd_state list * fd_state list * fd_state list) t
    (** Wrapper for {! Unix.select} as a state machine. *)

  val run : unit -> unit
    (** Main function, doesn't return. It waits for unix events,
        runs state machines until everything has been processed, and
        waits for unix events again. *)
end
