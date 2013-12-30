
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

(** {1 Distributed Algorithms} *)

type ('s, -'i, +'o) t = 's -> 'i -> 's * 'o list
(** Transition function of an event automaton *)

type ('s, 'i, 'o) instance
(** Instance of an automaton, with a concrete state, and connections to other
    automaton instances. *)

module EventQueue : sig
  type t
  (** Stateful value used to store the event (pending transitions) that remain
    * to process, using an in-memory queue and processing pending tasks until
    * none remains. A default global queue is provided, see {!default}. *)

  val default : t
  (** Default event queue *)

  val create : unit -> t
end

val instantiate : ?queue:EventQueue.t ->
                  f:('s, 'i, 'o) t -> 's -> ('s, 'i, 'o) instance
(** [instantiate ~f init] creates an instance of [f] with initial state
    [init]. The [queue] is used to process transitions of this automaton.
    
    @param queue event queue used to process transitions of the automaton
    upon calls to {!send}. Default value is {!EventQueue.default}. *)

val transition : ('s, 'i, 'o) instance -> 'i -> ('s * 'o list)
(** Compute the transition function for the given input *)

val state : ('s, _, _) instance -> 's
(** Current state of the automaton instance *)

val on_transition : ('s, 'i, 'o) instance -> ('s -> 'i -> 's * 'o list -> bool) -> unit
(** [on_state_change a k] calls [k] with the previous state, input,
    new state and ouputs of [a] every time [a] changes state.
    The callback [k] returns a boolean to signal whether it wants to continue
    being called ([true]) or stop being called ([false]). *)

val connect : left:(_, _, 'a) instance -> right:(_, 'a, _) instance -> unit
(** [connect ~left ~right] connects the ouput of [left] to the input of [right].
    Outputs of [left] will be fed to [right]. *)

val send : (_, 'i, _) instance -> 'i -> unit
(** [send a i] uses [a]'s transition function to update [a] with the input
    event [i]. The output of the transition function (a list of outputs) is
    recursively processed.

    This may not terminate, if the automata keep on creating new outputs that
    trigger other outputs forever. *)
