
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

type queue
(** Stateful value used to store the event (pending transitions) that remain
  * to process, using an in-memory queue and processing pending tasks until
  * none remains. A default global queue is provided, see {!default_queue}. *)

val default_queue : queue
(** Default event queue *)

val create_queue : unit -> queue

val instantiate :
  ?queue:queue ->
  f:('s, 'i, 'o) t ->
  's ->
  ('s, 'i, 'o) instance
(** [instantiate ~f init] creates an instance of [f] with initial state
    [init]. 
    
    @param queue event queue used to process transitions of the automaton
    upon calls to {!send}. Default value is {!default_queue}. *)

val transition : ('s, 'i, 'o) instance -> ('s, 'i, 'o) t
(** Transition function of this instance *)

val state : ('s, _, _) instance -> 's
(** Current state of the automaton instance *)

val on_transition : ('s, 'i, 'o) instance -> ('s -> 'i -> 's * 'o list -> bool) -> unit
(** [on_state_change a k] calls [k] with the previous state, input,
    new state and ouputs of [a] every time [a] changes state.
    The callback [k] returns a boolean to signal whether it wants to continue
    being called ([true]) or stop being called ([false]). *)

val connect : (_, _, 'a) instance -> (_, 'a, _) instance -> unit
(** [connect left right] connects the ouput of [left] to the input of [right].
    Outputs of [left] will be fed to [right]. *)

val connect_map : ('a -> 'b) -> (_, _, 'a) instance -> (_, 'b, _) instance -> unit
(** [connect_map f left right] is a generalization of {!connect}, that
    applies [f] to outputs of [left] before they are sent to [right] *)

val send : (_, 'i, _) instance -> 'i -> unit
(** [send a i] uses [a]'s transition function to update [a] with the input
    event [i]. The output of the transition function (a list of outputs) is
    recursively processed.

    This may not terminate, if the automata keep on creating new outputs that
    trigger other outputs forever. *)

(** {2 Helpers} *)

val map_i : ('a -> 'b) -> ('s, 'b, 'o) t -> ('s, 'a, 'o) t
(** map inputs *)

val map_o : ('a -> 'b) -> ('s, 'i, 'a) t -> ('s, 'i, 'b) t
(** map outputs *)

val iter : ('s -> 'i -> ('s * 'o list) -> unit) -> ('s,'i,'o) instance -> unit
(** Iterate on every transition (wrapper around {!on_transition}) *)

val iter_state : ('s -> unit) -> ('s, _, _) instance -> unit

val iter_input : ('i -> unit) -> (_, 'i, _) instance -> unit

val iter_output : ('o -> unit) -> (_, _, 'o) instance -> unit
