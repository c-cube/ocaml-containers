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

(** {1 Composable State Machines}

This module defines state machines that should help design applications
with a more explicit control of state (e.g. for networking applications). *)

type ('input, 'state, 'output) t = 'state -> 'input -> ('output * 'state) option
(** transition function that fully describes an automaton. It returns
    [None] to indicate that it stops. *)

type ('a, 's, 'b) automaton = ('a, 's, 'b) t

(** {2 Basic Interface} *)

val empty : ('a, 's, 'b) t
(** empty automaton, ignores state and input, stops *)

val id : ('a, unit, 'a) t
(** automaton that simply returns its inputs, forever *)

val repeat : 'a -> (unit, unit, 'a) t
(** repeat the same output forever, disregarding its inputs *)

val get_state : ('a, 's, _) t -> ('a, 's, 's) t
(** Ignore output and output state instead *)

val next : ('a, 's, 'b) t -> 's -> 'a -> ('b * 's) option
(** feed an input into the automaton, obtaining an output and
    a new state (unless the automaton has stopped) *)

val scan : ('a, 's, 'b) t -> ('a, 's * 'b list, 'b list) t
(** [scan a] accumulates all the successive outputs of [a]
    as its output *)

val lift : ('b -> 'a -> 'b) -> ('a, 'b, 'b) t
(** Lift a function into an automaton *)

val ignore_state : ('a -> 'b) -> ('a, 's, 'b) t
(** Lift a function that ignores the state into an automaton *)

val ignore_arg : ('s -> 's) -> ('a, 's, 's) t
(** Lift a function that ignores the input into an automaton *)

val map_in : ('a2 -> 'a) -> ('a, 's, 'b) t -> ('a2, 's, 'b) t

val map_out : ('b -> 'b2) -> ('a, 's, 'b) t -> ('a, 's, 'b2) t

val nest : ('a, 's, 'b) t list -> ('a, 's list, 'b list) t
(** runs all automata in parallel on the input.
    The state must be a list of the same length as the list of automata. 
    @raise Invalid_argument otherwise *)

val split : ('a, 's, 'b) t -> ('a, 's, ('b * 'b)) t
(** duplicates outputs *)

val unsplit : ('b -> 'c -> 'd) -> ('a, 's, 'b * 'c) t ->
              ('a, 's, 'd) t
(** combines the two outputs into one using the function *)

val pair : ('a1, 's1, 'b1) t -> ('a2, 's2, 'b2) t ->
           ('a1 * 'a2, 's1 * 's2, 'b1 * 'b2) t
(** pairs two automata together *)

val ( *** ) : ('a1, 's1, 'b1) t -> ('a2, 's2, 'b2) t ->
              ('a1 * 'a2, 's1 * 's2, 'b1 * 'b2) t
(** alias for {!pair} *)

val first : ('a1, 's1, 'b1) t -> (('a1 * 'keep), 's1, ('b1 * 'keep)) t

val second : ('a1, 's1, 'b1) t -> (('keep * 'a1), 's1, ('keep * 'b1)) t

val (>>>) : ('a, 's1, 'b) t -> ('b, 's2, 'c) t ->
            ('a, 's1 * 's2, 'c) t
(** composition (outputs of the first automaton are fed to
    the second one's input) *)

type ('s1,'s2) append_state =
  | Left of 's1 * 's2
  | Right of 's2

val append : ('a, 's1, 'b) t -> ('a, 's2, 'b) t ->
             ('a, ('s1, 's2) append_state, 'b) t
(** [append a b] first behaves like [a], then behaves like [a2]
    once [a1] is exhausted. *)

val flatten : ('a, ('a, 's, 'b) t list * 's, 'b) t
(** runs all automata on the input stream, one by one, until they
    stop. *)

val filter : ('b -> bool) -> ('a, 's, 'b) t -> ('a, 's, 'b option) t
(** [filter f a] yields only the outputs of [a] that satisfy [a] *)

type ('a, 'c, 's1, 's2) flat_map_state =
  ('s1 * (('a, 's2, 'c) t * 's2) option)

val flat_map : ('b -> ('a, 's2, 'c) t * 's2) -> ('a, 's1, 'b) t ->
               ('a, ('a, 'c, 's1, 's2) flat_map_state, 'c) t
(** maps outputs of the first automaton to sub-automata, that are used
    to produce outputs until they are exhausted, at which point the
    first one is used again, and so on *)

val run_list : ('a, 's, 'b) t -> init:'s -> 'a list -> 'b list
(** Run the automaton on a list of inputs *)

(** {2 Instances} *)

module Int : sig
  val range : int -> (unit, int, int) t
  (** yields all integers smaller than the argument, then stops *)
end

module List : sig
  val iter : (unit, 'a list, 'a) t
  (** iterate on the list *)

  val build : ('a, 'a list, 'a list) t
  (** build a list from its inputs *)
end

module Gen : sig
  type 'a gen = unit -> 'a option

  val map : ('a, 's, 'b) t -> 's -> 'a gen -> 'b gen
end

module Sequence : sig
  type 'a sequence = ('a -> unit) -> unit

  val map : ('a, 's, 'b) t -> 's -> 'a sequence -> 'b sequence
end

module KList : sig
  type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

  val map : ('a, 's, 'b) t -> 's -> 'a klist -> 'b klist
end

(** {2 Mutable Interface} *)

module Mut : sig
  type ('a, 's, 'b) t = {
    next : ('a, 's, 'b) automaton;
    mutable state : 's;
  } (** mutable automaton, with in-place modification *)

  val create : ('a, 's, 'b) automaton -> init:'s -> ('a, 's, 'b) t
  (** create a new mutable automaton *)

  val get_state : ('a, 's, _) t -> ('a, 's, 's) t
  (** Erases the outputs with the states *)

  val cur_state : (_, 's, _) t -> 's
  (** current state *)

  val next : ('a, 's, 'b) t -> 'a -> 'b option
  (** feed an input into the automaton, obtainin and output (unless
      the automaton has stopped) and updating the automaton's state *)

  val copy : ('a, 's, 'b) t -> ('a, 's, 'b) t
  (** copy the automaton into a new one, that can evolve independently *)

  val scan : ('a, 's, 'b) t -> ('a, 's * 'b list, 'b list) t

  val nest : ('a, 's, 'b) t list -> ('a, 's list, 'b list) t

  val append : ('a, 's1, 'b) t -> ('a, 's2, 'b) t ->
               ('a, ('s1,'s2) append_state, 'b) t

  val iter : ('a -> unit) -> (unit, _, 'a) t -> unit
  (** iterate on the given left-unit automaton *)

  module Int : sig
    val range : int -> int -> (unit, int, int) t
  end

  module List : sig
    val iter : 'a list -> (unit, 'a list, 'a) t
    (** Iterate on the given list *)

    val build : 'a list -> ('a, 'a list, 'a list) t
    (** build a list from its inputs and the initial list (prepending
        inputs to it) *)
  end
end
