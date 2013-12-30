
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

(** {1 Automaton} *)

type ('s, -'i, +'o) t = 's -> 'i -> 's * 'o list
(** Transition function of an event automaton *)

type ('s, 'i, 'o) automaton = ('s, 'i, 'o) t

(** {2 Combinators} *)

val map_i : ('a -> 'b) -> ('s, 'b, 'o) t -> ('s, 'a, 'o) t
(** map inputs *)

val map_o : ('a -> 'b) -> ('s, 'i, 'a) t -> ('s, 'i, 'b) t
(** map outputs *)

val fmap_o : ('a -> 'b list) -> ('s, 'i, 'a) t -> ('s, 'i, 'b) t
(** flat-map outputs *)

val filter_i : ('a -> bool) -> ('s, 'a, 'o) t -> ('s, 'a, 'o) t
(** Filter inputs *)

val filter_o : ('a -> bool) -> ('s, 'i, 'a) t -> ('s, 'i, 'a) t
(** Filter outputs *)

val fold : ('a -> 'b -> 'a) -> ('a, 'b, 'a) t
(** Automaton that folds over its input using the given function *)

val product : ('s1, 'i, 'o) t -> ('s2, 'i, 'o) t -> ('s1 * 's2, 'i, 'o) t
(** Product of transition functions and states. *)

(** {2 Input}

Input sink, that accepts values of a given type. Cofunctor. *)

module I : sig
  type -'a t

  val create : ('a -> unit) -> 'a t

  val comap : ('a -> 'b) -> 'b t -> 'a t

  val filter : ('a -> bool) -> 'a t -> 'a t

  val send : 'a t -> 'a -> unit
  (** [send a i] inputs [i] on the channel [a]. *)
end

(** {2 Output}

Stream of output values. Functor. *)

module O : sig
  type 'a t

  val create : unit -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val filter : ('a -> bool) -> 'a t -> 'a t

  val on : 'a t -> ('a -> bool) -> unit

  val once : 'a t -> ('a -> unit) -> unit

  val send : 'a t -> 'a -> unit

  val propagate : 'a t -> 'a t -> unit
  (** [propagate a b] forwards all elements of [a] into [b]. As long as [a]
      exists, [b] will not be GC'ed. *)
end

val connect : 'a O.t -> 'a I.t -> unit
  (** Pipe an output into an input *)

(** {2 Instance} *)

module Instance : sig
  type ('s, 'i, 'o) t
  (** Instance of an automaton, with a concrete state, and connections to other
      automaton instances. *)

  val transition_function : ('s, 'i, 'o) t -> ('s, 'i, 'o) automaton
  (** Transition function of this instance *)

  val i : (_, 'a, _) t -> 'a I.t

  val o : (_, _, 'a) t -> 'a O.t

  val state : ('a, _, _) t -> 'a

  val transitions : ('s, 'i, 'o) t -> ('s * 'i * 's * 'o list) O.t

  val send : (_, 'i, _) t -> 'i -> unit
  (** Shortcut to send an input *)

  val create : f:('s, 'i, 'o) automaton -> 's -> ('s, 'i, 'o) t
  (** [create ~f init] creates an instance of [f] with initial state
      [init].
      
      @param f the transition function
      @param init the initial state *)
end
