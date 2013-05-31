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

(** {1 Behavior Trees for React} *)

(** Behavior trees are a modular alternative to state machines for controlling
    dynamic behavior in time. They are primarily used in video games to
    implement non-player AI.

    A tree is composed of basic actions, basic tests, and combinators. During
    execution, some subset of the nodes of a tree may be {b running}; at some
    point the execution of a given node will terminate with either
    {b success} or {b failure}. Depending on the kind of node, this result
    may propagate to parent nodes, or set other nodes running.

    For instance, a {i sequence} node runs its subtrees one by one. If a
    subtree succeeds, the next one is activated; if it fails, the whole
    sequence will fail.
    
    Here, we build them on top of
    {{: http://erratique.ch/software/react/doc/React.html} React}.

    Documentation source:
    {{: http://aigamedev.com/open/article/bt-overview/} aigamedev (and links)}
*)

(** {2 Behavior tree} *)

type tree = private
  | Test of bool React.event                (* test the next occurrence *)
  | TestFun of (unit -> bool)               (* call and test value *)
  | Wait of unit React.event                (* wait for the event to trigger *)
  | Timeout of float                        (* fails after the given timeout *)
  | Do of (unit -> bool)                    (* perform an action *)
  | If of bool React.signal * tree * tree   (* switch *)
  | Sequence of bool * tree list            (* yield to subtrees sequentially. bool: loop? *)
  | Select of select_strategy * tree list   (* select one subtree *)
  | Parallel of parallel_strategy * tree list   (* run all subtrees in parallel *)
  | Closure of (unit -> tree)               (* build a tree dynamically *)
  | Succeed
  | Fail
  (** A behavior tree *)
and select_strategy = tree list -> (unit -> tree option)
  (** How to select a subtree to run. It may yield a different result each
      time it is called. *)
and parallel_strategy =
  | PSForall  (** succeeds when all subtrees succeed *)
  | PSExists  (** succeeds when some subtree succeeds (kill the others) *)

val strategy_inorder : select_strategy
  (** Select subnodes one after the other, then fails *)

val strategy_random : ?proba_fail:float -> select_strategy
  (** Randomly chooses a subtree. May fail at each point with
      a probability of [proba_fail]. *)

val succeed : tree
  (** Behavior that always succeeds *)

val fail : tree
  (** Behavior that always fails *)

val test : bool React.event -> tree
  (** Fails or succeeds based on the next occurrence of the event *)

val test_fun : (unit -> bool) -> tree
  (** Tests that the result of calling this function is true *)

val test_signal : bool React.signal -> tree
  (** Fails or succeeds based on the current signal value *)

val wait : unit React.event -> tree
  (** Wait for the event to trigger, then succeed *)

val timeout : float -> tree
  (** Fails after the given amount of seconds *)

val delay : float -> tree
  (** Wait for the given amount of seconds, then succeed *)

val do_ : (unit -> bool) -> tree
  (** Perform an action, then succeed iff it returned true *)

val do_succeed : (unit -> unit) -> tree
  (** Perform an action and succeed (unless it raises an exception) *)

val if_ : bool React.signal -> tree -> tree -> tree
  (** Conditional choice, based on the current value of the signal *)

val sequence : ?loop:bool -> tree list -> tree
  (** Sequence of sub-trees to run *)

val select : ?strat:select_strategy -> tree list -> tree
  (** Choice among the subtrees. The strategy defines in which order subtrees
      are tried. *)

val or_else : tree -> tree -> tree
  (** Binary choice, favoring the left one *)

val parallel : ?strat:parallel_strategy -> tree list -> tree
  (** Run subtrees in parallel (default strat: PSForall) *)

val closure : (unit -> tree) -> tree
  (** Produce a tree dynamically, at each call. *)

(** {2 Lightweight futures} *)

module Fut : sig
  type 'a t
    (** Future value of type 'a *)

  val create : unit -> 'a t * ('a -> unit)
    (** Create a future, and a function that sets its value (if already set,
        will raise Invalid_argument) *)

  val subscribe : 'a t -> ('a -> unit) -> unit
    (** Get notified exactly once with the value (maybe right now) *)

  val is_set : 'a t -> bool
    (** Value already known? *)

  val return : 'a -> 'a t
    (** Monadic return (returns immediately) *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
    (** Monadic bind *)

  val next : 'a React.event -> 'a t
    (** Next occurrence of the event *)

  val wait : 'a t -> 'a option React.signal
    (** The value of the future (None while it's not set) *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** Simple map *)

  val first : 'a t list -> 'a t
    (** First future of the list to be set (or any that is already
        set if at least one is set) *)

  val last : 'a t list -> 'a t
    (** Last future to be set (or any if they are all already set) *)

  val filter : ('a -> bool) -> 'a t list -> 'a option t
    (** Filters out results that do not satisfy the predicate; returns the
        first result that satisfy it, or None *)

  val l2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Binary lift *)
end

(** {2 Run a tree} *)

type result = bool Fut.t

val run : ?delay:(float -> unit React.event) ->
          tree ->
          result
  (** Run the tree. It returns a {! result}, which wraps
      either true (success) or false (failure).
      
      [delay] is the function to call to get notified after the given amount of 
      seconds elapsed. *)
