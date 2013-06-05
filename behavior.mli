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

(** {1 Behavior Trees for Lwt} *)

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
    {{: http://ocsigen.org/lwt/} Lwt}.

    Documentation source:
    {{: http://aigamedev.com/open/article/bt-overview/} aigamedev (and links)}
*)

(** {2 Behavior tree} *)

(** A behavior tree *)
type tree = private
  | Test of (unit -> bool)                  (* call and test value *)
  | Wait of (unit -> bool Lwt.t)            (* wait for the future to complete *)
  | Do of (unit -> bool)                    (* perform an action *)
  | If of (unit -> bool) * tree * tree      (* switch *)
  | Sequence of bool * tree list            (* yield to subtrees sequentially. bool: loop? *)
  | Select of select_strategy * tree list   (* select one subtree *)
  | Parallel of parallel_strategy * tree list   (* run all subtrees in parallel *)
  | Closure of (unit -> tree)               (* build a tree dynamically *)
  | Succeed                                 (* always succeed *)
  | Fail                                    (* always fail *)

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

val test : (unit -> bool) -> tree
  (** Fails or succeeds based on the next occurrence of the event *)

val wait : bool Lwt.t -> tree
  (** Returns the same result as the future *)

val wait_ : unit Lwt.t -> tree
  (** Wait for the future to complete, then succeed *)

val wait_closure : (unit -> bool Lwt.t) -> tree

val timeout : float -> tree
  (** Fails after the given amount of seconds *)

val delay : float -> tree
  (** Wait for the given amount of seconds, then succeed *)

val do_ : (unit -> bool) -> tree
  (** Perform an action, then succeed iff it returned true *)

val do_succeed : (unit -> unit) -> tree
  (** Perform an action and succeed (unless it raises an exception) *)

val if_ : (unit -> bool) -> tree -> tree -> tree
  (** Conditional choice, based on the current value of the signal *)

val when_ : (unit -> bool) -> tree -> tree
  (** Run the given tree if the signal is true, else succeed *)

val while_ : (unit -> bool) -> tree list -> tree
  (** While the signal is true, run the subtrees *)

val sequence : ?loop:bool -> tree list -> tree
  (** Sequence of sub-trees to run *)

val repeat : tree -> tree
  (** Repeat the same tree indefinitely *)

val select : ?strat:select_strategy -> tree list -> tree
  (** Choice among the subtrees. The strategy defines in which order subtrees
      are tried. *)

val or_else : tree -> tree -> tree
  (** Binary choice, favoring the left one *)

val parallel : ?strat:parallel_strategy -> tree list -> tree
  (** Run subtrees in parallel (default strat: PSForall) *)

val closure : (unit -> tree) -> tree
  (** Produce a tree dynamically, at each call. *)

(** {2 Run a tree} *)

type result = bool Lwt.t

val run : tree -> result
  (** Run the tree. It returns a {! result}, which wraps
      either true (success) or false (failure). *)
