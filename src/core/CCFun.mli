
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Functions} *)

val (|>) : 'a -> ('a -> 'b) -> 'b
(** A 'pipe' operator. [x |> f] is the same as [f x]. *)

val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition. [compose f g x] is [g (f x)]. *)

val compose_binop : ('a -> 'b) -> ('b -> 'b -> 'c) -> 'a -> 'a -> 'c
(** [compose_binop f g] is [fun x y -> g (f x) (f y)].
    Example (partial order):
      [List.sort (compose_binop fst CCInt.compare) [1, true; 2, false; 1, false]].
    @since 0.6*)

val (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Alias to [compose]. *)

val (@@) : ('a -> 'b) -> 'a -> 'b
(** [f @@ x] is the same as [f x], but right-associative.
    @since 0.5 *)

val id : 'a -> 'a
(** Identity function. *)

val const : 'a -> 'b -> 'a
(** Produce a function that just returns its first argument.
    [const x y = x] for any [y]. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** Reverse the order of arguments for a binary function. *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** Convert a function which accepts a pair of arguments into a function which accepts two arguments.
    [curry f x y] is [f (x,y)]. *)

val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Convert a function which accepts a two arguments into a function which accepts a pair of arguments.
    [uncurry f (x,y)] is  [f x y]. *)

val tap : ('a -> _) -> 'a -> 'a
(** [tap f x] evaluates [f x], discards it, then returns [x]. Useful
    in a pipeline, for instance:
    {[CCArray.(1 -- 10)
      |> tap CCArray.shuffle
      |> tap @@ CCArray.sort Pervasives.compare
    ]}
*)

val (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Mathematical composition. [(%) f g x] is [f (g x)]. *)

val lexicographic : ('a -> 'a -> int) -> ('a -> 'a -> int) -> 'a -> 'a -> int
(** Lexicographic combination of comparison functions. *)

val finally : h:(unit -> _) -> f:(unit -> 'a) -> 'a
(** [finally h f] calls [f ()] and returns its result. If it raises, the
    same exception is raised; in {b any} case, [h ()] is called after
    [f ()] terminates. *)

val finally1 : h:(unit -> _) -> ('a -> 'b) -> 'a -> 'b
(** [finally1 ~h f x] is the same as [f x], but after the computation,
    [h ()] is called whether [f x] rose an exception or not.
    @since 0.16 *)

val finally2 : h:(unit -> _) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(** [finally2 ~h f x y] is the same as [f x y], but after the computation,
    [h ()] is called whether [f x y] rose an exception or not.
    @since 0.16 *)

val opaque_identity : 'a -> 'a
(** [opaque_identity x] is like [x], but prevents Flambda from using [x]'s
    definition for optimizing it. (flambda is an optimization/inlining pass
    in OCaml >= 4.03).
    @since 0.18 *)

(** {2 Monad}

    Functions with a fixed domain are monads in their codomain. *)

module Monad(X : sig type t end) : sig
  type 'a t = X.t -> 'a
  val return : 'a -> 'a t
  (** Monadic [return]. *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic [bind]. *)
end
