(* This file is free software, part of containers. See file "license" for more details. *)

(** Basic operations on Functions *)

include module type of Fun
(** @inline *)

val and_pred : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [and_p f g x] is [(f x) && (g x)].
    Produces a predicate which is a conjunction of the two predicates.
    @since 3.13.1
*)

val or_pred : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [or_p f g x] is [(f x) || (g x)].
    Produces a predicate which is a disjunction of the two predicates.
    @since 3.13.1
*)

val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [compose f g x] is [g (f x)]. Composition. *)

val compose_binop : ('a -> 'b) -> ('b -> 'b -> 'c) -> 'a -> 'a -> 'c
(** [compose_binop f g] is [fun x y -> g (f x) (f y)].
    Example (partial order):
      [List.sort (compose_binop fst CCInt.compare) [1, true; 2, false; 1, false]].
    @since 0.6*)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** [curry f x y] is [f (x,y)].
    Convert a function which accepts a pair of arguments into a function which accepts two arguments. *)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** [uncurry f (x,y)] is  [f x y].
    Convert a function which accepts a two arguments into a function which accepts a pair of arguments. *)

val tap : ('a -> _) -> 'a -> 'a
(** [tap f x] evaluates [f x], discards it, then returns [x]. Useful
    in a pipeline, for instance:
    {[CCArray.(1 -- 10)
      |> tap CCArray.shuffle
      |> tap @@ CCArray.sort Stdlib.compare
    ]}
*)

val lexicographic : ('a -> 'a -> int) -> ('a -> 'a -> int) -> 'a -> 'a -> int
(** Lexicographic combination of comparison functions. *)

val finally : h:(unit -> _) -> f:(unit -> 'a) -> 'a
(** [finally ~h f] calls [f ()] and returns its result. If it raises, the
    same exception is raised; in {b any} case, [h ()] is called after
    [f ()] terminates.
    If [h ()] raises an exception, then this exception will be passed on and
    any exception that may have been raised by [f ()] is lost. *)

val finally1 : h:(unit -> _) -> ('a -> 'b) -> 'a -> 'b
(** [finally1 ~h f x] is the same as [f x], but after the computation,
    [h ()] is called whether [f x] rose an exception or not.
    If [h ()] raises an exception, then this exception will be passed on and
    any exception that may have been raised by [f ()] is lost.
    @since 0.16 *)

val finally2 : h:(unit -> _) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(** [finally2 ~h f x y] is the same as [f x y], but after the computation,
    [h ()] is called whether [f x y] rose an exception or not.
    If [h ()] raises an exception, then this exception will be passed on and
    any exception that may have been raised by [f ()] is lost.
    @since 0.16 *)

val opaque_identity : 'a -> 'a
(** [opaque_identity x] is like [x], but prevents Flambda from using [x]'s
    definition for optimizing it. (flambda is an optimization/inlining pass
    in OCaml >= 4.03).
    @since 0.18 *)

val iterate : int -> ('a -> 'a) -> 'a -> 'a
(** [iterate n f] is [f] iterated [n] times. That is to say, [iterate 0 f x] is
    [x], [iterate 1 f x] is [f x], [iterate 2 f x] is [f (f x)], etc.
    @since 2.1 *)

val with_return : (('ret -> 'a) -> 'ret) -> 'ret
(** [with_return f] is [f return], where [return] is a function
  that can be invoked to exit the scope early.

  For example:
{[
let find_array arr x =
  let@ return = with_return in
  for i = 0 to Array.length arr-1 do
    if arr.(i) = x then return i;
  done;
  -1
]}

  @since 3.15 *)

(** {2 Infix}

    Infix operators. *)

module Infix : sig
  val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  (** [(f %> g) x] or [(%>) f g x] is [g (f x)]. Alias to [compose]. *)

  val ( % ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** [(f % g) x] or [(%) f g x] is [f (g x)]. Mathematical composition. *)

  val ( let@ ) : ('a -> 'b) -> 'a -> 'b
  (** [let@ x = foo in bar] is the equivalent of [foo @@ fun x -> bar].
      It can be very useful for resource management, alongside with {!protect}.
      @since 3.11 *)

  val ( ||> ) : 'a * 'b -> ('a -> 'b -> 'c) -> 'c
  (** [x ||> f] is [f (fst x) (snd x)]
      @since 3.13.1 *)

  val ( |||> ) : 'a * 'b * 'c -> ('a -> 'b -> 'c -> 'd) -> 'd
  (** like [||>] but for tuples of size 3
      @since 3.13.1 *)
end

include module type of Infix

(** {2 Monad}

    Functions with a fixed domain are monads in their codomain. *)

module Monad (X : sig
  type t
end) : sig
  type 'a t = X.t -> 'a
  (** Definition of a monad in continuation-passing style. *)

  val return : 'a -> 'a t
  (** Monadic [return]. *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** Monadic [map]. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic [bind]. *)
end
