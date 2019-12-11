
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Equality Combinators} *)

(** @since 1.2 *)

type 'a t = 'a -> 'a -> bool
(** Equality function. Must be transitive, symmetric, and reflexive. *)

val poly : 'a t
(** Standard polymorphic equality. *)

val physical : 'a t
(** Standard physical equality.
    @since 2.0 *)

val int : int t
val string : string t
val bool : bool t
val float : float t
val unit : unit t

val list : 'a t -> 'a list t
val array : 'a t -> 'a array t

val option : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val map : f:('a -> 'b) -> 'b t -> 'a t
(** [map f eq] is the equality function that, given objects [x] and [y],
    projects [x] and [y] using [f] (e.g. using a record field) and then
    compares those projections with [eq].
    Example:
    [map fst int] compares values of type [(int * 'a)]  by their
      first component. *)

module Infix : sig
  val (>|=) : 'b t -> ('a -> 'b) -> 'a t
  (** Infix equivalent of {!map}. *)
end

include module type of Infix

(* test consistency of interfaces *)
(*$inject
  module type L = module type of CCEqual
  module type LL = module type of CCEqualLabels
*)

(*$R
  ignore (module CCEqualLabels : L)
*)

(*$R
  ignore (module CCEqual : LL)
*)

