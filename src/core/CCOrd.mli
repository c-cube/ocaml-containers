
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Comparisons} *)

type 'a t = 'a -> 'a -> int
(** Comparison (total ordering) between two elements, that returns an int. *)

val compare : 'a t
(** Polymorphic "magic" comparison. *)

val opp : 'a t -> 'a t
(** Opposite order. *)

val equiv : int -> int -> bool
(** Returns [true] iff the two comparison results are the same. *)

val int : int t
val string : string t
val bool : bool t
val float : float t

(** {2 Lexicographic Combination} *)

val (<?>) : int -> ('a t * 'a * 'a) -> int
(** [c1 <?> (ord, x, y)] returns the same as [c1] if [c1] is not [0];
    otherwise it uses [ord] to compare the two values [x] and [y],
    of type ['a].

    Example:
    {[CCInt.compare 1 3
      <?> (String.compare, "a", "b")
      <?> (CCBool.compare, true, false)]}

    Same example, using only CCOrd::
    {[CCOrd.(int 1 3
        <?> (string, "a", "b")
        <?> (bool, true, false))]}
*)

val option : 'a t -> 'a option t
(** Comparison of optional values. [None] is smaller than any [Some _].
    @since 0.15 *)

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val list : 'a t -> 'a list t
(** Lexicographic combination on lists. *)

val array : 'a t -> 'a array t

val map : ('a -> 'b) -> 'b t -> 'a t
(** [map f ord] is the comparison function that, given objects [x] and [y],
    projects [x] and [y] using [f] (e.g. using a record field) and then
    compares those projections with [ord].
    Example:
    [map fst CCInt.compare] compares values of type [(int * 'a)]  by their
      first component. *)

val (>|=) : 'b t -> ('a -> 'b) -> 'a t
(** Infix equivalent of {!map}. *)

module Infix : sig
  val (<?>) : int -> ('a t * 'a * 'a) -> int
  (** [c1 <?> (ord, x, y)] returns the same as [c1] if [c1] is not [0];
      otherwise it uses [ord] to compare the two values [x] and [y],
      of type ['a]. *)

  val (>|=) : 'b t -> ('a -> 'b) -> 'a t
  (** Infix equivalent of {!map}. *)

end
