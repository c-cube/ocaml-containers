
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Helpers for the standard {b Seq} type}

    See {{: https://github.com/c-cube/oseq/} oseq} for a richer API.
*)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type + 'a t = unit -> 'a node
and +'a node = 'a Seq.node =
  | Nil
  | Cons of 'a * 'a t

val nil : 'a t

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

val singleton : 'a -> 'a t

val repeat : ?n:int -> 'a -> 'a t
(** [repeat ~n x] repeats [x] [n] times then stops. If [n] is omitted,
    then [x] is repeated forever.
    @since 0.3.3 *)

val cycle : 'a t -> 'a t
(** Cycle through the iterator infinitely. The iterator shouldn't be empty.
    @since 0.3.3 *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** [unfold f acc] calls [f acc] and:
    - if [f acc = Some (x, acc')], yield [x], continue with [unfold f acc'].
    - if [f acc = None], stops.
    @since 0.13 *)

val is_empty : 'a t -> bool

val head : 'a t -> 'a option
(** Head of the list.
    @since 0.13 *)

val head_exn : 'a t -> 'a
(** Unsafe version of {!head}.
    @raise Not_found if the list is empty.
    @since 0.13 *)

val tail : 'a t -> 'a t option
(** Tail of the list.
    @since 0.13 *)

val tail_exn : 'a t -> 'a t
(** Unsafe version of {!tail}.
    @raise Not_found if the list is empty.
    @since 0.13 *)

val equal : 'a equal -> 'a t equal
(** Equality step by step. Eager. *)

val compare : 'a ord -> 'a t ord
(** Lexicographic comparison. Eager. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on values. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Alias for {!fold} *)

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate with index (starts at 0).
    @since 0.13 *)

val length : _ t -> int
(** Number of elements in the list.
    Will not terminate if the list if infinite:
    use (for instance) {!take} to make the list finite if necessary. *)

val take : int -> 'a t -> 'a t

val take_while : ('a -> bool) -> 'a t -> 'a t

val drop : int -> 'a t -> 'a t

val drop_while : ('a -> bool) -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Map with index (starts at 0).
    @since 0.13 *)

val fmap : ('a -> 'b option) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t

val product_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Fair product of two (possibly infinite) lists into a new list. Lazy.
    The first parameter is used to combine each pair of elements.
    @since 0.3.3 *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Specialization of {!product_with} producing tuples.
    @since 0.3.3 *)

val group : 'a equal -> 'a t -> 'a t t
(** [group eq l] groups together consecutive elements that satisfy [eq]. Lazy.
    For instance [group (=) [1;1;1;2;2;3;3;1]] yields
      [[1;1;1]; [2;2]; [3;3]; [1]].
    @since 0.3.3 *)

val uniq : 'a equal -> 'a t -> 'a t
(** [uniq eq l] returns [l] but removes consecutive duplicates. Lazy.
    In other words, if several values that are equal follow one another,
    only the first of them is kept.
    @since 0.3.3 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flatten : 'a t t -> 'a t

val range : int -> int -> int t

val (--) : int -> int -> int t
(** [a -- b] is the range of integers containing
    [a] and [b] (therefore, never empty). *)

val (--^) : int -> int -> int t
(** [a -- b] is the integer range from [a] to [b], where [b] is excluded.
    @since 0.17 *)

(** {2 Operations on two Collections} *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** Fold on two collections at once. Stop at soon as one of them ends. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Map on two collections at once. Stop as soon as one of the
    arguments is exhausted. *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on two collections at once. Stop as soon as one of them ends. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val merge : 'a ord -> 'a t -> 'a t -> 'a t
(** Merge two sorted iterators into a sorted iterator. *)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** Combine elements pairwise. Stop as soon as one of the lists stops.
    @since 0.13 *)

val unzip : ('a * 'b) t -> 'a t * 'b t
(** Split each tuple in the list.
    @since 0.13 *)

(** {2 Misc} *)

val sort : cmp:'a ord -> 'a t -> 'a t
(** Eager sort. Require the iterator to be finite. [O(n ln(n))] time
    and space.
    @since 0.3.3 *)

val sort_uniq : cmp:'a ord -> 'a t -> 'a t
(** Eager sort that removes duplicate values. Require the iterator to be
    finite. [O(n ln(n))] time and space.
    @since 0.3.3 *)

val memoize : 'a t -> 'a t
(** Avoid recomputations by caching intermediate results.
    @since 0.14 *)

(** {2 Fair Combinations} *)

val interleave : 'a t -> 'a t -> 'a t
(** Fair interleaving of both streams.
    @since 0.13 *)

val fair_flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Fair version of {!flat_map}.
    @since 0.13 *)

val fair_app : ('a -> 'b) t -> 'a t -> 'b t
(** Fair version of {!(<*>)}.
    @since 0.13 *)

(** {2 Implementations}
    @since 0.3.3 *)

val return : 'a -> 'a t
val pure : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val (>>-) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {! fair_flat_map}.
    @since 0.13 *)

val (<.>) : ('a -> 'b) t -> 'a t -> 'b t
(** Infix version of {!fair_app}.
    @since 0.13 *)

(** {2 Infix operators}

    @since 0.17 *)

module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (>>-) : 'a t -> ('a -> 'b t) -> 'b t
  val (<.>) : ('a -> 'b) t -> 'a t -> 'b t
  val (--) : int -> int -> int t
  val (--^) : int -> int -> int t
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t
end

(** {2 Conversions} *)

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list
(** Gather all values into a list. *)

val of_array : 'a array -> 'a t
(** Iterate on the array.
    @since 0.13 *)

val to_array : 'a t -> 'a array
(** Convert into array. Iterate twice.
    @since 0.13 *)

val to_rev_list : 'a t -> 'a list
(** Convert to a list, in reverse order. More efficient than {!to_list}. *)

val to_iter : 'a t -> 'a iter

val to_gen : 'a t -> 'a gen

val of_gen : 'a gen -> 'a t
(** [of_gen g] consumes the generator and caches intermediate results.
    @since 0.13 *)

(** {2 IO} *)

val pp : ?sep:string -> 'a printer -> 'a t printer
(** Print the list with the given separator (default ",").
    Do not print opening/closing delimiters. *)
