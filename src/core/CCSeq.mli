(** Helpers for the standard {b Seq} type

    See {{: https://github.com/c-cube/oseq/} oseq} for a richer API.

    @since 3.0
*)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

include module type of Seq
(** @inline *)

val nil : 'a t
val empty : 'a t
val cons : 'a -> 'a t -> 'a t
val singleton : 'a -> 'a t

val init : int -> (int -> 'a) -> 'a t
(** [init n f] corresponds to the sequence [f 0; f 1; ...; f (n-1)].
    @raise Invalid_argument if n is negative.
    @since 3.10 *)

val repeat : ?n:int -> 'a -> 'a t
(** [repeat ~n x] repeats [x] [n] times then stops. If [n] is omitted,
    then [x] is repeated forever. *)

val forever : (unit -> 'a) -> 'a t
(** [forever f] corresponds to the infinite sequence containing all the [f ()].
    @since 3.10 *)

val cycle : 'a t -> 'a t
(** Cycle through the iterator infinitely. The iterator shouldn't be empty. *)

val iterate : ('a -> 'a) -> 'a -> 'a t
(** [iterate f a] corresponds to the infinite sequence containing [a], [f a], [f (f a)],
    ...
    @since 3.10 *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** [unfold f acc] calls [f acc] and:
    - if [f acc = Some (x, acc')], yield [x], continue with [unfold f acc'].
    - if [f acc = None], stops. *)

val is_empty : 'a t -> bool
(** [is_empty xs] checks in the sequence [xs] is empty *)

val head : 'a t -> 'a option
(** Head of the list. *)

val head_exn : 'a t -> 'a
(** Unsafe version of {!head}.
    @raise Not_found if the list is empty. *)

val tail : 'a t -> 'a t option
(** Tail of the list. *)

val tail_exn : 'a t -> 'a t
(** Unsafe version of {!tail}.
    @raise Not_found if the list is empty. *)

val uncons : 'a t -> ('a * 'a t) option
(** [uncons xs] return [None] if [xs] is empty other
    @since 3.10 *)

val equal : 'a equal -> 'a t equal
(** Equality step by step. Eager. *)

val compare : 'a ord -> 'a t ord
(** Lexicographic comparison. Eager. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on values. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Alias for {!fold} *)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_lefti f init xs] applies [f acc i x] where [acc] is the result of the previous
    computation or [init] for the first one, [i] is the index in the sequence (starts at
    0) and [x] is the element of the sequence.
    @since 3.10 *)

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Alias of {!foldi}.
    @since 3.10 *)

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate with index (starts at 0). *)

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
(** Map with index (starts at 0). *)

val fmap : ('a -> 'b option) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t

val product_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Fair product of two (possibly infinite) lists into a new list. Lazy.
    The first parameter is used to combine each pair of elements. *)

val map_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Alias of {!product_with}.
    @since 3.10 *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Specialization of {!product_with} producing tuples. *)

val group : 'a equal -> 'a t -> 'a t t
(** [group eq l] groups together consecutive elements that satisfy [eq]. Lazy.
    For instance [group (=) [1;1;1;2;2;3;3;1]] yields
      [[1;1;1]; [2;2]; [3;3]; [1]]. *)

val uniq : 'a equal -> 'a t -> 'a t
(** [uniq eq l] returns [l] but removes consecutive duplicates. Lazy.
    In other words, if several values that are equal follow one another,
    only the first of them is kept. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [a1; ...; an]] checks if all elements of the sequence satisfy the
    predicate [p].  That is, it returns [(p a1) && ... && (p an)] for a
    non-empty list and [true] if the sequence is empty.  It consumes the
    sequence until it finds an element not satisfying the predicate.
    @since 3.3 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [a1; ...; an]] checks if at least one element of the sequence
    satisfies the predicate [p]. That is, it returns [(p a1) || ... || (p an)]
    for a non-empty sequence and [false] if the list is empty.  It consumes the
    sequence until it finds an element satisfying the predicate.
    @since 3.3 *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p [a1; ...; an]] return [Some ai] for the first [ai] satisfying the predicate
    [p] and return [None] otherwise.
    @since 3.10 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find f [a1; ...; an]] return [Some (f ai)] for the first [ai] such that
    [f ai = Some _] and return [None] otherwise.
    @since 3.10 *)

val scan : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
(** [scan f init xs] is the sequence containing the intermediate result of
    [fold f init xs].
    @since 3.10 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val concat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Alias of {!flat_map}
    @since 3.10 *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val flatten : 'a t t -> 'a t

val concat : 'a t t -> 'a t
(** Alias of {!flatten}.
    @since 3.10 *)

val range : int -> int -> int t

val ( -- ) : int -> int -> int t
(** [a -- b] is the range of integers containing
    [a] and [b] (therefore, never empty). *)

val ( --^ ) : int -> int -> int t
(** [a --^ b] is the integer range from [a] to [b], where [b] is excluded. *)

(** {2 Operations on two Collections} *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** Fold on two collections at once. Stop as soon as one of them ends. *)

val fold_left2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** Alias for {!fold2}.
    @since 3.10 *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Map on two collections at once. Stop as soon as one of the
    arguments is exhausted. *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on two collections at once. Stop as soon as one of them ends. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val merge : 'a ord -> 'a t -> 'a t -> 'a t
(** Merge two sorted iterators into a sorted iterator. *)

val sorted_merge : 'a ord -> 'a t -> 'a t -> 'a t
(** Alias of {!merge}.
    @since 3.10 *)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** Combine elements pairwise. Stop as soon as one of the lists stops. *)

val unzip : ('a * 'b) t -> 'a t * 'b t
(** Split each tuple in the list. *)

val split : ('a * 'b) t -> 'a t * 'b t
(** Alias of {!unzip}.
    @since 3.10 *)

val zip_i : 'a t -> (int * 'a) t
(** [zip_i seq] zips the index of each element with the element itself.
    @since 3.8
*)

(** {2 Misc} *)

val sort : cmp:'a ord -> 'a t -> 'a t
(** Eager sort. Require the iterator to be finite. [O(n ln(n))] time
    and space. *)

val sort_uniq : cmp:'a ord -> 'a t -> 'a t
(** Eager sort that removes duplicate values. Require the iterator to be
    finite. [O(n ln(n))] time and space. *)

val memoize : 'a t -> 'a t
(** Avoid recomputations by caching intermediate results. *)

(** {2 Fair Combinations} *)

val interleave : 'a t -> 'a t -> 'a t
(** Fair interleaving of both streams. *)

val fair_flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Fair version of {!flat_map}. *)

val fair_app : ('a -> 'b) t -> 'a t -> 'b t
(** Fair version of {!(<*>)}. *)

(** {2 Implementations} *)

val return : 'a -> 'a t
val pure : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

val ( >>- ) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {! fair_flat_map}. *)

val ( <.> ) : ('a -> 'b) t -> 'a t -> 'b t
(** Infix version of {!fair_app}. *)

(** {2 Infix operators} *)

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( >>- ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <.> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( -- ) : int -> int -> int t
  val ( --^ ) : int -> int -> int t
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse (M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t
  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t
  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t
end

(** {2 Conversions} *)

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list
(** Gather all values into a list. *)

val of_array : 'a array -> 'a t
(** Iterate on the array. *)

val to_array : 'a t -> 'a array
(** Convert into array. *)

val to_rev_list : 'a t -> 'a list
(** Convert to a list, in reverse order. More efficient than {!to_list}. *)

val to_iter : 'a t -> 'a iter
val to_gen : 'a t -> 'a gen

val of_gen : 'a gen -> 'a t
(** [of_gen g] consumes the generator and caches intermediate results. *)

val of_string : string -> char t
(** Iterate on characters.
    @since 3.7 *)

(** {2 IO} *)

val pp :
  ?pp_start:unit printer ->
  ?pp_stop:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a t printer
(** [pp ~pp_start ~pp_stop ~pp_sep pp_item ppf s] formats the sequence [s] on [ppf].
    Each element is formatted with [pp_item], [pp_start] is called at the beginning,
    [pp_stop] is called at the end, [pp_sep] is called between each elements.
    By defaults [pp_start] and [pp_stop] does nothing and [pp_sep] defaults to
    (fun out -> Format.fprintf out ",@ "). *)
