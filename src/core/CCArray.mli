
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array utils} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Arrays} *)

include module type of struct include Array end

type 'a t = 'a array

val empty : 'a t
(** The empty array, physically equal to [||]. *)

val equal : 'a equal -> 'a t equal
(** Hoist an equality test for elements to arrays.
    Arrays are only equal if their lengths are the same and
    corresponding elements test equal. *)

val compare : 'a ord -> 'a t ord

val swap : 'a t -> int -> int -> unit
(** [swap arr i j] swaps elements at indices [i] and [j].
    @since 1.4 *)

val get : 'a t -> int -> 'a
(** [get a n] returns the element number [n] of array [a].
    The first element has number 0.
    The last element has number [length a - 1].
    You can also write [a.(n)] instead of [get a n].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [(length a - 1)]. *)

val get_safe : 'a t -> int -> 'a option
(** [get_safe a i] returns [Some a.(i)] if [i] is a valid index.
    @since 0.18 *)

val set : 'a t -> int -> 'a -> unit
(** [set a n x] modifies array [a] in place, replacing
    element number [n] with [x].
    You can also write [a.(n) <- x] instead of [set a n x].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [length a - 1]. *)

val length : _ t -> int
(** Return the length (number of elements) of the given array. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f x a] computes [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the array [a]. *)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold left on array, with index. *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Fold left on array until a stop condition via [('a, `Stop)] is
    indicated by the accumulator.
    @since 0.8 *)

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
(** [fold_map f acc a] is a [fold_left]-like function, but it also maps the
    array to another array.
    @since 1.2 *)

val scan_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t
(** [scan_left f acc a] returns the array
    [ [|acc; f acc x0; f (f acc a.(0)) a.(1); …|] ].

    @since 1.2 *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f a] applies function [f] in turn to all
    the elements of [a].  It is equivalent to
    [f a.(0); f a.(1); ...; f a.(length a - 1); ()]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Like {!Array.iter}, but the function is applied to the index of the
    element as first argument, and the element itself as second argument. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit v1 o1 v2 o2 len] copies [len] elements
    from array [v1], starting at element number [o1], to array [v2],
    starting at element number [o2]. It works correctly even if
    [v1] and [v2] are the same array, and the source and
    destination chunks overlap.

    Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
    designate a valid subarray of [v1], or if [o2] and [len] do not
    designate a valid subarray of [v2]. *)

val reverse_in_place : 'a t -> unit
(** Reverse the array in place. *)

val sorted : ('a -> 'a -> int) -> 'a t -> 'a array
(** [sorted cmp a] makes a copy of [a] and sorts it with [cmp].
    @since 1.0 *)

val sort_indices : ('a -> 'a -> int) -> 'a t -> int array
(** [sort_indices cmp a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the index at which the [i]-th element of [sorted cmp a]
    appears in [a]. [a] is not modified.

    In other words, [map (fun i -> a.(i)) (sort_indices cmp a) = sorted cmp a].
    [sort_indices] yields the inverse permutation of {!sort_ranking}.
    @since 1.0 *)

val sort_ranking : ('a -> 'a -> int) -> 'a t -> int array
(** [sort_ranking cmp a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the index at which the [i]-the element of [a] appears
    in [sorted cmp a]. [a] is not modified.

    In other words, [map (fun i -> (sorted cmp a).(i)) (sort_ranking cmp a) = a].
    [sort_ranking] yields the inverse permutation of {!sort_indices}.

    In the absence of duplicate elements in [a], we also have
    [lookup_exn a.(i) (sorted a) = (sorted_ranking a).(i)].
    @since 1.0 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f a] returns [Some y] if there is an element [x] such
    that [f x = Some y], else it returns [None].
    @since 1.3 *)

val find : ('a -> 'b option) -> 'a t -> 'b option
(** Alias to {!find_map}.
    @deprecated since 1.3 *)

val find_map_i : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find_map}, but also pass the index to the predicate function.
    @since 1.3 *)

val findi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Alias to {!find_map_i}.
    @since 0.3.4
    @deprecated since 1.3 *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None].
    @since 0.3.4 *)

val lookup : cmp:'a ord -> 'a -> 'a t -> int option
(** Lookup the index of some value in a sorted array.
    Undefined behavior if the array is not sorted wrt [cmp].
    Complexity: [O(log (n))] (dichotomic search).
    @return [None] if the key is not present, or
      [Some i] ([i] the index of the key) otherwise. *)

val lookup_exn : cmp:'a ord -> 'a -> 'a t -> int
(** Like {!lookup}, but
    @raise Not_found if the key is not present. *)

val bsearch : cmp:('a -> 'a -> int) -> 'a -> 'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [bsearch ?cmp x arr] finds the index of the object [x] in the array [arr],
    provided [arr] is {b sorted} using [cmp]. If the array is not sorted,
    the result is not specified (may @raise Invalid_argument).

    Complexity: [O(log n)] where n is the length of the array
    (dichotomic search).

    @return
    - [`At i] if [cmp arr.(i) x = 0] (for some i).
    - [`All_lower] if all elements of [arr] are lower than [x].
    - [`All_bigger] if all elements of [arr] are bigger than [x].
    - [`Just_after i] if [arr.(i) < x < arr.(i+1)].
    - [`Empty] if the array is empty.

    @raise Invalid_argument if the array is found to be unsorted w.r.t [cmp].
    @since 0.13 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [|a1; ...; an|]] checks if all elements of the array
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Forall on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths.
    Allow different types.
    @since 0.20 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [|a1; ...; an|]] checks if at least one element of
    the array satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)]. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Exists on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths.
    Allow different types.
    @since 0.20 *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** Fold on two arrays stepwise.
    @raise Invalid_argument if they have distinct lengths.
    @since 0.20 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on two arrays stepwise.
    @raise Invalid_argument if they have distinct lengths.
    @since 0.20 *)

val shuffle : 'a t -> unit
(** Shuffle randomly the array, in place. *)

val shuffle_with : Random.State.t -> 'a t -> unit
(** Like {!shuffle} but using a specialized random state. *)

val random_choose : 'a t -> 'a random_gen
(** Choose an element randomly.
    @raise Not_found if the array/slice is empty. *)

val to_seq : 'a t -> 'a sequence
(** Return a [sequence] of the elements of an array.
    The input array is shared with the sequence and modifications of it will result
    in modification of the sequence. *)

val to_gen : 'a t -> 'a gen
(** Return a [gen] of the elements of an array. *)

val to_klist : 'a t -> 'a klist
(** Return a [klist] of the elements of an array. *)

(** {2 IO} *)

val pp: ?sep:string -> 'a printer -> 'a t printer
(** Print an array of items with printing function. *)

val pp_i: ?sep:string -> (int -> 'a printer) -> 'a t printer
(** Print an array, giving the printing function both index and item. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] applies function [f] to all the elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(length a - 1) |]]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f a b] applies function [f] to all the elements of [a] and [b],
    and builds an array with the results returned by [f]:
    [[| f a.(0) b.(0); ...; f a.(length a - 1) b.(length b - 1)|]].
      @raise Invalid_argument if they have distinct lengths.
      @since 0.20 *)

val rev : 'a t -> 'a t
(** Copy + reverse in place.
    @since 0.20 *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements out of the array. Only the elements satisfying
    the given predicate will be kept. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map each element into another value, or discard it. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b array
(** Transform each element into an array, then flatten. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {!flat_map}. *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of {!map}.
    @since 0.8 *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of {!map}.
    @since 0.8 *)

val except_idx : 'a t -> int -> 'a list
(** Remove given index, obtaining the list of the other elements. *)

val (--) : int -> int -> int t
(** Range array. Bounds included. *)

val (--^) : int -> int -> int t
(** Range array, excluding right bound.
    @since 0.17 *)

val random : 'a random_gen -> 'a t random_gen
val random_non_empty : 'a random_gen -> 'a t random_gen
val random_len : int -> 'a random_gen -> 'a t random_gen

(** {2 Generic Functions} *)

module type MONO_ARRAY = sig
  type elt
  type t

  val length : t -> int

  val get : t -> int -> elt

  val set : t -> int -> elt -> unit
end

val sort_generic :
  (module MONO_ARRAY with type t = 'arr and type elt = 'elt) ->
  cmp:('elt -> 'elt -> int) -> 'arr -> unit
(** Sort the array, without allocating (eats stack space though). Performance
    might be lower than {!Array.sort}.
    @since 0.14 *)
