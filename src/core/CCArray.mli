
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
(** The type for arrays *)

val empty : 'a t
(** [empty] is the empty array, physically equal to [||]. *)

val equal : 'a equal -> 'a t equal
(** [equal eq a1 a2] is [true] if the lengths of [a1] and [a2] are the same
    and if their corresponding elements test equal, using [eq]. *)

val compare : 'a ord -> 'a t ord
(** [compare cmp a1 a2] compares arrays [a1] and [a2] using the function comparison [cmp]. *)

val swap : 'a t -> int -> int -> unit
(** [swap a i j] swaps elements at indices [i] and [j].
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
(** [length a] returns the length (number of elements) of the given array [a]. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f acc a] computes [f (... (f (f acc a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the array [a]. *)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [foldi f acc a] is just like {!fold}, but it also passes in the index
    of each element as the second argument to the folded function [f]. *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** [fold_while f acc a] folds left on array [a] until a stop condition via [('a, `Stop)]
    is indicated by the accumulator.
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
(** [iter f a] applies function [f] in turn to all elements of [a].
    It is equivalent to [f a.(0); f a.(1); ...; f a.(length a - 1); ()]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f a] is like {!iter}, but the function [f] is applied with the index of the
    element as first argument, and the element itself as second argument. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit a1 o1 a2 o2 len] copies [len] elements
    from array [a1], starting at element number [o1], to array [a2],
    starting at element number [o2]. It works correctly even if
    [a1] and [a2] are the same array, and the source and
    destination chunks overlap.

    Raise [Invalid_argument "CCArray.blit"] if [o1] and [len] do not
    designate a valid subarray of [a1], or if [o2] and [len] do not
    designate a valid subarray of [a2]. *)

val reverse_in_place : 'a t -> unit
(** [reverse_in_place a] reverses the array [a] in place. *)

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
    such that [b.(i)] is the index at which the [i]-th element of [a] appears
    in [sorted cmp a]. [a] is not modified.

    In other words, [map (fun i -> (sorted cmp a).(i)) (sort_ranking cmp a) = a].
    [sort_ranking] yields the inverse permutation of {!sort_indices}.

    In the absence of duplicate elements in [a], we also have
    [lookup_exn a.(i) (sorted a) = (sorted_ranking a).(i)].
    @since 1.0 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f a] returns [Some y] if there is an element [x] such
    that [f x = Some y]. Otherwise returns [None].
    @since 1.3 *)

val find : ('a -> 'b option) -> 'a t -> 'b option
(** [find f a] is an alias to {!find_map}.
    @deprecated since 1.3, use {!find_map} instead. *)

val find_map_i : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** [find_map_i f a] is like {!find_map}, but the index of the element is also passed
    to the predicate function [f].
    @since 1.3 *)

val findi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** [findi f a] is an alias to {!find_map_i}.
    @since 0.3.4
    @deprecated since 1.3, use {!find_map_i} instead. *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p a] returns [Some (i,x)] where [x] is the [i]-th element of [a],
    and [p x] holds. Otherwise returns [None].
    @since 0.3.4 *)

val lookup : cmp:'a ord -> 'a -> 'a t -> int option
(** [lookup cmp x a] lookups the index of some key [x] in a sorted array [a].
    Undefined behavior if the array [a] is not sorted wrt [cmp].
    Complexity: [O(log (n))] (dichotomic search).
    @return [None] if the key [x] is not present, or
      [Some i] ([i] the index of the key) otherwise. *)

val lookup_exn : cmp:'a ord -> 'a -> 'a t -> int
(** [lookup_exn cmp x a] is like {!lookup}, but
    @raise Not_found if the key [x] is not present. *)

val bsearch : cmp:('a -> 'a -> int) -> 'a -> 'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [bsearch ~cmp x a] finds the index of the object [x] in the array [a],
    provided [a] is {b sorted} using [cmp]. If the array is not sorted,
    the result is not specified (may raise Invalid_argument).

    Complexity: [O(log n)] where n is the length of the array [a]
    (dichotomic search).

    @return
    - [`At i] if [cmp a.(i) x = 0] (for some i).
    - [`All_lower] if all elements of [a] are lower than [x].
    - [`All_bigger] if all elements of [a] are bigger than [x].
    - [`Just_after i] if [a.(i) < x < a.(i+1)].
    - [`Empty] if the array [a] is empty.

    @raise Invalid_argument if the array is found to be unsorted w.r.t [cmp].
    @since 0.13 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [|a1; ...; an|]] is [true] if all elements of the array
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [for_all2 p [|a1; ...; an|] [|b1; ...; bn|]] is [true] if each pair of elements [ai bi]
    satisfies the predicate [p].
    That is, it returns [(p a1 b1) && (p a2 b2) && ... && (p an bn)].

    @raise Invalid_argument if arrays have distinct lengths.
    Allow different types.
    @since 0.20 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [|a1; ...; an|]] is [true] if at least one element of
    the array satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)]. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [exists2 p [|a1; ...; an|] [|b1; ...; bn|]] is [true] if any pair of elements [ai bi]
    satisfies the predicate [p].
    That is, it returns [(p a1 b1) || (p a2 b2) || ... || (p an bn)].

    @raise Invalid_argument if arrays have distinct lengths.
    Allow different types.
    @since 0.20 *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** [fold2 f acc a b] fold on two arrays [a] and [b] stepwise.
    It computes [f (... (f acc a1 b1)...) an bn].

    @raise Invalid_argument if arrays have distinct lengths.
    @since 0.20 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f a b] iterates on the two arrays [a] and [b] stepwise.
    It is equivalent to  [f a0 b0; ...; f a.(length a - 1) b.(length b - 1); ()].

    @raise Invalid_argument if arrays have distinct lengths.
    @since 0.20 *)

val shuffle : 'a t -> unit
(** [shuffle a] randomly shuffles the array [a], in place. *)

val shuffle_with : Random.State.t -> 'a t -> unit
(** [shuffle_with rs a] randomly shuffles the array [a] (like {!shuffle}) but a specialized random
    state [rs] is used to control the random numbers being produced during shuffling (for reproducibility). *)

val random_choose : 'a t -> 'a random_gen
(** [random_choose a rs] randomly chooses an element of [a].
    @raise Not_found if the array/slice is empty. *)

val to_seq : 'a t -> 'a sequence
(** [to_seq a] returns a [sequence] of the elements of an array [a].
    The input array [a] is shared with the sequence and modification of it will result
    in modification of the sequence. *)

val to_gen : 'a t -> 'a gen
(** [to_gen a] returns a [gen] of the elements of an array [a]. *)

val to_klist : 'a t -> 'a klist
(** [to_klist] returns a [klist] of the elements of an array [a]. *)

(** {2 IO} *)

val pp: ?sep:string -> 'a printer -> 'a t printer
(** [pp ~sep pp_item ppf a] formats the array [a] on [ppf].
    Each element is formatted with [pp_item] and elements are separated
    by [sep] (defaults to ", "). *)

val pp_i: ?sep:string -> (int -> 'a printer) -> 'a t printer
(** [pp_i ~sep pp_item ppf a] prints the array [a] on [ppf].
    The printing function [pp_item] is giving both index and element.
    Elements are separated by [sep] (defaults to ", "). *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] applies function [f] to all elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(length a - 1) |]]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f a b] applies function [f] to all elements of [a] and [b],
    and builds an array with the results returned by [f]:
    [[| f a.(0) b.(0); ...; f a.(length a - 1) b.(length b - 1)|]].

    @raise Invalid_argument if [a] and [b] have distinct lengths.
    @since 0.20 *)

val rev : 'a t -> 'a t
(** [rev a] copies the array [a] and reverses it in place.
    @since 0.20 *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p a] filters elements out of the array [a]. Only the elements satisfying
    the given predicate [p] will be kept. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f [|a1; ...; an|]] calls [(f a1) ... (f an)] and returns an array [b] consisting
    of all elements [bi] such as [f ai = Some bi]. When [f] returns [None], the corresponding
    element of [a] is discarded. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b array
(** [flat_map f a] transforms each element of [a] into an array, then flattens. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [a >>= f] is the infix version of {!flat_map}. *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** [a >>| f] is the infix version of {!map}.
    @since 0.8 *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** [a >|= f] is the infix version of {!map}.
    @since 0.8 *)

val except_idx : 'a t -> int -> 'a list
(** [except_idx a i] removes the element of [a] at given index [i], and returns
    the list of the other elements. *)

val (--) : int -> int -> int t
(** [x -- y] creates an array containing integers in the range [x .. y]. Bounds included. *)

val (--^) : int -> int -> int t
(** [x --^ y] creates an array containing integers in the range [x .. y]. Right bound excluded.
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
(** [sort_generic (module M) cmp a] sorts the array [a], without allocating (eats stack space though).
    Performance might be lower than {!Array.sort}.
    @since 0.14 *)
