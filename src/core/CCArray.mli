(* This file is free software, part of containers. See file "license" for more details. *)

(** Array utils *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Arrays} *)

include module type of Array
(** @inline *)

val empty : 'a t
(** [empty] is the empty array, physically equal to [[||]]. *)

val equal : 'a equal -> 'a t equal
(** [equal eq a1 a2] is [true] if the lengths of [a1] and [a2] are the same
    and if their corresponding elements test equal, using [eq]. *)

val compare : 'a ord -> 'a t ord
(** [compare cmp a1 a2] compares arrays [a1] and [a2] using the function comparison [cmp]. *)

val swap : 'a t -> int -> int -> unit
(** [swap a i j] swaps elements at indices [i] and [j].
    @since 1.4 *)

val get_safe : 'a t -> int -> 'a option
(** [get_safe a i] returns [Some a.(i)] if [i] is a valid index.
    @since 0.18 *)

val map_inplace : ('a -> 'a) -> 'a t -> unit
(** [map_inplace f a] replace all elements of [a] by its image by [f].
    @since 3.8 *)

val mapi_inplace : (int -> 'a -> 'a) -> 'a t -> unit
(** [mapi_inplace f a] replace all elements of [a] by its image by [f].
    @since 3.10 *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f init a] computes [f (… (f (f init a.(0)) a.(1)) …) a.(n-1)],
    where [n] is the length of the array [a].
    Same as {!Array.fold_left}*)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [foldi f init a] is just like {!fold}, but it also passes in the index
    of each element as the second argument to the folded function [f]. *)

val fold_while : ('a -> 'b -> 'a * [ `Stop | `Continue ]) -> 'a -> 'b t -> 'a
(** [fold_while f init a] folds left on array [a] until a stop condition via [('a, `Stop)]
    is indicated by the accumulator.
    @since 0.8 *)

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
(** [fold_map f init a] is a [fold_left]-like function, but it also maps the
    array to another array.
    @since 1.2, but only
    @since 2.1 with labels *)

val scan_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t
(** [scan_left f init a] returns the array
    [ [|init; f init x0; f (f init a.(0)) a.(1); …|] ].

    @since 1.2, but only
    @since 2.1 with labels *)

val reverse_in_place : 'a t -> unit
(** [reverse_in_place a] reverses the array [a] in place. *)

val sorted : ('a -> 'a -> int) -> 'a t -> 'a array
(** [sorted f a] makes a copy of [a] and sorts it with [f].
    @since 1.0 *)

val sort_indices : ('a -> 'a -> int) -> 'a t -> int array
(** [sort_indices f a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the index at which the [i]-th element of [sorted f a]
    appears in [a]. [a] is not modified.

    In other words, [map (fun i -> a.(i)) (sort_indices f a) = sorted f a].
    [sort_indices] yields the inverse permutation of {!sort_ranking}.
    @since 1.0 *)

val sort_ranking : ('a -> 'a -> int) -> 'a t -> int array
(** [sort_ranking f a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the index at which the [i]-th element of [a] appears
    in [sorted f a]. [a] is not modified.

    In other words, [map (fun i -> (sorted f a).(i)) (sort_ranking f a) = a].
    [sort_ranking] yields the inverse permutation of {!sort_indices}.

    In the absence of duplicate elements in [a], we also have
    [lookup_exn a.(i) (sorted a) = (sorted_ranking a).(i)].
    @since 1.0 *)

val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** [mem ~eq x a] return true if x is present in [a]. Linear time.
    @since 3.0
*)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f a] returns [Some y] if there is an element [x] such
    that [f x = Some y]. Otherwise returns [None].
    @since 1.3, but only
    @since 2.1 with labels *)

val find_map_i : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** [find_map_i f a] is like {!find_map}, but the index of the element is also passed
    to the predicate function [f].
    @since 1.3, but only
    @since 2.1 with labels *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx f a] returns [Some (i,x)] where [x] is the [i]-th element of [a],
    and [f x] holds. Otherwise returns [None].
    @since 0.3.4 *)

val max : ('a -> 'a -> int) -> 'a t -> 'a option
(** [max cmp a] returns [None] if [a] is empty, otherwise, returns [Some e] where [e]
    is a maximum element in [a] with respect to [cmp].
    @since 3.12 *)

val max_exn : ('a -> 'a -> int) -> 'a t -> 'a
(** [max_exn cmp a] is like {!max}, but
    @raise Invalid_argument if [a] is empty.
    @since 3.12 *)

val argmax : ('a -> 'a -> int) -> 'a t -> int option
(** [argmax cmp a] returns [None] if [a] is empty, otherwise, returns [Some i] where [i]
    is the index of a maximum element in [a] with respect to [cmp].
    @since 3.12 *)

val argmax_exn : ('a -> 'a -> int) -> 'a t -> int
(** [argmax_exn cmp a] is like {!argmax}, but
    @raise Invalid_argument if [a] is empty.
    @since 3.12 *)

val min : ('a -> 'a -> int) -> 'a t -> 'a option
(** [min cmp a] returns [None] if [a] is empty, otherwise, returns [Some e] where [e]
    is a minimum element in [a] with respect to [cmp].
    @since 3.12 *)

val min_exn : ('a -> 'a -> int) -> 'a t -> 'a
(** [min_exn cmp a] is like {!min}, but
    @raise Invalid_argument if [a] is empty.
    @since 3.12 *)

val argmin : ('a -> 'a -> int) -> 'a t -> int option
(** [argmin cmp a] returns [None] if [a] is empty, otherwise, returns [Some i] where [i]
    is the index of a minimum element in [a] with respect to [cmp].
    @since 3.12 *)

val argmin_exn : ('a -> 'a -> int) -> 'a t -> int
(** [argmin_exn cmp a] is like {!argmin}, but
    @raise Invalid_argument if [a] is empty.
    @since 3.12 *)

val lookup : cmp:'a ord -> 'a -> 'a t -> int option
(** [lookup ~cmp key a] lookups the index of some key [key] in a sorted array [a].
    Undefined behavior if the array [a] is not sorted wrt [~cmp].
    Complexity: [O(log (n))] (dichotomic search).
    @return [None] if the key [key] is not present, or
      [Some i] ([i] the index of the key) otherwise. *)

val lookup_exn : cmp:'a ord -> 'a -> 'a t -> int
(** [lookup_exn ~cmp key a] is like {!lookup}, but
    @raise Not_found if the key [key] is not present. *)

val bsearch :
  cmp:('a -> 'a -> int) ->
  'a ->
  'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [bsearch ~cmp key a] finds the index of the object [key] in the array [a],
    provided [a] is {b sorted} using [cmp]. If the array is not sorted,
    the result is not specified (may raise Invalid_argument).

    Complexity: [O(log n)] where n is the length of the array [a]
    (dichotomic search).

    @return
    - [`At i] if [cmp a.(i) key = 0] (for some i).
    - [`All_lower] if all elements of [a] are lower than [key].
    - [`All_bigger] if all elements of [a] are bigger than [key].
    - [`Just_after i] if [a.(i) < key < a.(i+1)].
    - [`Empty] if the array [a] is empty.

    @raise Invalid_argument if the array is found to be unsorted w.r.t [cmp].
    @since 0.13 *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [for_all2 f [|a1; …; an|] [|b1; …; bn|]] is [true] if each pair of elements [ai bi]
    satisfies the predicate [f].
    That is, it returns [(f a1 b1) && (f a2 b2) && … && (f an bn)].

    @raise Invalid_argument if arrays have distinct lengths.
    Allow different types.
    @since 0.20 *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [exists2 f [|a1; …; an|] [|b1; …; bn|]] is [true] if any pair of elements [ai bi]
    satisfies the predicate [f].
    That is, it returns [(f a1 b1) || (f a2 b2) || … || (f an bn)].

    @raise Invalid_argument if arrays have distinct lengths.
    Allow different types.
    @since 0.20 *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** [fold2 f init a b] fold on two arrays [a] and [b] stepwise.
    It computes [f (… (f init a1 b1) …) an bn].

    @raise Invalid_argument if [a] and [b] have distinct lengths.
    @since 0.20 *)

val shuffle : 'a t -> unit
(** [shuffle a] randomly shuffles the array [a], in place. *)

val shuffle_with : Random.State.t -> 'a t -> unit
(** [shuffle_with rs a] randomly shuffles the array [a] (like {!shuffle}) but a specialized random
    state [rs] is used to control the random numbers being produced during shuffling (for reproducibility). *)

val random_choose : 'a t -> 'a random_gen
(** [random_choose a rs] randomly chooses an element of [a].
    @raise Not_found if the array/slice is empty. *)

val to_string : ?sep:string -> ('a -> string) -> 'a array -> string
(** [to_string ~sep item_to_string a] print [a] to a string using [sep] as a separator
    between elements of [a].
    @since 2.7 *)

val to_iter : 'a t -> 'a iter
(** [to_iter a] returns an [iter] of the elements of an array [a].
    The input array [a] is shared with the sequence and modification of it will result
    in modification of the iterator.
    @since 2.8 *)

val to_gen : 'a t -> 'a gen
(** [to_gen a] returns a [gen] of the elements of an array [a]. *)

(** {2 IO} *)

val pp :
  ?pp_start:unit printer ->
  ?pp_stop:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a t printer
(** [pp ~pp_start ~pp_stop ~pp_sep pp_item ppf a] formats the array [a] on [ppf].
    Each element is formatted with [pp_item], [pp_start] is called at the beginning,
    [pp_stop] is called at the end, [pp_sep] is called between each elements.
    By defaults [pp_start] and [pp_stop] does nothing and [pp_sep] defaults to
    (fun out -> Format.fprintf out ",@ "). *)

val pp_i :
  ?pp_start:unit printer ->
  ?pp_stop:unit printer ->
  ?pp_sep:unit printer ->
  (int -> 'a printer) ->
  'a t printer
(** [pp_i ~pp_start ~pp_stop ~pp_sep pp_item ppf a] prints the array [a] on [ppf].
    The printing function [pp_item] is giving both index and element.
    [pp_start] is called at the beginning,
    [pp_stop] is called at the end, [pp_sep] is called between each elements.
    By defaults [pp_start] and [pp_stop] does nothing and [pp_sep] defaults to
    (fun out -> Format.fprintf out ",@ "). *)

val rev : 'a t -> 'a t
(** [rev a] copies the array [a] and reverses it in place.
    @since 0.20 *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f a] filters elements out of the array [a]. Only the elements satisfying
    the given predicate [f] will be kept. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f [|a1; …; an|]] calls [(f a1) … (f an)] and returns an array [b] consisting
    of all elements [bi] such as [f ai = Some bi]. When [f] returns [None], the corresponding
    element of [a] is discarded. *)

val monoid_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [monoid_product f a b] passes all combinaisons of tuples from the two arrays [a] and [b]
    to the function [f].
    @since 2.8 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b array
(** [flat_map f a] transforms each element of [a] into an array, then flattens. *)

val except_idx : 'a t -> int -> 'a list
(** [except_idx a i] removes the element of [a] at given index [i], and returns
    the list of the other elements. *)

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
  cmp:('elt -> 'elt -> int) ->
  'arr ->
  unit
(** [sort_generic (module M) ~cmp a] sorts the array [a], without allocating (eats stack space though).
    Performance might be lower than {!Array.sort}.
    @since 0.14 *)

(** {3 Infix Operators}
    It is convenient to [open CCArray.Infix] to access the infix operators
    without cluttering the scope too much.

    @since 2.7 *)

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [a >>= f] is the infix version of {!flat_map}. *)

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  (** [a >>| f] is the infix version of {!map}.
      @since 0.8 *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** [a >|= f] is the infix version of {!map}.
      @since 0.8 *)

  val ( -- ) : int -> int -> int t
  (** [x -- y] creates an array containing integers in the range [x .. y]. Bounds included. *)

  val ( --^ ) : int -> int -> int t
  (** [x --^ y] creates an array containing integers in the range [x .. y]. Right bound excluded.
      @since 0.17 *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

include module type of Infix
