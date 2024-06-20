(* This file is free software, part of containers. See file "license" for more details. *)

(** Growable, mutable vector.

    This replaces {!CCVector}, removing permissions, and re-vamping the API overall.
    @since NEXT_RELEASE
*)

type 'a t
(** The type of a vector of elements of type ['a], with *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator. *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

val create : unit -> 'a t
(** Create a new, empty vector. *)

val create_with : ?capacity:int -> 'a -> 'a t
(** Create a new vector, the value is used to enforce the type the new vector.
    @param capacity the size of the underlying array. *)

val return : 'a -> 'a t
(** Singleton vector.
    @since 0.14 *)

val make : int -> 'a -> 'a t
(** [make n x] makes a vector of size [n], filled with [x]. *)

val init : int -> (int -> 'a) -> 'a t
(** Init the vector with the given function and size. *)

val clear : _ t -> unit
(** Clear the content of the vector.
    This ensures that [length v = 0] but the underlying array is kept,
    and possibly references to former elements, which are therefore
    not garbage collectible. *)

val clear_and_reset : _ t -> unit
(** Clear the content of the vector, and deallocate the underlying array,
    removing references to all the elements. The elements can be collected. *)

val ensure_with : init:'a -> 'a t -> int -> unit
(** Hint to the vector that it should have at least the given capacity.
    This does not affect [length v].
    @param init if [capacity v = 0], used to enforce the type of the vector
      (see {!create_with}).
    @raise Invalid_arg if the size is not suitable (negative, or too big for OCaml arrays) *)

val ensure : _ t -> int -> unit
(** Hint to the vector that it should have at least the given capacity.
    Just a hint, will not be enforced if the vector is empty and [init]
    is not provided.
    @raise Invalid_arg if the size is not suitable (negative, or too big for OCaml arrays)
*)

val is_empty : _ t -> bool
(** Is the vector empty? *)

val push : 'a t -> 'a -> unit
(** Add an element at the end of the vector. *)

val resize_with : 'a t -> (int -> 'a) -> int -> unit
(** [resize_with vec f size] resizes vector [vec] up to [size], fills vector
    with calls to [f] on indexes [[vec.size-1.. size - 1]].
    The contents and size of vec are untouched if [size] is inferior or equal
    to [length vec].
    @raise Invalid_argument if the size is too big *)

val resize_with_init : 'a t -> init:'a -> int -> unit
(** [resize_with_init vec init size] resizes vector [vec] up to [size],
    fills vector with calls to [init] on indexes [[length vec -1.. size - 1]].
    The contents and size of vec are untouched if [size] is inferior or equal
    to [length vec].
    @raise Invalid_argument if the size is too big *)

val append : 'a t -> 'a t -> unit
(** [append a b] adds all elements of [b] to [a]. *)

val append_array : 'a t -> 'a array -> unit
(** Like {!append}, with an array. *)

val append_iter : 'a t -> 'a iter -> unit
(** Append content of iterator. *)

val append_seq : 'a t -> 'a Seq.t -> unit
(** Append content of iterator.
    Renamed from [append_std_seq] since 3.0. *)

val append_list : 'a t -> 'a list -> unit
(** Append content of list. *)

val equal : 'a equal -> 'a t equal
(** Content-wise equality *)

val compare : 'a ord -> 'a t ord
(** Total ordering on vectors. Lexicographic comparison. *)

exception Empty
(** Raised on empty stack/vector. *)

val pop : 'a t -> 'a option
(** Remove last element, or [None]. *)

val pop_exn : 'a t -> 'a
(** Remove last element, or raise an exception if empty.
    @raise Empty on an empty vector. *)

val top : 'a t -> 'a option
(** Top element, if present. *)

val top_exn : 'a t -> 'a
(** Top element, if present.
    @raise Empty on an empty vector. *)

val copy : 'a t -> 'a t
(** Shallow copy. *)

val truncate : _ t -> int -> unit
(** Truncate to the given size (remove elements above this size).
    Does nothing if the parameter is bigger than the current size. *)

val shrink_to_fit : _ t -> unit
(** Shrink internal array to fit the size of the vector. This will
    most likely reallocate the internal array. *)

val member : ('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** Is the element a member of the vector? *)

val sorted : ('a -> 'a -> int) -> 'a t -> 'a t
(** Sort the vector, returning a copy of it that is sorted
    w.r.t the given ordering. The vector itself is unchanged.
    The underlying array of the new vector can be smaller than
    the original one. *)

val sort : ('a -> 'a -> int) -> 'a t -> unit
(** Sort the vector in place (modifying it).
    This function change the size of the underlying array. *)

val uniq_sort : ('a -> 'a -> int) -> 'a t -> unit
(** Sort the array and remove duplicates, in place (e.g. modifying
    the vector itself). *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on the vector's content. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on the vector, with indexes. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map elements of the vector, yielding a new vector. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [map f v] is just like {!map}, but it also passes in the index
    of each element as the first argument to the function [f]. *)

val map_in_place : ('a -> 'a) -> 'a t -> unit
(** Map elements of the vector in place. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements from the vector. [filter p v] leaves [v] unchanged but
    returns a new vector that only contains elements of [v] satisfying [p]. *)

val filter_in_place : ('a -> bool) -> 'a t -> unit
(** Filter elements from the vector in place. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on elements of the vector *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on elements of the vector. Alias for {!fold_left}. *)

val exists : ('a -> bool) -> 'a t -> bool
(** Existential test (is there an element that satisfies the predicate?). *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Universal test (do all the elements satisfy the predicate?). *)

val find : ('a -> bool) -> 'a t -> 'a option
(** Find an element that satisfies the predicate. *)

val find_exn : ('a -> bool) -> 'a t -> 'a
(** Find an element that satisfies the predicate, or
    @raise Not_found if no element does. *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f v] returns the first [Some y = f x] for [x] in [v],
    or [None] if [f x = None] for each [x] in [v]. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map elements with a function, possibly filtering some of them out. *)

val filter_map_in_place : ('a -> 'a option) -> 'a t -> unit
(** Filter-map elements of the vector in place. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map each element to a sub-vector. *)

val flat_map_seq : ('a -> 'b Seq.t) -> 'a t -> 'b t
(** Like {!flat_map}, but using [Seq] for intermediate collections. *)

val flat_map_list : ('a -> 'b list) -> 'a t -> 'b t
(** Like {!flat_map}, but using {!list} for
    intermediate collections. *)

val cartesian_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** All combinaisons of tuples from the two vectors are passed to the function. *)

val range_inclusive : int -> int -> int t
(** Range of integers, either ascending or descending, where both
    bounds are included, therefore the result is never empty).
    Example: [1 -- 10] returns the vector [[1;2;3;4;5;6;7;8;9;10]]. *)

val range_exclusive : int -> int -> int t
(** Range of integers, either ascending or descending, but excluding the second argument.
    Example: [1 --^ 10] returns the vector [[1;2;3;4;5;6;7;8;9]]. *)

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Infix version of {!flat_map}. *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix version of {!map}. *)

  val ( -- ) : int -> int -> int t
  (** Alias for {!range_inclusive} *)

  val ( --^ ) : int -> int -> int t
  (** Alias for {!range_exclusive} *)

  [@@@ifge 4.08]

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** @since 2.8 *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** @since 2.8 *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** @since 2.8 *)

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  (** @since 2.8 *)

  [@@@endif]
end

include module type of Infix

val get : 'a t -> int -> 'a
(** Access element by its index, or
    @raise Invalid_argument if bad index. *)

val set : 'a t -> int -> 'a -> unit
(** Modify element at given index, or
    @raise Invalid_argument if the index is
    invalid (i.e. not in [[0.. length v-1]]). *)

val remove_and_shift : 'a t -> int -> unit
(** [remove_and_shift v i] remove the [i-th] element from [v].
    Move elements that are after the [i-th] in [v], in linear time.
    Preserve the order of the elements in [v].
    See {!remove_unordered} for constant time removal function that doesn't
    preserve the order of elements. *)

val remove_unordered : 'a t -> int -> unit
(** [remove_unordered v i] remove the [i-th] element from [v].
    Does {b NOT} preserve the order of the elements in [v]
    (might swap with the last element).
    See {!remove_and_shift} if you want to keep the ordering. *)

val insert : 'a t -> int -> 'a -> unit
(** [insert v i x] insert the given element at index i.
    Elements at location [i] and later are first shifted over in linear time before inserting [x].
    Preserve the order of elements in [v]. *)

val rev : 'a t -> 'a t
(** Reverse the vector. *)

val rev_in_place : 'a t -> unit
(** Reverse the vector in place. *)

val rev_iter : ('a -> unit) -> 'a t -> unit
(** [rev_iter f a] is the same as [iter f (rev a)], only more efficient. *)

val size : _ t -> int
(** Number of elements in the vector. *)

val length : _ t -> int
(** Synonym for {! size}. *)

val capacity : _ t -> int
(** Number of elements the vector can contain without being resized. *)

val unsafe_get_array : 'a t -> 'a array
(** Access the underlying {b shared} array (do not modify!).
    [unsafe_get_array v] is longer than [size v], but elements at higher
    index than [size v] are undefined (do not access!). *)

val of_array : 'a array -> 'a t
(** [of_array a] returns a vector corresponding to the array [a]. Operates in [O(n)] time. *)

val of_list : 'a list -> 'a t

val to_array : 'a t -> 'a array
(** [to_array v] returns an array corresponding to the vector [v].
    This allocates a new array. *)

val to_list : 'a t -> 'a list
(** Return a list with the elements contained in the vector. *)

val of_iter : ?init:'a t -> 'a iter -> 'a t
(** Convert an Iterator to a vector. *)

val of_seq : ?init:'a t -> 'a Seq.t -> 'a t
(** Convert an Iterator to a vector. *)

val to_iter : 'a t -> 'a iter
(** Return a [iter] with the elements contained in the vector. *)

val to_iter_rev : 'a t -> 'a iter
(** [to_iter_rev v] returns the sequence of elements of [v] in reverse order,
    that is, the last elements of [v] are iterated on first.
*)

val to_seq : 'a t -> 'a Seq.t
(** Return an iterator with the elements contained in the vector.
    Renamed from [to_std_seq] since 3.0.
*)

val to_seq_rev : 'a t -> 'a Seq.t
(** [to_seq v] returns the sequence of elements of [v] in reverse order,
    that is, the last elements of [v] are iterated on first.
*)

val unsafe_slice : 'a t -> 'a array * int * int
(** Vector as an array slice. By doing it we expose the internal array, so
    be careful!. *)

val slice_iter : 'a t -> int -> int -> 'a iter
(** [slice_iter v start len] is the sequence of elements from [v.(start)]
    to [v.(start+len-1)].
*)

val to_string :
  ?start:string ->
  ?stop:string ->
  ?sep:string ->
  ('a -> string) ->
  'a t ->
  string
(** Print the vector in a string. *)

val pp :
  ?pp_start:unit printer ->
  ?pp_stop:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a t printer
(** [pp ~pp_start ~pp_stop ~pp_sep pp_item ppf v] formats the vector [v] on [ppf].
    Each element is formatted with [pp_item], [pp_start] is called at the beginning,
    [pp_stop] is called at the end, [pp_sep] is called between each elements.
    By defaults [pp_start] and [pp_stop] does nothing and [pp_sep] defaults to
    (fun out -> Format.fprintf out ",@ "). *)
