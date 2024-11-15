(* This file is free software, part of containers. See file "license" for more details. *)

(** Leftist Heaps

    Implementation following Okasaki's book. *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a gen = unit -> 'a option
type 'a ktree = unit -> [ `Nil | `Node of 'a * 'a ktree list ]
type 'a printer = Format.formatter -> 'a -> unit

module type PARTIAL_ORD = sig
  type t

  val leq : t -> t -> bool
  (** [leq x y] shall return [true] iff [x] is lower or equal to [y]. *)
end

module type TOTAL_ORD = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] shall return
      a negative value if [a] is smaller than [b],
      [0] if [a] and [b] are equal or
      a positive value if [a] is greater than [b] *)
end

module type S = sig
  type elt
  type t

  exception Empty

  (** {2 Basic heap operations} *)

  val empty : t
  (** [empty] returns the empty heap. *)

  val is_empty : t -> bool
  (** [is_empty h] returns [true] iff the heap [h] is empty. *)

  val merge : t -> t -> t
  (** [merge h1 h2] merges the two heaps [h1] and [h2].
      If one heap is empty, the result is physically equal to the other heap.
      Complexity: [O(log (m+n))] where [m] and [n] are the number of elements in each heap.
  *)

  val insert : elt -> t -> t
  (** [insert x h] inserts an element [x] into the heap [h].
      Complexity: [O(log n)] where [n] is the number of elements in [h].
  *)

  val add : t -> elt -> t
  (** [add h x] is [insert x h]. *)

  val find_min : t -> elt option
  (** [find_min h] returns the minimal element of [h],
      or [None] if [h] is empty.
      Complexity: [O(1)].
  *)

  val find_min_exn : t -> elt
  (** [find_min_exn h] is akin to {!find_min},
      but it raises {!Empty} when the heap is empty.
      @raise Empty if the heap is empty. *)

  val take : t -> (t * elt) option
  (** [take h] returns the minimum element of [h]
      and the new heap without this element,
      or [None] if [h] is empty.
      Complexity: [O(log n)].
  *)

  val take_exn : t -> t * elt
  (** [take_exn h] is akin to {!take},
      but it raises {!Empty} when the heap is empty.
      @raise Empty if the heap is empty. *)

  val size : t -> int
  (** [size h] is the number of elements in the heap [h].
      Complexity: [O(n)].
  *)

  (** {2 Deleting elements} *)

  val delete_one : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_one eq x h] deletes an occurrence of the value [x] from the heap [h],
      if there is some.
      If [h] does not contain [x], then [h] itself is returned.
      Elements are identified by the equality function [eq].
      Complexity: [O(n)].
      @since 2.0 *)

  val delete_all : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_all eq x h] deletes all occurrences of the value [x] from the heap [h].
      If [h] does not contain [x], then [h] itself is returned.
      Elements are identified by the equality function [eq].
      This function is more efficient than {!filter}
      because it avoids considering elements greater than [x].
      Complexity: [O(n)].
      @since 2.0 *)

  val filter : (elt -> bool) -> t -> t
  (** [filter p h] filters the elements of [h],
      only retaining those that satisfy the predicate [p].
      If no element in [h] satisfies [p], then [h] itself is returned.
      Complexity: [O(n)].
  *)

  (** {2 Iterating on elements} *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f h] invokes [f] on every element of the heap [h]. *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** [fold f acc h] folds on all elements of [h]. *)

  (** {2 Adding many elements at once} *)

  val add_list : t -> elt list -> t
  (** [add_list h l] adds the elements of the list [l] into the heap [h].
      An element occurring several times will be added that many times to the heap.
      Elements need not be given in any particular order.
      This function is more efficient than repeated insertions.
      Complexity: [O(log m + n)]
      where [m] and [n] are the number of elements in [h] and [l], respectively.
      @since 0.16 *)

  val add_iter : t -> elt iter -> t
  (** [add_iter h iter] is akin to {!add_list},
      but taking an {!type:iter} of elements as input.
      @since 2.8 *)

  val add_seq : t -> elt Seq.t -> t
  (** [add_seq h seq] is akin to {!add_list},
      but taking a [Seq.t] of elements as input.
      Renamed from [add_std_seq] since 3.0.
      @since 3.0 *)

  val add_gen : t -> elt gen -> t
  (** [add_gen h gen] is akin to {!add_list},
      but taking a {!type:gen} of elements as input.
      @since 0.16 *)

  val add_iter_almost_sorted : t -> elt iter -> t
  (** [add_iter_almost_sorted h iter] is equivalent to
      [merge h (of_iter_almost_sorted iter)].
      See {!of_iter_almost_sorted}.
      Complexity: [O(log m + n)].
      @since 3.14
  *)

  (** {2 Conversions} *)

  val of_list : elt list -> t
  (** [of_list l] builds a heap from the list of elements [l].
      Elements need not be given in any particular order.
      This function is more efficient than repeated insertions.
      It is equivalent to {!add_list}[ empty l].
      Complexity: [O(n)].
  *)

  val of_iter : elt iter -> t
  (** [of_iter iter] is akin to {!of_list},
      but taking an {!type:iter} of elements as input.
      @since 2.8 *)

  val of_seq : elt Seq.t -> t
  (** [of_seq seq] is akin to {!of_list},
      but taking a [Seq.t] of elements as input.
      Renamed from [of_std_seq] since 3.0.
      @since 3.0 *)

  val of_gen : elt gen -> t
  (** [of_gen gen] is akin to {!of_list},
      but taking a {!type:gen} of elements as input. *)

  val of_iter_almost_sorted : elt iter -> t
  (** [of_iter iter] builds a heap from the {!type:iter} sequence of elements.
      Elements need not be given in any particular order.
      However, the heap takes advantage of partial sorting found in the input:
      the closer the input sequence is to being sorted,
      the more efficient it is to convert the heap to a sorted sequence.
      This enables heap-sorting that is faster than [O(n log n)]
      when the input is almost sorted.
      In the best case, when only a constant number of elements are misplaced,
      then successive {!take} run in [O(1)],
      and {!to_list_sorted} runs in [O(n)].
      Complexity: [O(n)].
      @since 3.14
  *)

  val to_list : t -> elt list
  (** [to_list h] returns a list of the elements of the heap [h],
      in no particular order.
      Complexity: [O(n)].
  *)

  val to_iter : t -> elt iter
  (** [to_iter h] is akin to {!to_list}, but returning an {!type:iter} of elements.
      @since 2.8 *)

  val to_seq : t -> elt Seq.t
  (** [to_seq h] is akin to {!to_list}, but returning a [Seq.t] of elements.
      Renamed from [to_std_seq] since 3.0.
      @since 3.0 *)

  val to_gen : t -> elt gen
  (** [to_gen h] is akin to {!to_list}, but returning a {!type:gen} of elements. *)

  val to_list_sorted : t -> elt list
  (** [to_list_sorted h] returns the list of elements of the heap [h]
      in increasing order.
      Complexity: [O(n log n)].
      @since 1.1 *)

  val to_iter_sorted : t -> elt iter
  (** [to_iter_sorted h] is akin to {!to_list_sorted},
      but returning an {!type:iter} of elements.
      @since 2.8 *)

  val to_seq_sorted : t -> elt Seq.t
  (** [to_seq_sorted h] is akin to {!to_list_sorted},
      but returning a [Seq.t] of elements.
      Renamed from [to_std_seq_sorted] since 3.0.
      @since 3.0 *)

  val to_tree : t -> elt ktree
  (** [to_tree h] returns a {!type:ktree} of the elements of the heap [h].
      The layout is not specified.
      Complexity: [O(n)].
  *)

  (** {2 Pretty-printing} *)

  val to_string : ?sep:string -> (elt -> string) -> t -> string
  (**  [to_string ?sep f h] prints the heap [h] to a string,
       using [f] to convert elements to strings
       and [sep] (default: [","]) as a separator between elements.
       @since 2.7 *)

  val pp :
    ?pp_start:unit printer ->
    ?pp_stop:unit printer ->
    ?pp_sep:unit printer ->
    elt printer ->
    t printer
  (** [pp ?pp_start ?pp_stop ?pp_sep ppf h] prints [h] on [ppf].
      Each element is formatted with [ppf], [pp_start] is called at the beginning,
      [pp_stop] is called at the end, [pp_sep] is called between each element.
      By default, [pp_start] and [pp_stop] do nothing, and [pp_sep] is
      [(fun out -> Format.fprintf out ",@ ")].
      Renamed from [print] since 2.0
      @since 0.16 *)
end

module Make (E : PARTIAL_ORD) : S with type elt = E.t

(** A convenient version of [Make] that takes a [TOTAL_ORD] instead of
    a partially ordered module.
    It allows to directly pass modules that implement [compare]
    without implementing [leq] explicitly. *)
module Make_from_compare (E : TOTAL_ORD) : S with type elt = E.t
