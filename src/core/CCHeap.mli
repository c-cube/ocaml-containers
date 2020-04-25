
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Leftist Heaps}

    Implementation following Okasaki's book. *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]
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

  val empty : t
  (** [empty] returns the empty heap. *)

  val is_empty : t -> bool
  (** [is_empty h] returns [true] if the heap [h] is empty. *)

  exception Empty

  val merge : t -> t -> t
  (** [merge h1 h2] merges the two heaps [h1] and [h2]. *)

  val insert : elt -> t -> t
  (** [insert x h] inserts an element [x] into the heap [h]. *)

  val add : t -> elt -> t
  (** [add h x] inserts an element [x] into the heap [h]. *)

  val filter :  (elt -> bool) -> t -> t
  (** [filter p h] filters values, only retaining the ones that satisfy the predicate [p].
      Linear time at least. *)

  val find_min : t -> elt option
  (** [find_min h] find the minimal element of the heap [h]. *)

  val find_min_exn : t -> elt
  (** [find_min_exn h] is like {!find_min} but can fail.
      @raise Empty if the heap is empty. *)

  val take : t -> (t * elt) option
  (** [take h] extracts and returns the minimum element, and the new heap (without
      this element), or [None] if the heap [h] is empty. *)

  val take_exn : t -> t * elt
  (** [take_exn h] is like {!take}, but can fail.
      @raise Empty if the heap is empty. *)

  val delete_one : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_one eq x h] uses [eq] to find one occurrence of a value [x]
      if it exist in the heap [h], and delete it.
      If [h] do not contain [x] then it return [h].
      @since 2.0 *)

  val delete_all : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_all eq x h] uses [eq] to find all [x] in [h] and delete them.
      If [h] do not contain [x] then it return [h].
      The difference with {!filter} is that [delete_all] stops as soon as
      it enters a subtree whose root is bigger than the element.
      @since 2.0 *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f h] iterates over the heap [h] invoking [f] with the current element. *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** [fold f acc h] folds on all values of [h]. *)

  val size : t -> int
  (** [size h] is the number of elements in the heap [h]. Linear complexity. *)

  (** {2 Conversions}

      The interface of [of_gen], [of_seq], [of_klist]
      has changed since 0.16 (the old signatures
      are now [add_seq], [add_gen], [add_klist]). *)

  val to_list : t -> elt list
  (** [to_list h] returns the elements of the heap [h], in no particular order. *)

  val to_list_sorted : t -> elt list
  (** [to_list_sorted h] returns the elements of the heap [h] in increasing order.
      @since 1.1 *)

  val add_list : t -> elt list -> t
  (** [add_list h l] adds the elements of the list [l] into the heap [h].
      An element occurring several times will be added that many times to the heap.
      @since 0.16 *)

  val of_list : elt list -> t
  (** [of_list l] is [add_list empty l]. Complexity: [O(n log n)]. *)

  val add_iter : t -> elt iter -> t
  (** [add_iter h iter] is like {!add_list}.
      @since 2.8 *)

  val add_std_seq : t -> elt Seq.t -> t
  (** [add_std_seq h Seq.t] is like {!add_list}.
      @since 2.8 *)

  val of_iter : elt iter -> t
  (** [of_iter iter] builds a heap from a given [iter]. Complexity: [O(n log n)].
      @since 2.8 *)

  val of_std_seq : elt Seq.t -> t
  (** [of_std_seq Seq.t] builds a heap from a given [Seq.t]. Complexity: [O(n log n)].
      @since 2.8 *)

  val to_iter : t -> elt iter
  (** [to_iter h] returns a [iter] of the elements of the heap [h].
      @since 2.8 *)

  val to_std_seq : t -> elt Seq.t
  (** [to_std_seq h] returns a [Seq.t] of the elements of the heap [h].
      @since 2.8 *)

  val to_iter_sorted : t -> elt iter
  (** [to_iter_sorted h] returns a [iter] by iterating on the elements of [h], 
      in increasing order.
      @since 2.8 *)

  val to_std_seq_sorted : t -> elt Seq.t
  (** [to_std_seq_sorted h ] returns a [Seq.t] by iterating on the elements of [h],
      in increasing order.
      @since 2.8 *)

  val add_klist : t -> elt klist -> t
  (** [add_klist h klist] adds the klist [klist] to the heap [h].
      @since 0.16 *)

  val of_klist : elt klist -> t
  (** [of_klist klist] builds a heap from a given [klist]. Complexity: [O(n log n)]. *)

  val to_klist : t -> elt klist
  (** [to_klist h] returns a [klist] of the elements of the heap [h]. *)

  val add_gen : t -> elt gen -> t
  (** [add_gen h gen] adds the gen [gen] to the heap [h].
      @since 0.16 *)

  val of_gen : elt gen -> t
  (** [of_gen gen] builds a heap from a given [gen]. Complexity: [O(n log n)]. *)

  val to_gen : t -> elt gen
  (** [to_gen h] returns a [gen] of the elements of the heap [h]. *)

  val to_tree : t -> elt ktree
  (** [to_tree h] returns a [ktree] of the elements of the heap [h]. *)

  val to_string : ?sep:string -> (elt -> string) -> t -> string
  (**  [to_string ?sep f h] prints the heap [h] in a string
       using [sep] as a given separator (default ",") between each element
       (converted to a string using [f]).
       @since 2.7 *)

  val pp : ?sep:string -> elt printer -> t printer
  (** [pp ?sep ppf h] prints [h] on [ppf].
      Elements are separated by [sep] (default ",").
      Renamed from {!print} since 2.0
      @since 0.16 *)
end

module Make(E : PARTIAL_ORD) : S with type elt = E.t

(** A convenient version of [Make] that take a [TOTAL_ORD] instead of
    a partially ordered module.
    It allow to directly pass modules that implement [compare]
    without implementing [leq] explicitly *)
module Make_from_compare(E : TOTAL_ORD) : S with type elt = E.t
