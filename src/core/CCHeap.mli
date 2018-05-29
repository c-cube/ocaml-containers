
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Leftist Heaps} following Okasaki *)

type 'a sequence = ('a -> unit) -> unit
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
  (** Empty heap. *)

  val is_empty : t -> bool
  (** Is the heap empty? *)

  exception Empty

  val merge : t -> t -> t
  (** Merge two heaps. *)

  val insert : elt -> t -> t
  (** Insert a value in the heap. *)

  val add : t -> elt -> t
  (** Synonym to {!insert}. *)

  val filter :  (elt -> bool) -> t -> t
  (** Filter values, only retaining the ones that satisfy the predicate.
      Linear time at least. *)

  val find_min : t -> elt option
  (** Find minimal element. *)

  val find_min_exn : t -> elt
  (** Like {!find_min} but can fail.
      @raise Empty if the heap is empty. *)

  val take : t -> (t * elt) option
  (** Extract and return the minimum element, and the new heap (without
      this element), or [None] if the heap is empty. *)

  val take_exn : t -> t * elt
  (** Like {!take}, but can fail.
      @raise Empty if the heap is empty. *)

  val delete_one : (elt -> elt -> bool) -> elt -> t -> t
  (** Delete one occurrence of a value if it exist in the heap.
      [delete_one eq x h], use [eq] to find one [x] in [h] and delete it.
      If [h] do not contain [x] then it return [h].
      @since 2.0 *)

  val delete_all : (elt -> elt -> bool) -> elt -> t -> t
  (** Delete all occurrences of a value in the heap.
      [delete_all eq x h], use [eq] to find all [x] in [h] and delete them.
      If [h] do not contain [x] then it return [h].
      The difference with {!filter} is that [delete_all] stops as soon as
      it enters a subtree whose root is bigger than the element.
      @since 2.0 *)

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on elements. *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Fold on all values. *)

  val size : t -> int
  (** Number of elements (linear complexity). *)

  (** {2 Conversions}

      The interface of [of_gen], [of_seq], [of_klist]
      has changed since 0.16 (the old signatures
      are now [add_seq], [add_gen], [add_klist]). *)

  val to_list : t -> elt list
  (** Return the elements of the heap, in no particular order. *)

  val to_list_sorted : t -> elt list
  (** Return the elements in increasing order.
      @since 1.1 *)

  val add_list : t -> elt list -> t
  (** Add the elements of the list to the heap. An element occurring several
      times will be added that many times to the heap.
      @since 0.16 *)

  val of_list : elt list -> t
  (** [of_list l] is [add_list empty l]. Complexity: [O(n log n)]. *)

  val add_seq : t -> elt sequence -> t (** @since 0.16 *)
  (** Like {!add_list}. *)

  val of_seq : elt sequence -> t
  (** Build a heap from a given [sequence]. Complexity: [O(n log n)]. *)

  val to_seq : t -> elt sequence
  (** Return a [sequence] of the elements of the heap. *)

  val to_seq_sorted : t -> elt sequence
  (** Iterate on the elements, in increasing order.
      @since 1.1 *)

  val add_klist : t -> elt klist -> t (** @since 0.16 *)

  val of_klist : elt klist -> t
  (** Build a heap from a given [klist]. Complexity: [O(n log n)]. *)

  val to_klist : t -> elt klist
  (** Return a [klist] of the elements of the heap. *)

  val add_gen : t -> elt gen -> t (** @since 0.16 *)

  val of_gen : elt gen -> t
  (** Build a heap from a given [gen]. Complexity: [O(n log n)]. *)

  val to_gen : t -> elt gen
  (** Return a [gen] of the elements of the heap. *)

  val to_tree : t -> elt ktree
  (** Return a [ktree] of the elements of the heap. *)

  val pp : ?sep:string -> elt printer -> t printer
  (** Printer.
      Renamed from {!print} since 2.0
      @since 0.16 *)
end

module Make(E : PARTIAL_ORD) : S with type elt = E.t

(** A convenient version of [Make] that take a [TOTAL_ORD] instead of
    a partially ordered module.
    It allow to directly pass modules that implement [compare]
    without implementing [leq] explicitly *)
module Make_from_compare(E : TOTAL_ORD) : S with type elt = E.t
