
(* This code is extracted from Msat ( https://github.com/Gbury/mSAT ). *)

(** {1 Imperative Heaps} *)

module type RANKED = sig
  type t
  val idx: t -> int (** Index in heap. return -1 if never set *)
  val set_idx : t -> int -> unit (** Update index in heap *)
  val lt : t -> t -> bool (** [cmp a b] is true iff [a < b] *)
end

module type S = sig
  type elt
  (** Type of elements *)

  type t
  (** Heap of {!elt}, whose priority is increased or decreased
      incrementally (see {!decrease} for instance) *)

  val create : unit -> t
  (** Create a heap *)

  val decrease : t -> elt -> unit
  (** [decrease h x] decreases the value associated to [x] within [h] *)

  val increase : t -> elt -> unit
  (** [increase h x] increases the value associated to [x] within [h] *)

  val in_heap : elt -> bool

  val size : t -> int
  (** Number of integers within the heap *)

  val is_empty : t -> bool

  val clear : t -> unit
  (** Clear the content of the heap *)

  val insert : t -> elt -> unit
  (** Insert a new element into the heap *)

  (*val update : (int -> int -> bool) -> t -> int -> unit*)

  val remove_min : t -> elt
  (** Remove and return the integer that has the lowest value from the heap
      @raise Not_found if the heap is empty *)

  val filter : t -> (elt -> bool) -> unit
  (** Filter out values that don't satisfy the predicate *)
end
