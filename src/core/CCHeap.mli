
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
  (** [leq x y] shall return [true] iff [x] is lower or equal to [y] *)
end

module type S = sig
  type elt
  type t

  val empty : t
  (** Empty heap *)

  val is_empty : t -> bool
  (** Is the heap empty? *)

  exception Empty

  val merge : t -> t -> t
  (** Merge two heaps *)

  val insert : elt -> t -> t
  (** Insert a value in the heap *)

  val add : t -> elt -> t
  (** Synonym to {!insert} *)

  val filter :  (elt -> bool) -> t -> t
  (** Filter values, only retaining the ones that satisfy the predicate.
      Linear time at least. *)

  val find_min : t -> elt option
  (** Find minimal element *)

  val find_min_exn : t -> elt
  (** Same as {!find_min} but can fail
      @raise Empty if the heap is empty *)

  val take : t -> (t * elt) option
  (** Extract and return the minimum element, and the new heap (without
      this element), or [None] if the heap is empty *)

  val take_exn : t -> t * elt
  (** Same as {!take}, but can fail.
      @raise Empty if the heap is empty *)

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on elements *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Fold on all values *)

  val size : t -> int
  (** Number of elements (linear complexity) *)

  (** {2 Conversions}

      The interface of [of_gen], [of_seq], [of_klist]
      has changed @since 0.16 (the old signatures
      are now [add_seq], [add_gen], [add_klist]) *)

  val to_list : t -> elt list

  val add_list : t -> elt list -> t (** @since 0.16 *)

  val of_list : elt list -> t

  val add_seq : t -> elt sequence -> t (** @since 0.16 *)

  val of_seq : elt sequence -> t

  val to_seq : t -> elt sequence

  val add_klist : t -> elt klist -> t (** @since 0.16 *)

  val of_klist : elt klist -> t

  val to_klist : t -> elt klist

  val add_gen : t -> elt gen -> t (** @since 0.16 *)

  val of_gen : elt gen -> t

  val to_gen : t -> elt gen

  val to_tree : t -> elt ktree

  val print : ?sep:string -> elt printer -> t printer
  (** @since 0.16 *)
end

module Make(E : PARTIAL_ORD) : S with type elt = E.t
