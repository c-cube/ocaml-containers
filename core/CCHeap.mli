(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Leftist Heaps} following Okasaki *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

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

  (** {2 Conversions} *)

  val to_list : t -> elt list
  val of_list : elt list -> t

  val of_seq : t -> elt sequence -> t
  val to_seq : t -> elt sequence

  val of_klist : t -> elt klist -> t
  val to_klist : t -> elt klist

  val of_gen : t -> elt gen -> t
  val to_gen : t -> elt gen

  val to_tree : t -> elt ktree
end

module Make(E : PARTIAL_ORD) : S with type elt = E.t
