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

(** {1 Leftist Heaps}
Polymorphic implementation, following Okasaki *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option

type 'a t
  (** Heap containing values of type 'a *)

val empty_with : leq:('a -> 'a -> bool) -> 'a t
  (** Empty heap. The function is used to check whether the first element is
      smaller than the second. *)

val empty : 'a t
  (** Empty heap using [Pervasives.compare] *)

val is_empty : _ t -> bool
  (** Is the heap empty? *)

val merge : 'a t -> 'a t -> 'a t
  (** Merge two heaps (assume they have the same comparison function) *)

val insert : 'a t -> 'a -> 'a t
  (** Insert a value in the heap *)

val add : 'a t -> 'a -> 'a t
  (** Synonym to {!insert} *)

val filter : 'a t -> ('a -> bool) -> 'a t
  (** Filter values, only retaining the ones that satisfy the predicate.
      Linear time at least. *)

val find_min : 'a t -> 'a
  (** Find minimal element, or fails
      @raise Not_found if the heap is empty *)

val extract_min : 'a t -> 'a t * 'a
  (** Extract and returns the minimal element, or
      raise Not_found if the heap is empty *)

val take : 'a t -> ('a * 'a t) option
  (** Extract and return the minimum element, and the new heap (without
      this element), or [None] if the heap is empty *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on all values *)

val size : _ t -> int
  (** Number of elements (linear complexity) *)

val of_seq : 'a t -> 'a sequence -> 'a t
val to_seq : 'a t -> 'a sequence

val of_klist : 'a t -> 'a klist -> 'a t
val to_klist : 'a t -> 'a klist

val of_gen : 'a t -> 'a gen -> 'a t
val to_gen : 'a t -> 'a gen
