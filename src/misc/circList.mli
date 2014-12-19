
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 Functional Circular List}

Those are infinite lists that are built from a finite list of
elements, and cycles through them.
Unless specified otherwise, operations have an amortized cost in O(1). *)

type +'a t

val singleton : 'a -> 'a t
(** list that cycles on one element *)

val of_list : 'a list -> 'a t
(** build a circular list from a list. Linear in the length
    of the list.
    @raise Invalid_argument if the list is empty *)

val length : 'a t -> int
(** length of the cycle. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] adds [x] at the beginning of [l] *)

val snoc : 'a t -> 'a -> 'a t
(** [snoc l x] adds [x] at the end of [l] *)

val next : 'a t -> 'a * 'a t
(** obtain the next element, and the list rotated by one. *)

val rev : 'a t -> 'a t
(** reverse the traversal (goes right-to-left from now). *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p l] returns [Some x] where [p x] is [true]
    and [x] belongs to [l], or [None] if no such
    element exists *)

val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** does the element belong to the infinite list? *)

val exists : ('a -> bool) -> 'a t -> bool

val for_all : ('a -> bool) -> 'a t -> bool

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** fold through each element of the list exactly once. *)

(** {2 Iterators} *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

val gen : 'a t -> 'a gen
(** CCGenerator on elements of the list *)

val seq : 'a t -> 'a sequence
(** CCSequence of elements of the list *)
