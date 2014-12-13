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

(** {1 Imperative priority queue} *)

type 'a sequence = ('a -> unit) -> unit

type 'a t
  (** A heap containing values of type 'a *)

val empty : cmp:('a -> 'a -> int) -> 'a t
  (** Create an empty heap *)

val insert : 'a t -> 'a -> unit
  (** Insert a value in the heap *)

val is_empty : 'a t -> bool
  (** Check whether the heap is empty *)

val min : 'a t -> 'a
  (** Access the minimal value of the heap, or raises Invalid_argument *)

val junk : 'a t -> unit
  (** Discard the minimal element *)

val pop : 'a t -> 'a
  (** Remove and return the mininal value (or raise Invalid_argument) *)

val iter : 'a t -> ('a -> unit) -> unit
  (** Iterate on the elements, in an unspecified order *)

val size : _ t -> int

val to_seq : 'a t -> 'a sequence

val of_seq : 'a t -> 'a sequence -> unit
