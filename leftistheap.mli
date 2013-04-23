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

(** {1 Leftist Heaps *)

(** Polymorphic implementation, following Okasaki *)

type 'a t
  (** Heap containing values of type 'a *)

val empty : leq:('a -> 'a -> bool) -> 'a t
  (** Empty heap. The function is used to check whether
      the first element is smaller than the second. *)

val is_empty : _ t -> bool
  (** Is the heap empty? *)

val merge : 'a t -> 'a t -> 'a t
  (** Merge two heaps (assume they have the same comparison function) *)

val insert : 'a t -> 'a -> 'a t
  (** Insert a value in the heap *)

val find_min : 'a t -> 'a
  (** Find minimal element, or raise Not_found *)

val extract_min : 'a t -> 'a t * 'a
  (** Extract and returns the minimal element, or raise Not_found *)

val iter : 'a t -> ('a -> unit) -> unit
  (** Iterate on elements *)

val size : _ t -> int
  (** Number of elements (linear) *)

val of_seq : 'a t -> 'a Sequence.t -> 'a t

val to_seq : 'a t -> 'a Sequence.t
