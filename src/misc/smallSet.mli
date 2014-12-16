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

(** {1 Small set structure} *)

(** This set structure is polymorphic, using a user-provided comparison
    function. It is implemented as a sorted list, so most operations
    are in linear time. *)

type 'a sequence = ('a -> unit) -> unit


type 'a t
  (** Set of elements of type 'a *)

val empty : cmp:('a -> 'a -> int) -> 'a t
  (** Create an empty set *)

val is_empty : _ t -> bool
  (** Is the set empty? *)

val mem : 'a t -> 'a -> bool
  (** Is the element member of the set? *)

val add : 'a t -> 'a -> 'a t
  (** add an element *)

val remove : 'a t -> 'a -> 'a t
  (** Remove element *)

val choose : 'a t -> 'a
  (** Some element of the set, of Not_found *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Fold on elements *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on elements *)

val size : _ t -> int
  (** Number of elements *)

val to_seq : 'a t -> 'a sequence

val of_seq : 'a t -> 'a sequence -> 'a t

val to_list : 'a t -> 'a list

val of_list : 'a t -> 'a list -> 'a t
