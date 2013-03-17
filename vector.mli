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

(** {1 Growable, mutable vector} *)

type 'a t
  (** the type of a vector of 'a *)

val create : int -> 'a t
  (** create a vector of given initial capacity *)

val clear : 'a t -> unit
  (** clear the content of the vector *)

val is_empty : 'a t -> bool
  (** is the vector empty? *)

val push : 'a t -> 'a -> unit
  (** add an element at the end of the vector *)

val append : 'a t -> 'a t -> unit
  (** [append a b] adds all elements of b to a *)

val append_array : 'a t -> 'a array -> unit
  (** same as append, with an array *)

val pop : 'a t -> 'a
  (** remove last element, or raise a Failure if empty *)

val copy : 'a t -> 'a t
  (** shallow copy *)

val shrink : 'a t -> int -> unit
  (** shrink to the given size (remove elements above this size) *)

val member : ?cmp:('a -> 'a -> bool) -> 'a t -> 'a -> bool
  (** is the element a member of the vector? *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> unit
  (** sort the array in place*)

val uniq_sort : ?cmp:('a -> 'a -> int) -> 'a t -> unit
  (** sort the array and remove duplicates in place*)

val iter : 'a t -> ('a -> unit) -> unit
  (** iterate on the vector *)

val iteri : 'a t -> (int -> 'a -> unit) -> unit
  (** iterate on the vector with indexes *)

val map : 'a t -> ('a -> 'b) -> 'b t
  (** map elements of the vector *)

val filter : 'a t -> ('a -> bool) -> 'a t
  (** filter elements from vector *)

val fold : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
  (** fold on elements of the vector *)

val exists : 'a t -> ('a -> bool) -> bool
  (** existential test *)

val for_all : 'a t -> ('a -> bool) -> bool
  (** universal test *)

val find : 'a t -> ('a -> bool) -> 'a
  (** find an element that satisfies the predicate, or Not_found *)

val get : 'a t -> int -> 'a
  (** access element, or Failure if bad index *)

val set : 'a t -> int -> 'a -> unit
  (** access element, or Failure if bad index *)

val size : 'a t -> int
  (** number of elements in vector *)

val unsafe_get_array : 'a t -> 'a array
  (** Access the underlying *shared* array (do not modify!) *)

val of_seq : ?init:'a t -> 'a Sequence.t -> 'a t
val to_seq : 'a t -> 'a Sequence.t

val from_array : 'a array -> 'a t
val from_list : 'a list -> 'a t
val to_array : 'a t -> 'a array
val to_list : 'a t -> 'a list

