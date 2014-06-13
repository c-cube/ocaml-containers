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

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

val create : unit -> 'a t
(** Create a new, empty vector *)

val create_with : ?capacity:int -> 'a -> 'a t
(** Create a new vector, using the given value as a filler.
    @param capacity the size of the underlying array
    {b caution}: the value will likely not be GC'd before the vector is. *)

val make : int -> 'a -> 'a t
(** [make n x] makes a vector of size [n], filled with [x] *)

val init : int -> (int -> 'a) -> 'a t
(** Init the vector with the given function and size *)

val clear : 'a t -> unit
(** clear the content of the vector *)

val ensure : 'a t -> int -> unit
(** Hint to the vector that it should have at least the given capacity.
    Just a hint, will not be enforced if the vector is empty. *)

val is_empty : 'a t -> bool
(** is the vector empty? *)

val push : 'a t -> 'a -> unit
(** add an element at the end of the vector *)

val append : 'a t -> 'a t -> unit
(** [append a b] adds all elements of b to a *)

val append_array : 'a t -> 'a array -> unit
(** same as append, with an array *)

val append_seq : 'a t -> 'a sequence -> unit
(** Append content of sequence *)

val equal : 'a equal -> 'a t equal

val compare : 'a ord -> 'a t ord
(** Lexicographic comparison *)

val pop : 'a t -> 'a option
(** Remove last element, or [None] *)

val pop_exn : 'a t -> 'a
(** remove last element, or raise a Failure if empty
    @raise Failure on an empty vector *)

val copy : 'a t -> 'a t
(** shallow copy *)

val shrink : 'a t -> int -> unit
(** shrink to the given size (remove elements above this size).
    Does nothing if the parameter is bigger than the current size. *)

val member : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** is the element a member of the vector? *)

val sort : ('a -> 'a -> int) -> 'a t -> unit
(** sort the array in place*)

val uniq_sort : ('a -> 'a -> int) -> 'a t -> unit
(** sort the array and remove duplicates in place*)

val iter : ('a -> unit) -> 'a t -> unit
(** iterate on the vector *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** iterate on the vector with indexes *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** map elements of the vector *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** filter elements from vector *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** fold on elements of the vector *)

val exists : ('a -> bool) -> 'a t -> bool
(** existential test *)

val for_all : ('a -> bool) -> 'a t -> bool
(** universal test *)

val find : ('a -> bool) -> 'a t -> 'a option
(** Find an element that satisfies the predicate *)

val find_exn  : ('a -> bool) -> 'a t -> 'a
(** find an element that satisfies the predicate, or
    @raise Not_found if no element does *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map elements with a function, possibly filtering some of them out *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map each element to a sub-vector *)

val flat_map' : ('a -> 'b sequence) -> 'a t -> 'b t
(** Like {!flat_map}, but using {!sequence} for intermediate collections *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val get : 'a t -> int -> 'a
(** access element, or
    @raise Failure if bad index *)

val set : 'a t -> int -> 'a -> unit
(** access element, or
    @raise Failure if bad index *)

val rev : 'a t -> 'a t
(** Reverse the vector *)

val rev' : 'a t -> unit
(** Reverse the vector in place *)

val size : 'a t -> int
(** number of elements in vector *)

val length : _ t -> int
(** Synonym for {! size} *)

val capacity : _ t -> int
(** Number of elements the vector can contain without being resized *)

val unsafe_get_array : 'a t -> 'a array
(** Access the underlying {b shared} array (do not modify!).
    [unsafe_get_array v] is longer than [size v], but elements at higher
    index than [size v] are undefined (do not access!). *)

val (--) : int -> int -> int t
(** Range of integers (both included) *)

val of_array : 'a array -> 'a t
val of_list : 'a list -> 'a t
val to_array : 'a t -> 'a array
val to_list : 'a t -> 'a list

val of_seq : ?init:'a t -> 'a sequence -> 'a t

val to_seq : 'a t -> 'a sequence

val slice : 'a t -> int -> int -> 'a sequence
(** [slice v start len] is the sequence of elements from [v.(start)]
    to [v.(start+len-1)]. *)

val of_klist : ?init:'a t -> 'a klist -> 'a t
val to_klist : 'a t -> 'a klist
val of_gen : ?init:'a t -> 'a gen -> 'a t
val to_gen : 'a t -> 'a gen

val pp : ?start:string -> ?stop:string -> ?sep:string ->
         'a printer -> 'a t printer

val print : ?start:string -> ?stop:string -> ?sep:string ->
            'a formatter -> 'a t formatter
