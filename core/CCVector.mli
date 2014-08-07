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

type ro = [`RO]
type rw = [`RW]

(** Mutability is [rw] (read-write) or [ro] (read-only) *)

type ('a, 'mut) t
(** the type of a vector of elements of type ['a], with
    a mutability flat ['mut] *)

type 'a vector = ('a, rw) t
(** Type synonym: a ['a vector] is mutable. *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

val freeze : ('a, _) t -> ('a, ro) t
(** Make an immutable vector (no copy! Don't use the old version)*)

val freeze_copy : ('a, _) t -> ('a, ro) t
(** Copy the vector into an immutable version *)

val create : unit -> ('a, rw) t
(** Create a new, empty vector *)

val create_with : ?capacity:int -> 'a -> ('a, rw) t
(** Create a new vector, using the given value as a filler.
    @param capacity the size of the underlying array
    {b caution}: the value will likely not be GC'd before the vector is. *)

val make : int -> 'a -> ('a, 'mut) t
(** [make n x] makes a vector of size [n], filled with [x] *)

val init : int -> (int -> 'a) -> ('a, 'mut) t
(** Init the vector with the given function and size *)

val clear : ('a, rw) t -> unit
(** clear the content of the vector *)

val ensure : ('a, rw) t -> int -> unit
(** Hint to the vector that it should have at least the given capacity.
    Just a hint, will not be enforced if the vector is empty. *)

val is_empty : ('a, _) t -> bool
(** is the vector empty? *)

val push : ('a, rw) t -> 'a -> unit
(** add an element at the end of the vector *)

val append : ('a, rw) t -> ('a, _) t -> unit
(** [append a b] adds all elements of b to a *)

val append_array : ('a, rw) t -> 'a array -> unit
(** same as append, with an array *)

val append_seq : ('a, rw) t -> 'a sequence -> unit
(** Append content of sequence *)

val equal : 'a equal -> ('a,_) t equal

val compare : 'a ord -> ('a,_) t ord
(** Lexicographic comparison *)

val pop : ('a, rw) t -> 'a option
(** Remove last element, or [None] *)

val pop_exn : ('a, rw) t -> 'a
(** remove last element, or raise a Failure if empty
    @raise Failure on an empty vector *)

val copy : ('a,_) t -> ('a,'mut) t
(** Shallow copy (may give an immutable or mutable vector) *)

val shrink : ('a, rw) t -> int -> unit
(** shrink to the given size (remove elements above this size).
    Does nothing if the parameter is bigger than the current size. *)

val member : ?eq:('a -> 'a -> bool) -> 'a -> ('a, _) t -> bool
(** is the element a member of the vector? *)

val sort : ('a -> 'a -> int) -> ('a, _) t -> ('a, 'mut) t
(** Sort the vector, returning a copy of it that is sorted
    w.r.t the given ordering. The vector itself is unchanged. *)

val sort' : ('a -> 'a -> int) -> ('a, rw) t -> unit
(** Sort the vector in place (modifying it). *)

val uniq_sort : ('a -> 'a -> int) -> ('a, rw) t -> unit
(** Sort the array and remove duplicates, in place (e.e. modifying
    the vector itself) *)

val iter : ('a -> unit) -> ('a,_) t -> unit
(** iterate on the vector's content *)

val iteri : (int -> 'a -> unit) -> ('a,_) t -> unit
(** iterate on the vector, with indexes *)

val map : ('a -> 'b) -> ('a,_) t -> ('b, 'mut) t
(** map elements of the vector, yielding a new vector *)

val filter : ('a -> bool) -> ('a,_) t -> ('a, 'mut) t
(** filter elements from the vector. [filter p v] leaves [v] unchanged but
    returns a new vector that only contains elements of [v] satisfying [p]. *)

val filter' : ('a -> bool) -> ('a, rw) t -> unit
(** Filter elements in place. Does {b NOT} preserve the order
    of the elements. *)

val fold : ('b -> 'a -> 'b) -> 'b -> ('a,_) t -> 'b
(** fold on elements of the vector *)

val exists : ('a -> bool) -> ('a,_) t -> bool
(** existential test (is there an element that satisfies the predicate?) *)

val for_all : ('a -> bool) -> ('a,_) t -> bool
(** universal test (do all the elements satisfy the predicate?) *)

val find : ('a -> bool) -> ('a,_) t -> 'a option
(** Find an element that satisfies the predicate *)

val find_exn  : ('a -> bool) -> ('a,_) t -> 'a
(** find an element that satisfies the predicate, or
    @raise Not_found if no element does *)

val filter_map : ('a -> 'b option) -> ('a,_) t -> ('b, 'mut) t
(** Map elements with a function, possibly filtering some of them out *)

val flat_map : ('a -> ('b,_) t) -> ('a,_) t -> ('b, 'mut) t
(** Map each element to a sub-vector *)

val flat_map' : ('a -> 'b sequence) -> ('a,_) t -> ('b, 'mut) t
(** Like {!flat_map}, but using {!sequence} for intermediate collections *)

val (>>=) : ('a,_) t -> ('a -> ('b,_) t) -> ('b, 'mut) t
(** Infix version of {!flat_map} *)

val (>|=) : ('a,_) t -> ('a -> 'b) -> ('b, 'mut) t
(** Infix version of {!map} *)

val get : ('a,_) t -> int -> 'a
(** access element by its index, or
    @raise Failure if bad index *)

val set : ('a, rw) t -> int -> 'a -> unit
(** modify element at given index, or
    @raise Failure if bad index *)

val remove : ('a, rw) t -> int -> unit
(** Remove the [n-th] element of the vector. Does {b NOT} preserve the order
    of the elements (might swap with the last element) *)

val rev : ('a,_) t -> ('a, 'mut) t
(** Reverse the vector *)

val rev' : ('a, rw) t -> unit
(** Reverse the vector in place *)

val size : ('a,_) t -> int
(** number of elements in vector *)

val length : (_,_) t -> int
(** Synonym for {! size} *)

val capacity : (_,_) t -> int
(** Number of elements the vector can contain without being resized *)

val unsafe_get_array : ('a, rw) t -> 'a array
(** Access the underlying {b shared} array (do not modify!).
    [unsafe_get_array v] is longer than [size v], but elements at higher
    index than [size v] are undefined (do not access!). *)

val (--) : int -> int -> (int, 'mut) t
(** Range of integers, either ascending or descending (both included,
    therefore the result is never empty).
    Example: [1 -- 10] returns the vector [[1;2;3;4;5;6;7;8;9;10]] *)

val of_array : 'a array -> ('a, 'mut) t
val of_list : 'a list -> ('a, 'mut) t
val to_array : ('a,_) t -> 'a array
val to_list : ('a,_) t -> 'a list

val of_seq : ?init:('a,rw) t -> 'a sequence -> ('a, rw) t

val to_seq : ('a,_) t -> 'a sequence

val slice : ('a,rw) t -> ('a array * int * int)
(** Vector as an array slice. By doing it we expose the internal array, so
    be careful! *)

val slice_seq : ('a,_) t -> int -> int -> 'a sequence
(** [slice_seq v start len] is the sequence of elements from [v.(start)]
    to [v.(start+len-1)]. *)

val of_klist : ?init:('a, rw) t -> 'a klist -> ('a, rw) t
val to_klist : ('a,_) t -> 'a klist
val of_gen : ?init:('a, rw) t -> 'a gen -> ('a, rw) t
val to_gen : ('a,_) t -> 'a gen

val pp : ?start:string -> ?stop:string -> ?sep:string ->
         'a printer -> ('a,_) t printer

val print : ?start:string -> ?stop:string -> ?sep:string ->
            'a formatter -> ('a,_) t formatter
