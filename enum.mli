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

(** {1 Restartable generators} *)

(** This structure is inspired from Ocaml Batteries' BatEnum.t. It features
    restartable generators. *)

exception EOG
  (** End of Generation *)

type 'a t = unit -> 'a generator
  (** An enum is a generator of generators *)
and 'a generator = unit -> 'a
  (** A generator may be called several times, yielding the next value
      each time. It raises EOG when it reaches the end. *)

(** {2 Generator functions} *)

val start : 'a t -> 'a generator
  (** Create a new generator *)

module Gen : sig
  val empty : 'a generator

  val next : 'a generator -> 'a
    (** Get next element, or raise EOG *)

  val junk : 'a generator -> unit
    (** Drop element *)

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a generator -> 'b
    (** Fold over the generator *)

  val iter : ('a -> unit) -> 'a generator -> unit
    (** Iterate on the generator *)

  val length : 'a generator -> int
    (** Consume generator to compute its length *)

  val of_list : 'a list -> 'a generator

  val to_list : 'a generator -> 'a list  (* not tailrec *)

  val to_rev_list : 'a generator -> 'a list

  val int_range : int -> int -> int generator
end

(** {2 Basic constructors} *)

val empty : 'a t
  (** Enmpty enum *)

val singleton : 'a -> 'a t
  (** One-element enum *)

val repeat : 'a -> 'a t
  (** Repeat same element endlessly *)

val iterate : 'a -> ('a -> 'a) -> 'a t
  (** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
  (** Dual of {!fold}, with a deconstructing operation *)

(** {2 Basic combinators} *)

val is_empty : _ t -> bool
  (** Check whether the enum is empty *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on the generator *)

val fold2 : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
  (** Fold on the two enums in parallel. Stops once one of the enums
      is exhausted. *)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
  (** Fold on non-empty sequences (otherwise raise Invalid_argument) *)

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
  (** Successive values of the accumulator *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on the enum *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** Iterate on the two sequences. Stops once one of them is exhausted.*)

val length : _ t -> int
  (** Length of an enum (linear time) *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Lazy map *)

val append : 'a t -> 'a t -> 'a t
  (** Append the two enums *)

val cycle : 'a t -> 'a t
  (** Cycle through the enum, endlessly. The enum must not be empty. *)

val flatten : 'a t t -> 'a t
  (** Flatten the enum of enum *)

val flatMap : ('a -> 'b t) -> 'a t -> 'b t
  (** Monadic bind *)

val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
  (** Is the given element, member of the enum? *)

val take : int -> 'a t -> 'a t
  (** Take at most n elements *)

val drop : int -> 'a t -> 'a t
  (** Drop n elements *)

val nth : int -> 'a t -> 'a
  (** n-th element, or Not_found *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** Filter out elements that do not satisfy the predicate.  *)

val takeWhile : ('a -> bool) -> 'a t -> 'a t
  (** Take elements while they satisfy the predicate *)

val dropWhile : ('a -> bool) -> 'a t -> 'a t
  (** Drop elements while they satisfy the predicate *)

val filterMap : ('a -> 'b option) -> 'a t -> 'b t
  (** Maps some elements to 'b, drop the other ones *)

val zipWith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Combine common part of the enums (stops when one is exhausted) *)

val zip : 'a t -> 'b t -> ('a * 'b) t
  (** Zip together the common part of the enums *)

val zipIndex : 'a t -> (int * 'a) t
  (** Zip elements with their index in the enum *)

val unzip : ('a * 'b) t -> 'a t * 'b t
  (** Unzip into two sequences *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition p l] returns the elements that satisfy [p],
      and the elements that do not satisfy [p] *)

val for_all : ('a -> bool) -> 'a t -> bool
  (** Predicate true for all elements? *)

val exists : ('a -> bool) -> 'a t -> bool
  (** Predicate true for at least one element? *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
  (** Minimum element *)

val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
  (** Maximum element *)

(** {2 Complex combinators} *)

val merge : 'a t t -> 'a t
  (** Pick elements fairly in each sub-enum. The given enum
      must be finite (not its elements, though). The merge of enums
      [e1, e2, ... en] picks one element in [e1], then one element in [e2],
      then in [e3], ..., then in [en], and then starts again at [e1]. Once
      a generator is empty, it is skipped; when they are all empty,
      their merge is also empty. 
      For instance, [merge [1;3;5] [2;4;6]] will be [1;2;3;4;5;6]. *)

(** {3 Mutable heap (taken from heap.ml to avoid dependencies)} *)
module Heap : sig
  type 'a t (** A heap containing values of type 'a *)
  val empty : cmp:('a -> 'a -> int) -> 'a t
  val insert : 'a t -> 'a -> unit
  val is_empty : 'a t -> bool
  val pop : 'a t -> 'a
end

val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  (** Merge two sorted sequences into a sorted sequence *)

val sorted_merge_n : ?cmp:('a -> 'a -> int) -> 'a t t -> 'a t
  (** Sorted merge of multiple sorted sequences *)

val persistent : 'a generator -> 'a t
  (** Store content of the generator in memory, to be able to iterate on it
      several times later *)

val round_robin : ?n:int -> 'a t -> 'a generator t
  (** Split the enum into [n] generators in a fair way. Elements with
      [index = k mod n] with go to the k-th enum. [n] defaults value
      is 2. *)

val tee : ?n:int -> 'a t -> 'a generator t
  (** Duplicate the enum into [n] generators (default 2). The generators
      share the same underlying instance of the enum, so the optimal case is
      when they are consumed evenly *)

val interleave : 'a t -> 'a t -> 'a t
  (** [interleave a b] yields an element of [a], then an element of [b],
      and so on until the end of [a] or [b] is reached. *)

val intersperse : 'a -> 'a t -> 'a t
  (** Put the separator element between all elements of the given enum *)

val product : 'a t -> 'b t -> ('a * 'b) t
  (** Cartesian product *)

val group : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
  (** Group equal consecutive elements together. *)

val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
  (** Remove consecutive duplicate elements. Basically this is
      like [fun e -> map List.hd (group e)]. *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort according to the given comparison function *)

val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort and remove duplicates *)

(* TODO later
val permutations : 'a t -> 'a t t
  (** Permutations of the enum. Each permutation becomes unavailable once
      the next one is produced. *)

val combinations : int -> 'a t -> 'a t t
  (** Combinations of given length. *)

val powerSet : 'a t -> 'a t t
  (** All subsets of the enum (in no particular order) *)
*)

(** {2 Basic conversion functions} *)

val of_list : 'a list -> 'a t
  (** Enumerate the list *)

val to_list : 'a t -> 'a list
  (** non tail-call trasnformation to list *)

val to_rev_list : 'a t -> 'a list
  (** Tail call conversion to list, in reverse order *)

val int_range : int -> int -> int t

val pp : ?start:string -> ?stop:string -> ?sep:string -> ?horizontal:bool ->
         (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty print an enum *)


val (@@) : 'a t -> 'a t -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (--) : int -> int -> int t

val (|>) : 'a -> ('a -> 'b) -> 'b

module Infix : sig
  val (@@) : 'a t -> 'a t -> 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  
  val (--) : int -> int -> int t
  
  val (|>) : 'a -> ('a -> 'b) -> 'b
end

