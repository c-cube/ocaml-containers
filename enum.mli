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

(** {1 Consumable generators} *)

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

val next : 'a generator -> 'a
  (** Get next element, or raise EOG *)

val junk : 'a generator -> unit
  (** Drop element *)

(** {2 Basic constructors} *)

val empty : 'a t
  (** Enmpty enum *)

val singleton : 'a -> 'a t
  (** One-element enum *)

val repeat : 'a -> 'a t
  (** Repeat same element endlessly *)

val iterate : 'a -> ('a -> 'a) -> 'a t
  (** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)

(** {2 Basic combinators} *)

val is_empty : _ t -> bool
  (** Check whether the enum is empty *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on the generator *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on the enum *)

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

val take : int -> 'a t -> 'a t
  (** Take at most n elements *)

val drop : int -> 'a t -> 'a t
  (** Drop n elements *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** Filter out elements that do not satisfy the predicate. The outer
      enum must be finite. *)

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

(** {2 Complex combinators} *)

val round_robin : 'a t t -> 'a t
  (** Pick elements fairly in each sub-enum *)

val persistent : 'a generator -> 'a t
  (** Store content of the generator in memory, to be able to iterate on it
      several times later *)

val tee : ?n:int -> 'a t -> 'a generator t
  (** Split the enum into [n] generators in a fair way. Elements with
      [index = k mod n] with go to the k-th enum. [n] defaults value
      is 2. *)

val interleave : 'a t -> 'a t -> 'a t
  (** [interleave a b] yields an element of [a], then an element of [b],
      and so on until the end of [a] or [b] is reached. *)

val intersperse : 'a -> 'a t -> 'a t
  (** Put the separator element between all elements of the given enum *)

val product : 'a t -> 'b t -> ('a * 'b) t
  (** Cartesian product *)

val permutations : 'a t -> 'a t t
  (** Permutations of the enum *)

val combinations : int -> 'a t -> 'a t t
  (** Combinations of given length *)

(** {2 Basic conversion functions} *)

val of_list : 'a list -> 'a t
  (** Enumerate the list *)

val to_list : 'a t -> 'a list
  (** non tail-call trasnformation to list *)

val to_rev_list : 'a t -> 'a list
  (** Tail call conversion to list, in reverse order *)

val int_range : int -> int -> int t

module Infix : sig
  val (@@) : 'a t -> 'a t -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (--) : int -> int -> int t
  val (|>) : 'a -> ('a -> 'b) -> 'b
end
