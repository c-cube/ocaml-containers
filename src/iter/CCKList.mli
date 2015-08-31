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

(** {1 Continuation List} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type + 'a t = unit ->
  [ `Nil
  | `Cons of 'a * 'a t
  ]

val nil : 'a t

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

val singleton : 'a -> 'a t

val repeat : ?n:int -> 'a -> 'a t
(** [repeat ~n x] repeats [x] [n] times then stops. If [n] is omitted,
    then [x] is repeated forever.
    @since 0.3.3 *)

val cycle : 'a t -> 'a t
(** Cycle through the iterator infinitely. The iterator shouldn't be empty.
    @since 0.3.3 *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** [unfold f acc] calls [f acc] and:
    - if [f acc = Some (x, acc')], yield [x], continue with [unfold f acc']
    - if [f acc = None], stops
    @since NEXT_RELEASE *)

val is_empty : 'a t -> bool

val equal : 'a equal -> 'a t equal
(** Equality step by step. Eager. *)

val compare : 'a ord -> 'a t ord
(** Lexicographic comparison. Eager. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on values *)

val iter : ('a -> unit) -> 'a t -> unit

val length : _ t -> int

val take : int -> 'a t -> 'a t

val take_while : ('a -> bool) -> 'a t -> 'a t

val drop : int -> 'a t -> 'a t

val drop_while : ('a -> bool) -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val fmap : ('a -> 'b option) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t

val product_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Fair product of two (possibly infinite) lists into a new list. Lazy.
    The first parameter is used to combine each pair of elements
    @since 0.3.3 *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Specialization of {!product_with} producing tuples
    @since 0.3.3 *)

val group : 'a equal -> 'a t -> 'a t t
(** [group eq l] groups together consecutive elements that satisfy [eq]. Lazy.
    For instance [group (=) [1;1;1;2;2;3;3;1]] yields
      [[1;1;1]; [2;2]; [3;3]; [1]]
    @since 0.3.3 *)

val uniq : 'a equal -> 'a t -> 'a t
(** [uniq eq l] returns [l] but removes consecutive duplicates. Lazy.
    In other words, if several values that are equal follow one another,
    only the first of them is kept.
    @since 0.3.3 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flatten : 'a t t -> 'a t

val range : int -> int -> int t

val (--) : int -> int -> int t

(** {2 Operations on two Collections} *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** Fold on two collections at once. Stop at soon as one of them ends *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Map on two collections at once. Stop as soon as one of the
    arguments is exhausted *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on two collections at once. Stop as soon as one of them ends *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val merge : 'a ord -> 'a t -> 'a t -> 'a t
(** Merge two sorted iterators into a sorted iterator *)

val sort : ?cmp:'a ord -> 'a t -> 'a t
(** Eager sort. Requires the iterator to be finite. O(n ln(n)) time
    and space.
    @since 0.3.3 *)

val sort_uniq : ?cmp:'a ord -> 'a t -> 'a t
(** Eager sort that removes duplicate values. Requires the iterator to be
    finite. O(n ln(n)) time and space.
    @since 0.3.3 *)

(** {2 Implementations}
    @since 0.3.3 *)

val return : 'a -> 'a t
val pure : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t
end

(** {2 Conversions} *)

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list
(** Gather all values into a list *)

val to_rev_list : 'a t -> 'a list
(** Convert to a list, in reverse order. More efficient than {!to_list} *)

val to_seq : 'a t -> 'a sequence

val to_gen : 'a t -> 'a gen

val of_gen : 'a gen -> 'a t
(** [of_gen g] consumes the generator and caches intermediate results
    @since NEXT_RELEASE *)

(** {2 IO} *)

val pp : ?sep:string -> 'a printer -> 'a t printer

val print : ?sep:string -> 'a formatter -> 'a t formatter
