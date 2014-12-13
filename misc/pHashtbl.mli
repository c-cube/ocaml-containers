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

(** {1 Open addressing hashtable (robin hood hashing)} *)

type 'a sequence = ('a -> unit) -> unit

type ('a, 'b) t = {
  mutable buckets : ('a, 'b) bucket array;
  mutable size : int;
  eq : 'a -> 'a -> bool;
  hash : 'a -> int;
  max_load : float;
} (** A hashtable is an array of (key, value) buckets that have a state,
      plus the size of the table and equality/hash functions *)
and ('a, 'b) bucket =
  | Empty
  | Deleted
  | Used of 'a * 'b * int  (* int: the distance from home of the key *)
  (** a bucket *)

val create : ?max_load:float -> ?eq:('a -> 'a -> bool) ->
              ?hash:('a -> int) -> int -> ('a, 'b) t
  (** Create a hashtable.  [max_load] is (number of items / size of table),
      and must be in )0, 1(. Functions for equality check and hashing
      can also be provided. *)

module type Hashable = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

val create_tc : (module Hashable with type t = 'a) -> int -> ('a, 'b) t
  (** Create a hashtable from the given 'typeclass' *)

val copy : ('a, 'b) t -> ('a, 'b) t
  (** Copy of the hashtable *)

val clear : (_, _) t -> unit
  (** Clear the content of the hashtable *)

val find : ('a, 'b) t -> 'a -> 'b
  (** Find the value for this key, or raise Not_found *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
  (** Add/replace the binding for this key. O(1) amortized. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
  (** Alias for [replace] *)

val remove : ('a, _) t -> 'a -> unit
  (** Remove the binding for this key, if any *)

val length : (_, _) t -> int
  (** Number of bindings in the table *)

val mem : ('a,_) t -> 'a -> bool
  (** Is the key present in the hashtable? *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  (** Iterate on bindings *)

val map : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Map, replaces values by other values *)

val filter : ('a -> 'b -> bool) -> ('a, 'b) t -> unit
  (** Destructive filter (remove bindings that do not satisfiy predicate) *)

val fold : ('c -> 'a -> 'b -> 'c) -> 'c -> ('a, 'b) t -> 'c
  (** Fold on bindings *)

val of_seq : ('a, 'b) t -> ('a * 'b) sequence -> unit
  (** Add the given pairs to the hashtable *)

val to_seq : ('a, 'b) t -> ('a * 'b) sequence
  (** Sequence of pairs *)

val stats : (_, _) t -> int * int * int * int * int * int
  (** Cf Weak.S *)

val get_eq : ('v, _) t -> ('v -> 'v -> bool)

val get_hash : ('v, _) t -> ('v -> int)
