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

(** {1 Functional Maps} *)

(* TODO: map-wide operations: merge, compare, equal, for_all, exists,
        batch (sorted) add, partition, split, max_elt, min_elt, map... *)

type 'a sequence = ('a -> unit) -> unit


(** {2 Polymorphic Maps} *)

type ('a, 'b) t
  (** Tree with keys of type 'a, and values of type 'b *)

val empty_with : cmp:('a -> 'a -> int) -> ('a, 'b) t
  (** Empty tree *)

val empty : unit -> ('a, 'b) t
  (** Empty tree using Pervasives.compare *)

val is_empty : (_, _) t -> bool
  (** Is the tree empty? *)

val find : ('a, 'b) t -> 'a -> 'b
  (** Find the element for this key, or raises Not_found *)

val mem : ('a, _) t -> 'a -> bool
  (** Is the key member of the tree? *)

val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** Add the binding to the tree *)

val singleton : cmp:('a -> 'a -> int) -> 'a -> 'b -> ('a, 'b) t
  (** Singleton map *)

val remove : ('a, 'b) t -> 'a -> ('a, 'b) t
  (** Remove the binding for this key *)

val iter : ('a, 'b) t -> ('a -> 'b -> unit) -> unit
  (** Iterate on bindings *)

val fold : ('a, 'b) t -> 'c -> ('c -> 'a -> 'b -> 'c) -> 'c
  (** Fold on bindings *)

val size : (_, _) t -> int
  (** Number of bindings (linear) *)

val choose : ('a, 'b) t -> ('a * 'b)
  (** Some binding, or raises Not_found *)

val to_seq : ('a, 'b) t -> ('a * 'b) sequence

val of_seq : ('a, 'b) t -> ('a * 'b) sequence -> ('a, 'b) t

(** {2 Functorial interface} *)

module type S = sig
  type key
  type 'a t
    (** Tree with keys of type [key] and values of type 'a *)

  val empty : unit -> 'a t
    (** Empty tree *)

  val is_empty : _ t -> bool
    (** Is the tree empty? *)

  val find : 'a t -> key -> 'a
    (** Find the element for this key, or raises Not_found *)

  val mem : _ t -> key -> bool
    (** Is the key member of the tree? *)

  val add : 'a t -> key -> 'a -> 'a t
    (** Add the binding to the tree *)

  val singleton : key -> 'a -> 'a t
    (** Singleton map *)

  val remove : 'a t -> key -> 'a t
    (** Remove the binding for this key *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
    (** Iterate on bindings *)

  val fold : 'a t -> 'c -> ('c -> key -> 'a -> 'c) -> 'c
    (** Fold on bindings *)

  val size : _ t -> int
    (** Number of bindings (linear) *)

  val choose : 'a t -> (key * 'a)
    (** Some binding, or raises Not_found *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_seq : 'a t -> (key * 'a) sequence -> 'a t
end

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORDERED) : S with type key = X.t
