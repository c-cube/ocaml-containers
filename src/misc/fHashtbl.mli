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

(** {1 Functional (persistent) hashtable} *)

type 'a sequence = ('a -> unit) -> unit

(** {2 Signatures} *)

module type HASH = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** The signature for such a functional hashtable *)
module type S = sig
  type 'a t
  type key

  val empty : int -> 'a t
    (** The empty hashtable (with sub-hashtables of given size) *)

  val is_empty : _ t -> bool

  val find : 'a t -> key -> 'a
    (** Find the binding for this key, or raise Not_found *)

  val mem : 'a t -> key -> bool
    (** Check whether the key is bound in this hashtable *)

  val replace : 'a t -> key -> 'a -> 'a t
    (** [replace t key val] returns a copy of [t] where [key] binds to [val] *)

  val remove : 'a t -> key -> 'a t
    (** Remove the bindings for the given key *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold on bindings *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
    (** Iterate on bindings *)

  val size : 'a t -> int
    (** Number of bindings *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_seq : ?size:int -> (key * 'a) sequence -> 'a t
end

(** {2 Persistent array} *)

module PArray : sig
  type 'a t

  val make : int -> 'a -> 'a t

  val get : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> 'a t

  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val length : 'a t -> int
end

(** {2 Tree-like hashtable} *)

module Tree(X : HASH) : S with type key = X.t

(** {2 Flat hashtable} *)

module Flat(X : HASH) : S with type key = X.t
