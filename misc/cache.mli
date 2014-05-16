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

(** {1 Memoization caches} *)

(** {2 Signatures} *)

module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module type HASH = sig
  include EQ
  val hash : t -> int
end

(** Signature of a cache for values *)
module type S = sig
  type 'a t
  type key

  val create : int -> 'a t
    (** Create a new cache of the given size. *)

  val clear : 'a t -> unit
    (** Clear content of the cache *)

  val with_cache : 'a t -> (key -> 'a) -> key -> 'a
    (** Wrap the function with the cache. This means that
        [with_cache cache f x] always returns the same value as
        [f x], if [f x] returns, or raise the same exception.
        However, [f] may not be called if [x] is in the cache. *)

  val with_cache_rec : 'a t -> ((key -> 'a) -> key -> 'a) -> key -> 'a
    (** Partially apply the given function with a cached version of itself.
        It returns the specialized function.
        [with_cache_rec cache f] applies [f] to a cached version of [f],
        called [f'], so that [f' x = f f' x]. *)
end

(** Signature of a cache for pairs of values *)
module type S2 = sig
  type 'a t
  type key1
  type key2

  val create : int -> 'a t
    (** Create a new cache of the given size. *)

  val clear : 'a t -> unit
    (** Clear content of the cache *)

  val with_cache : 'a t -> (key1 -> key2 -> 'a) -> key1 -> key2 -> 'a
    (** Wrap the function with the cache *)
end

(** {2 Dummy cache (no caching)} *)

module Dummy(X : sig type t end) : S with type key = X.t

module Dummy2(X : sig type t end)(Y : sig type t end) : S2 with type key1 = X.t and type key2 = Y.t

(** {2 Small linear cache} *)

(** This cache stores (key,value) pairs in an array, that is traversed
    linearily. It is therefore only reasonable for small sizes (like 5). *)

module Linear(X : EQ) : S with type key = X.t

module Linear2(X : EQ)(Y : EQ) : S2 with type key1 = X.t and type key2 = Y.t

(** {2 Hashtables that resolve collisions by replacing} *)

module Replacing(X : HASH) : S with type key = X.t

module Replacing2(X : HASH)(Y : HASH) : S2 with type key1 = X.t and type key2 = Y.t

(** {2 Hashtables with Least Recently Used eviction policy} *)

module LRU(X : HASH) : S with type key = X.t

(* TODO exception handling in LRU *)
(* TODO LRU2 *)

