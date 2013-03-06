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
    (** Wrap the function with the cache *)
end

(** Signature of a cache for pairs of values *)
module type S2 = sig
  type 'a t
  type key

  val create : int -> 'a t
    (** Create a new cache of the given size. *)

  val clear : 'a t -> unit
    (** Clear content of the cache *)

  val with_cache : 'a t -> (key -> key -> 'a) -> key -> key -> 'a
    (** Wrap the function with the cache *)
end


(** {2 Small linear cache} *)

(** This cache stores (key,value) pairs in an array, that is traversed
    linearily. It is therefore only reasonable for small sizes (like 5). *)

module Linear(X : EQ) = struct
  type 'a t = 'a bucket array
  and 'a bucket = Empty | Pair of key * 'a
  and key = X.t

  let create size =
    assert (size >= 1);
    Array.create size Empty

  let clear cache =
    Array.fill cache 0 (Array.length cache) Empty

  (** Insert the binding (x -> y) into the cache *)
  let insert cache x y =
    let n = Array.length cache in
    (* shift other values toward the end *)
    Array.blit cache 0 cache 1 (n-1);
    cache.(0) <- Pair(x,y)

  (** Try to find [f x] in the cache, otherwise compute it
      and cache the result *)
  let with_cache cache f x =
    let n = Array.length cache in
    let rec search i =
      (* function that performs the lookup *)
      if i = n then begin
          (* cache miss *)
          let y = f x in
          insert cache x y;
          y
      end else match cache.(i) with
      | Pair (x',y) when X.equal x x' -> y
      | Empty | Pair _ -> search (i+1)
    in
    search 0
end

module Linear2(X : EQ) = struct
  type 'a t = 'a bucket array
  and 'a bucket = Empty | Assoc of key * key * 'a
  and key = X.t

  let create size =
    assert (size >= 1);
    Array.create size Empty

  let clear cache =
    Array.fill cache 0 (Array.length cache) Empty

  (** Insert the binding (x -> y) into the cache *)
  let insert cache x1 x2 y =
    let n = Array.length cache in
    (* shift other values toward the end *)
    Array.blit cache 0 cache 1 (n-1);
    cache.(0) <- Assoc(x1,x2,y)

  (** Try to find [f x] in the cache, otherwise compute it
      and cache the result *)
  let with_cache cache f x1 x2 =
    let n = Array.length cache in
    let rec search i =
      (* function that performs the lookup *)
      if i = n then begin
          (* cache miss *)
          let y = f x1 x2 in
          insert cache x1 x2 y;
          y
      end else match cache.(i) with
      | Assoc (x1',x2',y) when X.equal x1 x1' && X.equal x2 x2' -> y
      | Empty | Assoc _ -> search (i+1)
    in
    search 0
end

(** {2 An imperative cache of fixed size for memoization of pairs} *)

module Replacing(X : HASH) = struct
  type key = X.t

  (** A slot of the array contains a (key, value, true)
      if key->value is stored there (at index hash(key) % length),
      (null, null, false) otherwise.
      
      The first slot in the array contains the function
      used to produce the value upon a cache miss. *)
  type 'a t = 'a bucket array
  and 'a bucket = Empty | Assoc of key * 'a

  let create size =
    Array.create size Empty

  let clear c =
    Array.fill c 0 (Array.length c) Empty

  let with_cache c f x =
    let i = (X.hash x) mod (Array.length c) in
    match c.(i) with
    | Assoc (x', y) when X.equal x x' ->
      y (* cache hit *)
    | Assoc _ | Empty -> (* cache miss *)
      let y = f x in
      c.(i) <- Assoc (x, y);
      y
end

module Replacing2(X : HASH) = struct
  type key = X.t

  (** A slot of the array contains a (key, value, true)
      if key->value is stored there (at index hash(key) % length),
      (null, null, false) otherwise.
      
      The first slot in the array contains the function
      used to produce the value upon a cache miss. *)
  type 'a t = 'a bucket array
  and 'a bucket = Empty | Assoc of key * key * 'a

  let create size =
    Array.create size Empty

  let clear c =
    Array.fill c 0 (Array.length c) Empty

  let with_cache c f x1 x2 =
    let i = (((X.hash x1 + 17) lxor X.hash x2) mod Array.length c) in
    match c.(i) with
    | Assoc (x1', x2', y) when X.equal x1 x1' && X.equal x2 x2' ->
      y (* cache hit *)
    | Assoc _ | Empty -> (* cache miss *)
      let y = f x1 x2 in
      c.(i) <- Assoc (x1, x2, y);
      y
end
