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

  val with_cache_rec : int -> ((key -> 'a) -> key -> 'a) -> ('a t * (key -> 'a))
    (** Partially apply the given function with a cached version of itself.
        The cache has as size the first (int) argument.
        It returns both the cache, and the specialized function *)
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

(** {2 Dummy cache (no caching) *)

module Dummy(X : sig type t end) = struct
  type 'a t = unit
  and key = X.t

  let create size = ()

  let clear () = ()

  let with_cache () f x = f x

  let with_cache_rec size f =
    let rec f' x = f f' x in
    (), f'
end

module Dummy2(X : sig type t end)(Y : sig type t end) = struct
  type 'a t = unit
  and key1 = X.t
  and key2 = Y.t

  let create size = ()

  let clear () = ()

  let with_cache () f x1 x2 = f x1 x2
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

  (** Partially apply the given function with a new cache of the
      given size. It returns both the cache, and the specialized function *)
  let with_cache_rec size f =
    let cache = create size in
    (* make a recursive version of [f] that uses the cache *)
    let rec f' x = with_cache cache (fun x -> f f' x) x in
    cache, f'
end

module Linear2(X : EQ)(Y : EQ) = struct
  type 'a t = 'a bucket array
  and 'a bucket = Empty | Assoc of key1 * key2 * 'a
  and key1 = X.t
  and key2 = Y.t

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
      | Assoc (x1',x2',y) when X.equal x1 x1' && Y.equal x2 x2' -> y
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

  (** Try to find [f x] in the cache, otherwise compute it
      and cache the result *)
  let with_cache c f x =
    let i = (X.hash x) mod (Array.length c) in
    match c.(i) with
    | Assoc (x', y) when X.equal x x' ->
      y (* cache hit *)
    | Assoc _ | Empty -> (* cache miss *)
      let y = f x in
      c.(i) <- Assoc (x, y);
      y

  (** Partially apply the given function with a new cache of the
      given size. It returns both the cache, and the specialized function *)
  let with_cache_rec size f =
    let cache = create size in
    (* make a recursive version of [f] that uses the cache *)
    let rec f' x = with_cache cache (fun x -> f f' x) x in
    cache, f'
end

module Replacing2(X : HASH)(Y : HASH) = struct
  (** A slot of the array contains a (key, value, true)
      if key->value is stored there (at index hash(key) % length),
      (null, null, false) otherwise.
      
      The first slot in the array contains the function
      used to produce the value upon a cache miss. *)
  type 'a t = 'a bucket array
  and 'a bucket = Empty | Assoc of key1 * key2 * 'a
  and key1 = X.t
  and key2 = Y.t


  let create size =
    Array.create size Empty

  let clear c =
    Array.fill c 0 (Array.length c) Empty

  let with_cache c f x1 x2 =
    let i = (((X.hash x1 + 17) lxor Y.hash x2) mod Array.length c) in
    match c.(i) with
    | Assoc (x1', x2', y) when X.equal x1 x1' && Y.equal x2 x2' ->
      y (* cache hit *)
    | Assoc _ | Empty -> (* cache miss *)
      let y = f x1 x2 in
      c.(i) <- Assoc (x1, x2, y);
      y
end

(** {2 Hashtables with Least Recently Used eviction policy *)

module LRU(X : HASH) = struct
  type key = X.t

  module H = Hashtbl.Make(X)

  type 'a t = {
    table : 'a node H.t;  (* hashtable key -> node *)
    first : 'a node;      (* dummy node for the entry of the list *)
    mutable len : int;    (* number of entries *)
    size : int;           (* max size *)
  }
  and 'a node = {
    mutable key : key;
    mutable value : 'a;
    mutable next : 'a node;
    mutable prev : 'a node;
  } (** Meta data for the value *)

  let create size =
    let rec first = 
      { key = Obj.magic 0; value = Obj.magic 0; next=first; prev=first; }
    in
    { table = H.create size;
      len = 0;
      size;
      first;
    }

  (** Clear the content of the cache *)
  let clear c =
    c.len <- 0;
    H.clear c.table;
    c.first.next <- c.first;
    c.first.prev <- c.first;
    ()

  (** Find an element, or raise Not_found *)
  let find c x =
    let n = H.find c.table x in
    assert (X.equal n.key x);
    n.value

  (** Replace least recently used element of [c] by x->y *)
  let replace c x y =
    let n = c.first.next in
    (* remove old element *)
    H.remove c.table n.key;
    (* insertion in hashtable *)
    H.add c.table x n;
    (* re-use the node for x,y *)
    n.key <- x;
    n.value <- y;
    (* remove from front of queue *)
    n.next.prev <- c.first;
    c.first.next <- n.next;
    (* insert at back of queue *)
    let last = c.first.prev in
    last.next <- n;
    c.first.prev <- n;
    n.next <- c.first;
    n.prev <- last;
    ()

  (** Insert x->y in the cache, increasing its entry count *)
  let insert c x y =
    c.len <- c.len + 1;
    let n = {
      key = x;
      value = y;
      next = c.first;
      prev = c.first.prev;
    } in
    (* insertion in hashtable *)
    H.add c.table x n;
    (* insertion at back of queue *)
    c.first.prev.next <- n;
    c.first.prev <- n;
    ()

  (** Try to find [f x] in the cache, otherwise compute it
      and cache the result *)
  let with_cache c f x =
    try
      find c x
    with Not_found ->
      let y = f x in
      (if c.len = c.size
        then replace c x y
        else insert c x y);
      y

  (** Partially apply the given function with a new cache of the
      given size. It returns both the cache, and the specialized function *)
  let with_cache_rec size f =
    let cache = create size in
    (* make a recursive version of [f] that uses the cache *)
    let rec f' x = with_cache cache (fun x -> f f' x) x in
    cache, f'
end
