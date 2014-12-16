
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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


(** {1 Open-Addressing Hash-table}

We use Robin-Hood hashing as described in
http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
with backward shift. *)

type 'a sequence = ('a -> unit) -> unit

module type S = sig
  type key
  type 'a t

  val create : int -> 'a t
  (** Create a new table of the given initial capacity *)

  val mem : 'a t -> key -> bool
  (** [mem tbl k] returns [true] iff [k] is mapped to some value
      in [tbl] *)

  val find : 'a t -> key -> 'a option

  val find_exn : 'a t -> key -> 'a

  val get : key -> 'a t -> 'a option
  (** [get k tbl] recovers the value for [k] in [tbl], or
      returns [None] if [k] doesn't belong *)

  val get_exn : key -> 'a t -> 'a

  val add : 'a t -> key -> 'a -> unit
  (** [add tbl k v] adds [k -> v] to [tbl], possibly replacing the old
      value associated with [k]. *)

  val remove : 'a t -> key -> unit
  (** Remove binding *)

  val size : _ t -> int
  (** Number of bindings *)

  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list

  val of_seq : (key * 'a) sequence -> 'a t
  val to_seq : 'a t -> (key * 'a) sequence

  val keys : _ t -> key sequence
  val values : 'a t -> 'a sequence
end

module type HASHABLE = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

module Make(X : HASHABLE) = struct
  type key = X.t

  type 'a bucket =
    | Empty
    | Key of key * 'a * int   (* store the hash too *)

  type 'a t = {
    mutable arr : 'a bucket array;
    mutable size : int;
  }

  let size tbl = tbl.size

  let _reached_max_load tbl =
    let n = Array.length tbl.arr in
    (n - tbl.size) < n/10 (* full at 9/10 *)

  let create i =
    let i = min Sys.max_array_length (max i 8) in
    { arr=Array.make i Empty; size=0; }

  (* initial index for a value with hash [h] *)
  let _initial_idx tbl h =
    h mod Array.length tbl.arr

  let _succ tbl i =
    let i' = i+1 in
    if i' = Array.length tbl.arr then 0 else i'

  let _pred tbl i =
    if i = 0 then Array.length tbl.arr - 1 else i-1

  (* distance to initial bucket, at index [i] with hash [h] *)
  let _dib tbl h i =
    let i0 = _initial_idx tbl h in
    if i>=i0
      then i-i0
      else i+ (Array.length tbl.arr - i0 - 1)

  (* insert k->v in [tbl], currently at index [i] *)
  let rec _linear_probe tbl k v h_k i =
    match tbl.arr.(i) with
    | Empty ->
        (* add binding *)
        tbl.size <- 1 + tbl.size;
        tbl.arr.(i) <- Key (k, v, h_k)
    | Key (k', _, h_k') when X.equal k k' ->
        (* replace *)
        assert (h_k = h_k');
        tbl.arr.(i) <- Key (k, v, h_k)
    | Key (k', v', h_k') ->
        if _dib tbl h_k i < _dib tbl h_k' i
        then (
          (* replace *)
          tbl.arr.(i) <- Key (k, v, h_k);
          _linear_probe tbl k' v' h_k' (_succ tbl i)
        ) else
          (* go further *)
          _linear_probe tbl k v h_k (_succ tbl i)

  (* resize table: put a bigger array in it, then insert values
    from the old array *)
  let _resize tbl =
    let size' = min Sys.max_array_length (2 * Array.length tbl.arr) in
    let arr' = Array.make size' Empty in
    let old_arr = tbl.arr in
    (* replace with new table *)
    tbl.size <- 0;
    tbl.arr <- arr';
    Array.iter
      (function
        | Empty -> ()
        | Key (k, v, h_k) -> _linear_probe tbl k v h_k (_initial_idx tbl h_k)
      ) old_arr

  let add tbl k v =
    if _reached_max_load tbl
      then _resize tbl;
    (* insert value *)
    let h_k = X.hash k in
    _linear_probe tbl k v h_k (_initial_idx tbl h_k)

  (* shift back elements that have a DIB > 0 until an empty bucket is
   met, or some element doesn't need shifting *)
  let rec _backward_shift tbl i =
    match tbl.arr.(i) with
    | Empty -> ()
    | Key (_, _, h_k) when _dib tbl h_k i = 0 ->
        ()  (* stop *)
    | Key (_k, _v, h_k) as bucket ->
        assert (_dib tbl h_k i > 0);
        (* shift backward *)
        tbl.arr.(_pred tbl i) <- bucket;
        tbl.arr.(i) <- Empty;
        _backward_shift tbl (_succ tbl i)

  (* linear probing for removal of [k] *)
  let rec _linear_probe_remove tbl k h_k i =
    match tbl.arr.(i) with
    | Empty -> ()
    | Key (k', _, _) when X.equal k k' ->
        tbl.arr.(i) <- Empty;
        tbl.size <- tbl.size - 1;
        _backward_shift tbl (_succ tbl i)
    | Key (_, _, h_k') ->
        if _dib tbl h_k' i < _dib tbl h_k i
          then ()  (* [k] not present, would be here otherwise *)
          else _linear_probe_remove tbl k h_k (_succ tbl i)

  let remove tbl k =
    let h_k = X.hash k in
    _linear_probe_remove tbl k h_k (_initial_idx tbl h_k)

  let rec _get_exn tbl k h_k i dib =
    match tbl.arr.(i) with
    | Empty -> raise Not_found
    | Key (k', v', _) when X.equal k k' -> v'
    | Key (_, _, h_k') ->
        if _dib tbl h_k' i < dib
          then raise Not_found  (* [k] would be here otherwise *)
          else _get_exn tbl k h_k (_succ tbl i) (dib+1)

  let get_exn k tbl =
    let h_k = X.hash k in
    let i0 = _initial_idx tbl h_k in
    match tbl.arr.(i0) with
    | Empty -> raise Not_found
    | Key (k', v, _) ->
      if X.equal k k' then v
      else let i1 = _succ tbl i0 in
        match  tbl.arr.(i1) with
        | Empty -> raise Not_found
        | Key (k', v, _) ->
          if X.equal k k' then v
          else
            let i2 = _succ tbl i1 in
            match tbl.arr.(i2) with
            | Empty -> raise Not_found
            | Key (k', v, _) ->
              if X.equal k k' then v
              else _get_exn tbl k h_k (_succ tbl i2) 3

  let get k tbl =
    try Some (get_exn k tbl)
    with Not_found -> None

  let find_exn tbl k = get_exn k tbl

  let find tbl k =
    try Some (get_exn k tbl)
    with Not_found -> None

  let mem tbl k =
    try ignore (get_exn k tbl); true
    with Not_found -> false

  let of_list l =
    let tbl = create 16 in
    List.iter (fun (k,v) -> add tbl k v) l;
    tbl

  let to_list tbl =
    Array.fold_left
      (fun acc bucket -> match bucket with
        | Empty -> acc
        | Key (k,v,_) -> (k,v)::acc
      ) [] tbl.arr

  let of_seq seq =
    let tbl = create 16 in
    seq (fun (k,v) -> add tbl k v);
    tbl

  let to_seq tbl yield =
    Array.iter
      (function Empty -> () | Key (k, v, _) -> yield (k,v))
      tbl.arr

  let keys tbl yield =
    Array.iter
      (function Empty -> () | Key (k, _, _) -> yield k)
      tbl.arr

  let values tbl yield =
    Array.iter
      (function Empty -> () | Key (_, v, _) -> yield v)
      tbl.arr
end

