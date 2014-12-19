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

module PArray = struct
  type 'a t = 'a zipper ref
  and 'a zipper =
    | Array of 'a array
    | Diff of int * 'a * 'a zipper ref

  (* XXX maybe having a snapshot of the array from point to point may help? *)

  let make size elt =
    let a = Array.make size elt in
    ref (Array a)

  (** Recover the given version of the shared array. Returns the array
      itself. *)
  let rec reroot t =
    match !t with
    | Array a -> a
    | Diff (i, v, t') ->
      begin
        let a = reroot t' in
        let v' = a.(i) in
        t' := Diff (i, v', t);
        a.(i) <- v;
        t := Array a;
        a
      end

  let get t i =
    match !t with
    | Array a -> a.(i)
    | Diff _ -> 
      let a = reroot t in
      a.(i)

  let set t i v =
    let a =
      match !t with
      | Array a -> a
      | Diff _ -> reroot t in
    let v' = a.(i) in
    if v == v'
      then t (* no change *)
      else begin
        let t' = ref (Array a) in
        a.(i) <- v;
        t := Diff (i, v', t');
        t' (* create new array *)
      end

  let fold_left f acc t =
    let a = reroot t in
    Array.fold_left f acc a

  let rec length t =
    match !t with
    | Array a -> Array.length a
    | Diff (_, _, t') -> length t'
end

(** {2 Tree-like hashtable} *)

module Tree(X : HASH) = struct
  (** The hashtable is a binary tree, with persistent arrays as leaves.
      Nodes at depth n of the tree are split on the n-th digit of the hash
      (starting with the least significant bit as 0).
      
      The left child is for bit=0, the right one for bit=1. *)

  type key = X.t

  type 'a t =
    | Split of 'a t * 'a t    (** Split on the last digit of the hash *)
    | Table of 'a buckets     (** Hashtable as a persistent array *)
    (** The hashtable, as a tree of persistent open addressing hashtables *)
  and 'a buckets = 'a bucket PArray.t
    (** A persistent array of buckets *)
  and 'a bucket =
    | Empty
    | Deleted
    | Used of key * 'a
    (** One buckets stores one key->value binding *)

  let empty_buckets size =
    PArray.make size Empty

  (** Empty hashtable *)
  let empty size =
    let size = max size 4 in  (* size >= 4 *)
    Table (empty_buckets size)

  let rec is_empty_array a i =
    if i = Array.length a then true
    else (a.(i) = Empty || a.(i) = Deleted) && is_empty_array a (i+1)

  let rec is_empty t =
    match t with
    | Split (l, r) -> is_empty l && is_empty r
    | Table a -> is_empty_array (PArray.reroot a) 0

  (** The address in a bucket array, after probing [i] times *)
  let addr n h i = ((h land max_int) + i) mod n

  (** Find the bucket that contains the given [key]. [h] is
      not necessarily the hash of the key, because it can have been
      shifted to right several times. *)
  let rec probe_find buckets n h key i =
    if i = n then raise Not_found else begin
      let j = addr n h i in
      match PArray.get buckets j with
      | Empty -> raise Not_found
      | Used (key', value) when X.equal key key' ->
        value (* found *)
      | Used _ | Deleted ->
        probe_find buckets n h key (i+1)  (* go further *)
    end
    
  (** Find the value bound to the given [key] *)
  let find t key =
    let h = X.hash key in
    (* find the appropriate leaf *)
    let rec find h t =
      match t with
      | Split (l, r) ->
        if h land 0x1 = 0
          then find (h lsr 1) l   (* bit=0, goto left *)
          else find (h lsr 1) r   (* bit=1, goto right *)
      | Table buckets ->
        probe_find buckets (PArray.length buckets) h key 0
    in
    find h t

  (** Check whether the key is bound in this hashtable *)
  let mem t key =
    try ignore (find t key); true
    with Not_found -> false

  (** Maximal depth of the tree (number of bits of the hash) *)
  let max_depth = Sys.word_size - 1

  (** [i] is the length of the current probe. [n] is the size of
      the buckets array. This decides whether the probe, looking
      for a free bucket to insert a binding in, is too long. *)
  let probe_too_long n i =
    i / 5 > n / 8  (* i/n > 5/8 *)

  (** Insert [key] -> [value] in the buckets. *)
  let rec probe_insert buckets ~depth h key value =
    let n = PArray.length buckets in
    let rec probe i =
      if n = i then (assert (depth = max_depth); failwith "FHashtbl is full")
      else if (depth < max_depth && probe_too_long n i)
        (* We are not too deep, and the table starts being full, we
           split it into two sub-tables *)
        then
          let depth' = depth + 1 in
          (* increase size of sub-arrays by 1.5 *)
          let sub_size = min (n + (n lsr 1)) Sys.max_array_length in
          let l, r = PArray.fold_left
            (fun (l,r) bucket -> match bucket with
              | Empty | Deleted -> (l,r)
              | Used (key',value') ->
                let h' = (X.hash key') lsr depth in
                if h' land 0x1 = 0
                  then 
                    let l' = insert l ~depth:depth' (h' lsr 1) key' value' in
                    l', r
                  else
                    let r' = insert r ~depth:depth' (h' lsr 1) key' value' in
                    l, r')
            (empty sub_size, empty sub_size) buckets in
          (* the split of those two sub-hashtables *)
          let new_table = Split (l, r) in
          (* insert in this new hashtable *)
          insert new_table ~depth h key value
        else (* look for an empty slot to insert the bucket *)
          let j = addr n h i in
          match PArray.get buckets j with
          | Empty | Deleted ->
            (* insert here *)
            let buckets' = PArray.set buckets j (Used (key, value)) in
            Table buckets'
          | Used (key', _) when X.equal key key' ->
            (* replace *)
            let buckets' = PArray.set buckets j (Used (key, value)) in
            Table buckets'
          | Used _ -> probe (i+1)  (* probe failed, go further *)
    in
    probe 0
  (** Insert [key] -> [value] in the sub-hashtable *)
  and insert t ~depth h key value =
    match t with
    | Split (l, r) ->
      if h land 0x1 = 0
        then  (* bit=0, goto left *)
          let l' = insert l ~depth:(depth+1) (h lsr 1) key value in
          Split (l', r)
        else  (* bit=1, goto right *)
          let r' = insert r ~depth:(depth+1) (h lsr 1) key value in
          Split (l, r')
    | Table buckets ->
      (* insert in the flat hashtable *)
      probe_insert buckets ~depth h key value

  (** [replace t key val] returns a copy of [t] where [key] binds to [val] *)
  let replace t key value =
    let h = X.hash key in
    insert t ~depth:0 h key value

  (** Recursive removal function *)
  let rec rec_remove t h key =
    match t with
    | Split (l, r) ->
      if h land 0x1 = 0
        then  (* bit=0, goto left *)
          let l' = rec_remove l (h lsr 1) key in
          if l == l' then t else Split (l', r)
        else  (* bit=1, goto right *)
          let r' = rec_remove r (h lsr 1) key in
          if r == r' then t else Split (l, r')
    | Table buckets ->
      (* remove from the flat hashtable *)
      probe_remove t buckets h key
  (* remove key from the buckets *)
  and probe_remove old_table buckets h key =
    let n = PArray.length buckets in
    let rec probe i =
      if i = n
        then old_table (* not present *)
        else
          let j = addr n h i in
          match PArray.get buckets j with
          | Empty -> old_table (* not present *)
          | Deleted -> probe (i+1)
          | Used (key', _) ->
            if X.equal key key'
              then Table (PArray.set buckets j Deleted)
              else probe (i+1)
    in
    probe 0
    

  (** Remove the bindings for the given key *)
  let remove t key =
    let h = X.hash key in
    rec_remove t h key

  (** Fold on bindings *)
  let rec fold f acc t =
    match t with
    | Split (l, r) ->
      let acc' = fold f acc l in
      fold f acc' r
    | Table buckets ->
      PArray.fold_left
        (fun acc bucket -> match bucket with
          | Empty | Deleted -> acc
          | Used (key, value) -> f acc key value)
        acc buckets

  let iter f t =
    fold (fun () k v -> f k v) () t

  let size t =
    fold (fun n _ _ -> n + 1) 0 t

  let to_seq t k =
    iter (fun key value -> k (key, value)) t

  let of_seq ?(size=32) seq =
    let cur = ref (empty size) in
    seq (fun (k,v) -> cur := replace !cur k v);
    !cur
end

(** {2 Flat hashtable} *)

module Flat(X : HASH) = struct
  type key = X.t

  (** A hashtable is a persistent array of (key, value) buckets *)
  type 'a t = {
    buckets : 'a bucket PArray.t;
    size : int;
  }
  and 'a bucket =
  | Deleted
  | Empty
  | Used of key * 'a

  let max_load = 0.8

  (** Empty table. Size will be >= 2 *)
  let empty size =
    let size = max 2 size in
    { buckets = PArray.make size Empty;
      size = 0;
    }

  let rec is_empty_array a i =
    if i = Array.length a then true
    else (a.(i) = Empty || a.(i) = Deleted) && is_empty_array a (i+1)

  let is_empty t = is_empty_array (PArray.reroot t.buckets) 0

  (** Index of slot, for i-th probing starting from hash [h] in
      a table of length [n] *)
  let addr h n i = ((h land max_int) + i) mod n
    
  (** Insert (key -> value) in buckets, starting with the hash. *)
  let insert buckets h key value =
    let n = PArray.length buckets in
    (* lookup an empty slot to insert the key->value in. *)
    let rec lookup h n i =
      let j = addr h n i in
      match PArray.get buckets j with
      | Empty ->
        PArray.set buckets j (Used (key, value))
      | Used (key', _) when X.equal key key' ->
        PArray.set buckets j (Used (key, value))
      | _ -> lookup h n (i+1)
    in
    lookup h n 0

  (** Resize the array, by inserting its content into twice as large an array *)
  let resize buckets =
    let new_size = min (PArray.length buckets * 2) Sys.max_array_length in
    let buckets' = PArray.make new_size Empty in
    (* loop to transfer values from buckets to buckets' *)
    let rec tranfer buckets' i =
      if i = PArray.length buckets then buckets'
        else match PArray.get buckets i with
        | Used (key, value) ->
          (* insert key -> value into new array *)
          let buckets' = insert buckets' (X.hash key) key value in
          tranfer buckets' (i+1)
        | _ ->
          tranfer buckets' (i+1)
    in tranfer buckets' 0

  (** Lookup [key] in the table *)
  let find t key =
    let buckets = t.buckets in
    let n = PArray.length buckets in
    let h = X.hash key in
    let rec probe h n i num =
      if num = n then raise Not_found
      else let j = addr h n i in
      match PArray.get buckets j with
      | Used (key', value) when X.equal key key' ->
        value  (* found value for this key *)
      | Deleted | Used _ ->
        probe h n (i+1) (num + 1) (* try next bucket *)
      | Empty -> raise Not_found
    in
    probe h n 0 0

  (** put [key] -> [value] in the hashtable *)
  let replace t key value =
    let load = float_of_int t.size /. float_of_int (PArray.length t.buckets) in
    let t =
      if load > max_load then { t with buckets = resize t.buckets } else t in
    let n = PArray.length t.buckets in
    let h = X.hash key in
    let buckets = t.buckets in
    let rec probe h n i =
      let j = addr h n i in
      match PArray.get buckets j with
      | Used (key', _) when X.equal key key' ->
        let buckets' = PArray.set buckets j (Used (key, value)) in
        { t with buckets = buckets' } (* replace binding *)
      | Deleted | Empty ->
        let buckets' = PArray.set buckets j (Used (key, value)) in
        { buckets = buckets'; size = t.size + 1; } (* add binding *)
      | Used _ ->
        probe h n (i+1) (* go further *)
    in
    probe h n 0

  (** Remove the key from the table *)
  let remove t key =
    let n = PArray.length t.buckets in
    let h = X.hash key in
    let buckets = t.buckets in
    let rec probe h n i =
      let j = addr h n i in
      match PArray.get buckets j with
      | Used (key', _) when X.equal key key' ->
        (* remove slot *)
        let buckets' = PArray.set buckets j Deleted in
        { buckets = buckets'; size = t.size - 1; }
      | Deleted | Used _ ->
        probe h n (i+1) (* search further *)
      | Empty -> t  (* not present *)
    in
    probe h n 0

  (** size of the table *)
  let size t = t.size

  (** Is the key member of the table? *)
  let mem t key =
    try ignore (find t key); true
    with Not_found -> false

  (** Iterate on key -> value pairs *)
  let iter k t =
    let buckets = t.buckets in
    for i = 0 to PArray.length buckets - 1 do
      match PArray.get buckets i with
      | Used (key, value) -> k key value
      | _ -> ()
    done

  (** Fold on key -> value pairs *)
  let fold f acc t =
    PArray.fold_left
      (fun acc bucket -> match bucket with
        | Used (key, value) -> f acc key value
        | _ -> acc)
      acc t.buckets

  let to_seq t k = iter (fun key value -> k (key, value)) t

  let of_seq ?(size=32) seq =
    let t = ref (empty size) in
    seq (fun (k,v) -> t := replace !t k v);
    !t
end
