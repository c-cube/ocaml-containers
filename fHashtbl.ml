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

  val find : 'a t -> key -> 'a
    (** Find the binding for this key, or raise Not_found *)

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

  val depth : 'a t -> int
    (** Depth of the tree *)

  val to_seq : 'a t -> (key * 'a) Sequence.t

  val of_seq : ?size:int -> (key * 'a) Sequence.t -> 'a t
end

(** {2 Persistent array} *)

module PArray = struct
  type 'a t = 'a zipper ref
  and 'a zipper =
    | Array of 'a array
    | Diff of int * 'a * 'a zipper ref

  (* XXX maybe having a snapshot of the array from point to point may help? *)

  let make size elt =
    let a = Array.create size elt in
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

(** {2 Constructor} *)

module Make(X : HASH) = struct
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
    Table (empty_buckets size)

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

  (** Insert [key] -> [value] in the buckets. *)
  let rec probe_insert buckets ~depth h key value =
    let n = PArray.length buckets in
    let rec probe i =
      if i = n
        then (* table seems full, split in two sub-hashtables *)
          let depth' = depth + 1 in
          let l, r = PArray.fold_left
            (fun (l,r) bucket -> match bucket with
              | Empty | Deleted -> (l,r)
              | Used (key',value') ->
                let h' = (X.hash key') lsr depth' in
                if h' land 0x1 = 0
                  then 
                    let l' = insert l ~depth:depth' h' key' value' in
                    l', r
                  else
                    let r' = insert r ~depth:depth' h' key' value' in
                    l, r')
            (empty n, empty n) buckets in
          Split (l, r)
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

  (** Remove the bindings for the given key *)
  let remove t key =
    failwith "not implemented" (* TODO *)

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

  let rec depth t =
    match t with
    | Table _ -> 0
    | Split (l, r) -> (max (depth l) (depth r)) + 1

  let to_seq t =
    Sequence.from_iter (fun k -> iter (fun key value -> k (key, value)) t)

  let of_seq ?(size=32) seq =
    Sequence.fold
      (fun t (k,v) -> replace t k v)
      (empty size) seq
end
