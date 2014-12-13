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

(* We use splay trees, following
http://www.cs.cornell.edu/Courses/cs3110/2009fa/recitations/rec-splay.html
*)

type 'a sequence = ('a -> unit) -> unit

(** {2 Polymorphic Maps} *)

type ('a, 'b) t = {
  cmp : 'a -> 'a -> int;
  mutable tree : ('a, 'b) tree;  (* for lookups *)
} (** Tree with keys of type 'a, and values of type 'b *)
and ('a, 'b) tree =
  | Empty
  | Node of ('a * 'b * ('a, 'b) tree * ('a, 'b) tree)

let empty_with ~cmp =
  { cmp;
    tree = Empty;
  }

let empty () =
  { cmp = Pervasives.compare;
    tree = Empty;
  }

let is_empty t =
  match t.tree with
  | Empty -> true
  | Node _ -> false

(** Pivot the tree so that the node that has key [key], or close to [key], is
    the root node. *)
let rec splay ~cmp (k, v, l, r) key =
  let c = cmp key k in
  if c = 0
    then (k, v, l, r) (* found *)
  else if c < 0
    then match l with
    | Empty -> (k, v, l, r)  (* not found *)
    | Node (lk, lv, ll, lr) ->
      let lc = cmp key lk in
      if lc = 0
        then (lk, lv, ll, Node (k, v, lr, r))  (* zig *)
      else if lc < 0
        then match ll with
        | Empty -> (lk, lv, Empty, Node (k, v, lr, r)) (* not found *)
        | Node n -> (* zig zig *)
          let (llk, llv, lll, llr) = splay ~cmp n key in
          (llk, llv, lll, Node (lk, lv, llr, Node (k, v, lr, r)))
      else
        match lr with
        | Empty -> (lk, lv, ll, Node (k, v, Empty, r))
        | Node n -> (* zig zag *)
          let (lrk, lrv, lrl, lrr) = splay ~cmp n key in
          (lrk, lrv, Node (lk, lv, ll, lrl), Node (k, v, lrr, r))
  else match r with
    | Empty -> (k, v, l, r)  (* not found *)
    | Node (rk, rv, rl, rr) ->
      let rc = cmp key rk in
      if rc = 0
        then (rk, rv, Node (k, v, l, rl), rr)  (* zag *)
      else if rc > 0
        then match rr with
        | Empty -> (rk, rv, Node (k, v, l, rl), Empty)  (* not found *)
        | Node n ->  (* zag zag *)
          let (rrk, rrv, rrl, rrr) = splay ~cmp n key in
          (rrk, rrv, Node (rk, rv, Node (k, v, l, rl), rrl), rrr)
      else match rl with
        | Empty -> (rk, rv, Node (k, v, l, Empty), rr) (* zag zig *)
        | Node n ->  (* zag zig *)
          let (rlk, rlv, rll, rlr) = splay ~cmp n key in
          (rlk, rlv, Node (k, v, l, rll), Node (rk, rv, rlr, rr))

let find t key =
  match t.tree with
  | Empty -> raise Not_found
  | Node (k, v, l, r) ->
    let (k, v, l, r) = splay ~cmp:t.cmp (k, v, l, r) key in
    t.tree <- Node (k, v, l, r);   (* save balanced tree *)
    if t.cmp key k = 0
      then v
      else raise Not_found

let mem t key =
  match t.tree with
  | Empty -> false
  | Node (k, v, l, r) ->
    let (k, v, l, r) = splay ~cmp:t.cmp (k, v, l, r) key in
    t.tree <- Node (k, v, l, r);   (* save balanced tree *)
    if t.cmp key k = 0
      then true
      else false

(** Recursive insertion of key->value in the tree *)
let rec insert ~cmp tree key value =
  match tree with
  | Empty -> Node (key, value, Empty, Empty)
  | Node (k, v, l, r) ->
    let c = cmp key k in
    if c = 0
      then Node (key, value, l, r)  (* replace *)
    else if c < 0
      then Node (k, v, insert ~cmp l key value, r)
    else Node (k, v, l, insert ~cmp r key value)

let add t key value =
  let tree =
    match t.tree with
    | Empty -> Node (key, value, Empty, Empty)
    | Node (k, v, l, r) ->
      let (k, v, l, r) = splay ~cmp:t.cmp (k, v, l, r) key in
      let tree = Node (k, v, l, r) in
      t.tree <- tree;  (* save balanced tree *)
      (* insertion in this tree *)
      insert ~cmp:t.cmp tree key value
  in
  { t with tree; }

let singleton ~cmp key value =
  add (empty_with ~cmp) key value

(** Merge of trees, where a < b *)
let rec left_merge a b =
  match a, b with
  | Empty, Empty -> Empty
  | Node (k, v, l, r), b -> Node (k, v, l, left_merge r b)
  | Empty, b -> b

let remove t key =
  match t.tree with
  | Empty -> t
  | Node (k, v, l, r) ->
    let (k, v, l, r) = splay ~cmp:t.cmp (k, v, l, r) key in
    t.tree <- Node (k, v, l, r);
    if t.cmp key k = 0
      then (* remove the node, by merging the subnodes *)
        let tree = left_merge l r in
        { t with tree; }
      else (* not present, same tree *)
        t

let iter t f =
  let rec iter t = match t with
  | Empty -> ()
  | Node (k, v, l, r) ->
    iter l;
    f k v;
    iter r
  in iter t.tree

let fold t acc f =
  let rec fold acc t = match t with
  | Empty -> acc
  | Node (k, v, l, r) ->
    let acc = fold acc l in
    let acc = f acc k v in
    fold acc r
  in
  fold acc t.tree

let size t = fold t 0 (fun acc _ _ -> acc+1)

let choose t =
  match t.tree with
  | Empty -> raise Not_found
  | Node (k, v, _, _) -> k, v

let to_seq t =
  fun kont -> iter t (fun k v -> kont (k, v))

let of_seq t seq =
  let t = ref t in
  seq (fun (k, v) -> t := add !t k v);
  !t

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

module Make(X : ORDERED) = struct

  type key = X.t
  type 'a t = {
    mutable tree : 'a tree;  (* for lookups *)
  } (** Tree with keys of type key, and values of type 'a *)
  and 'a tree =
    | Empty
    | Node of (key * 'a * 'a tree * 'a tree)

  let empty () =
    { tree = Empty; }

  let is_empty t =
    match t.tree with
    | Empty -> true
    | Node _ -> false

  (** Pivot the tree so that the node that has key [key], or close to [key], is
      the root node. *)
  let rec splay (k, v, l, r) key =
    let c = X.compare key k in
    if c = 0
      then (k, v, l, r) (* found *)
    else if c < 0
      then match l with
      | Empty -> (k, v, l, r)  (* not found *)
      | Node (lk, lv, ll, lr) ->
        let lc = X.compare key lk in
        if lc = 0
          then (lk, lv, ll, Node (k, v, lr, r))  (* zig *)
        else if lc < 0
          then match ll with
          | Empty -> (lk, lv, Empty, Node (k, v, lr, r)) (* not found *)
          | Node n -> (* zig zig *)
            let (llk, llv, lll, llr) = splay n key in
            (llk, llv, lll, Node (lk, lv, llr, Node (k, v, lr, r)))
        else
          match lr with
          | Empty -> (lk, lv, ll, Node (k, v, Empty, r))
          | Node n -> (* zig zag *)
            let (lrk, lrv, lrl, lrr) = splay n key in
            (lrk, lrv, Node (lk, lv, ll, lrl), Node (k, v, lrr, r))
    else match r with
      | Empty -> (k, v, l, r)  (* not found *)
      | Node (rk, rv, rl, rr) ->
        let rc = X.compare key rk in
        if rc = 0
          then (rk, rv, Node (k, v, l, rl), rr)  (* zag *)
        else if rc > 0
          then match rr with
          | Empty -> (rk, rv, Node (k, v, l, rl), Empty)  (* not found *)
          | Node n ->  (* zag zag *)
            let (rrk, rrv, rrl, rrr) = splay n key in
            (rrk, rrv, Node (rk, rv, Node (k, v, l, rl), rrl), rrr)
        else match rl with
          | Empty -> (rk, rv, Node (k, v, l, Empty), rr) (* zag zig *)
          | Node n ->  (* zag zig *)
            let (rlk, rlv, rll, rlr) = splay n key in
            (rlk, rlv, Node (k, v, l, rll), Node (rk, rv, rlr, rr))

  let find t key =
    match t.tree with
    | Empty -> raise Not_found
    | Node (k, v, l, r) ->
      let (k, v, l, r) = splay (k, v, l, r) key in
      t.tree <- Node (k, v, l, r);   (* save balanced tree *)
      if X.compare key k = 0
        then v
        else raise Not_found

  let mem t key =
    match t.tree with
    | Empty -> false
    | Node (k, v, l, r) ->
      let (k, v, l, r) = splay (k, v, l, r) key in
      t.tree <- Node (k, v, l, r);   (* save balanced tree *)
      if X.compare key k = 0
        then true
        else false

  (** Recursive insertion of key->value in the tree *)
  let rec insert tree key value =
    match tree with
    | Empty -> Node (key, value, Empty, Empty)
    | Node (k, v, l, r) ->
      let c = X.compare key k in
      if c = 0
        then Node (key, value, l, r)  (* replace *)
      else if c < 0
        then Node (k, v, insert l key value, r)
      else Node (k, v, l, insert r key value)

  let add t key value =
    let tree =
      match t.tree with
      | Empty -> Node (key, value, Empty, Empty)
      | Node (k, v, l, r) ->
        let (k, v, l, r) = splay (k, v, l, r) key in
        let tree = Node (k, v, l, r) in
        t.tree <- tree;  (* save balanced tree *)
        (* insertion in this tree *)
        insert tree key value
    in
    { tree; }

  let singleton key value =
    add (empty ()) key value

  (** Merge of trees, where a < b *)
  let rec left_merge a b =
    match a, b with
    | Empty, Empty -> Empty
    | Node (k, v, l, r), b -> Node (k, v, l, left_merge r b)
    | Empty, b -> b

  let remove t key =
    match t.tree with
    | Empty -> t
    | Node (k, v, l, r) ->
      let (k, v, l, r) = splay (k, v, l, r) key in
      t.tree <- Node (k, v, l, r);
      if X.compare key k = 0
        then (* remove the node, by merging the subnodes *)
          let tree = left_merge l r in
          { tree; }
        else (* not present, same tree *)
          t

  let iter t f =
    let rec iter t = match t with
    | Empty -> ()
    | Node (k, v, l, r) ->
      iter l;
      f k v;
      iter r
    in iter t.tree

  let fold t acc f =
    let rec fold acc t = match t with
    | Empty -> acc
    | Node (k, v, l, r) ->
      let acc = fold acc l in
      let acc = f acc k v in
      fold acc r
    in
    fold acc t.tree

  let size t = fold t 0 (fun acc _ _ -> acc+1)

  let choose t =
    match t.tree with
    | Empty -> raise Not_found
    | Node (k, v, _, _) -> k, v

  let to_seq t =
    fun kont -> iter t (fun k v -> kont (k, v))

  let of_seq t seq =
    let t = ref t in
    seq (fun (k, v) -> t := add !t k v);
    !t
end
