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

(** {1 Splay trees} *)

(** See http://en.wikipedia.org/wiki/Splay_tree and
    Okasaki's "purely functional data structures" p46 *)

type ('a, 'b) t = (('a, 'b) tree * ('a -> 'a -> int))
  (** A splay tree with the given comparison function *)
and ('a, 'b) tree =
  | Empty
  | Node of (('a,'b) tree * 'a * 'b * ('a,'b) tree)
  (** A splay tree containing values of type 'a *)

let empty ~cmp =
  (Empty, cmp)

let is_empty (tree, _) =
  match tree with
  | Empty -> true
  | Node _ -> false

(** Partition the tree into (elements <= pivot, elements > pivot) *)
let rec partition ~cmp pivot tree =
  match tree with
  | Empty -> Empty, Empty
  | Node (a, x, x_val, b) ->
    if cmp x pivot <= 0
      then begin
        match b with
        | Empty -> (tree, Empty)
        | Node (b1, y, y_val, b2) ->
          if cmp y pivot <= 0
            then
              let small, big = partition ~cmp pivot b2 in
              Node (Node (a, x, x_val, b1), y, y_val, small), big
            else
              let small, big = partition ~cmp pivot b1 in
              Node (a, x, x_val, small), Node (big, y, y_val, b2)
      end else begin
        match a with
        | Empty -> (Empty, tree)
        | Node (a1, y, y_val, a2) ->
          if cmp y pivot <= 0
            then
              let small, big = partition ~cmp pivot a2 in
              Node (a1, y, y_val, small), Node (big, x, x_val, b)
            else
              let small, big = partition ~cmp pivot a1 in
              small, Node (big, y, y_val, Node (a2, x, x_val, b))
      end

(** Insert the pair (key -> value) in the tree *)
let insert (tree, cmp) k v =
  let small, big = partition ~cmp k tree in
  let tree' = Node (small, k, v, big) in
  tree', cmp

let remove (tree, cmp) k = failwith "not implemented"

let replace (tree, cmp) k = failwith "not implemented"

(** Returns the top value, or raise Not_found is empty *)
let top (tree, _) =
  match tree with
  | Empty -> raise Not_found
  | Node (_, k, v, _) -> k, v

(** Access minimum value *)
let min (tree, _) =
  let rec min tree =
    match tree with
    | Empty -> raise Not_found
    | Node (Empty, k, v, _) -> k, v
    | Node (l, _, _, _) -> min l
  in min tree

(** Get minimum value and remove it from the tree *)
let delete_min (tree, cmp) =
  let rec delete_min tree = match tree with
  | Empty -> raise Not_found
  | Node (Empty, x, x_val, b) -> x, x_val, b
  | Node (Node (Empty, x, x_val, b), y, y_val, c) ->
    x, x_val, Node (b, y, y_val, c)  (* rebalance *)
  | Node (Node (a, x, x_val, b), y, y_val, c) ->
    let m, m_val, a' = delete_min a in
    m, m_val, Node (a', x, x_val, Node (b, y, y_val, c))
  in
  let m, m_val, tree' = delete_min tree in
  m, m_val, (tree', cmp)

(** Find the value for the given key (or raise Not_found).
    It also returns the splayed tree *)
let find (tree, cmp) k =
  failwith "not implemented" 

let find_fold (tree, cmp) k f acc =
  acc (* TODO *)

(** Iterate on elements *)
let iter (tree, _) f =
  let rec iter tree =
    match tree with
    | Empty -> ()
    | Node (a, x, x_val, b) ->
      iter a;
      f x x_val;
      iter b
  in iter tree

(** Number of elements (linear) *)
let size t =
  let r = ref 0 in
  iter t (fun _ _ -> incr r);
  !r

let get_cmp (_, cmp) = cmp
