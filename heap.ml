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

(** {1 Imperative priority queue} *)

module Tree = struct

  type 'a t = 'a tree * ('a -> 'a -> int)
    (** A splay tree with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of ('a tree * 'a * 'a tree)
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
    | Node (a, x, b) ->
      if cmp x pivot <= 0
        then begin
          match b with
          | Empty -> (tree, Empty)
          | Node (b1, y, b2) ->
            if cmp y pivot <= 0
              then
                let small, big = partition ~cmp pivot b2 in
                Node (Node (a, x, b1), y, small), big
              else
                let small, big = partition ~cmp pivot b1 in
                Node (a, x, small), Node (big, y, b2)
        end else begin
          match a with
          | Empty -> (Empty, tree)
          | Node (a1, y, a2) ->
            if cmp y pivot <= 0
              then
                let small, big = partition ~cmp pivot a2 in
                Node (a1, y, small), Node (big, x, b)
              else
                let small, big = partition ~cmp pivot a1 in
                small, Node (big, y, Node (a2, x, b))
        end

  (** Insert the element in the tree *)
  let insert (tree, cmp) x =
    let small, big = partition ~cmp x tree in
    let tree' = Node (small, x, big) in
    tree', cmp

  (** Returns the top value, or raise Not_found is empty *)
  let top (tree, _) =
    match tree with
    | Empty -> raise Not_found
    | Node (_, x, _) -> x

  (** Access minimum value *)
  let min (tree, _) =
    let rec min tree =
      match tree with
      | Empty -> raise Not_found
      | Node (Empty, x, _) -> x
      | Node (l, _, _) -> min l
    in min tree

  (** Get minimum value and remove it from the tree *)
  let delete_min (tree, cmp) =
    let rec delete_min tree = match tree with
    | Empty -> raise Not_found
    | Node (Empty, x, b) -> x, b
    | Node (Node (Empty, x, b), y, c) ->
      x, Node (b, y, c)  (* rebalance *)
    | Node (Node (a, x, b), y, c) ->
      let m, a' = delete_min a in
      m, Node (a', x, Node (b, y, c))
    in
    let m, tree' = delete_min tree in
    m, (tree', cmp)

  (** Iterate on elements *)
  let iter (tree, _) f =
    let rec iter tree =
      match tree with
      | Empty -> ()
      | Node (a, x, b) ->
        iter a; f x; iter b
    in iter tree
end

type 'a t = 'a Tree.t ref
  (** The heap is a reference to a splay tree *)

(** Create an empty heap *)
let empty ~cmp =
  ref (Tree.empty ~cmp)

(** Insert a value in the heap *)
let insert heap x =
  heap := Tree.insert !heap x

(** Check whether the heap is empty *)
let is_empty heap =
  Tree.is_empty !heap

(** Access the minimal value of the heap, or raises Empty *)
let min (heap : 'a t) : 'a =
  let elt = Tree.min !heap in
  elt

(** Discard the minimal element *)
let junk heap =
  let _, tree' = Tree.delete_min !heap in
  heap := tree'

(** Remove and return the mininal value (or raise Invalid_argument) *)
let pop heap =
  let elt, tree' = Tree.delete_min !heap in
  heap := tree';
  elt

(** Iterate on the elements, in an unspecified order *)
let iter heap k =
  Tree.iter !heap (fun elt -> k elt)

let size heap =
  let r = ref 0 in
  iter heap (fun _ -> incr r);
  !r

let to_seq heap =
  fun k -> iter heap k

let of_seq heap seq =
  seq (fun elt -> insert heap elt)
