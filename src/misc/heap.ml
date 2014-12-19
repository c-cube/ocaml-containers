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

type 'a sequence = ('a -> unit) -> unit

type 'a t = {
  mutable tree : 'a tree;
  cmp : 'a -> 'a -> int;
} (** A splay tree heap with the given comparison function *)
and 'a tree =
  | Empty
  | Node of ('a tree * 'a * 'a tree)
  (** A splay tree containing values of type 'a *)

let empty ~cmp = {
  tree = Empty;
  cmp;
}

let is_empty h =
  match h.tree with
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
let insert h x =
  let small, big = partition ~cmp:h.cmp x h.tree in
  let tree' = Node (small, x, big) in
  h.tree <- tree'

(** Access minimum value *)
let min h =
  let rec min tree =
    match tree with
    | Empty -> raise Not_found
    | Node (Empty, x, _) -> x
    | Node (l, _, _) -> min l
  in min h.tree

(** Get minimum value and remove it from the tree *)
let pop h =
  let rec delete_min tree = match tree with
  | Empty -> raise Not_found
  | Node (Empty, x, b) -> x, b
  | Node (Node (Empty, x, b), y, c) ->
    x, Node (b, y, c)  (* rebalance *)
  | Node (Node (a, x, b), y, c) ->
    let m, a' = delete_min a in
    m, Node (a', x, Node (b, y, c))
  in
  let m, tree' = delete_min h.tree in
  h.tree <- tree';
  m

let junk h =
  ignore (pop h)

(** Iterate on elements *)
let iter h f =
  let rec iter tree =
    match tree with
    | Empty -> ()
    | Node (a, x, b) ->
      iter a; f x; iter b
  in iter h.tree

let size h =
  let r = ref 0 in
  iter h (fun _ -> incr r);
  !r

let to_seq h =
  fun k -> iter h k

let of_seq h seq =
  seq (fun elt -> insert h elt)
