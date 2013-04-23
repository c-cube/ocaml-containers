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

(** {1 Leftist Heaps} *)

(** Polymorphic implementation, following Okasaki *)

type 'a t = {
  tree : 'a tree;
  leq : 'a -> 'a -> bool;
} (** Empty heap. The function is used to check whether
      the first element is smaller than the second. *)
and 'a tree =
  | Empty
  | Node of int * 'a * 'a tree * 'a tree

let empty_with ~leq =
  { tree = Empty;
    leq;
  }

let empty =
  { tree = Empty;
    leq = (fun x y -> x <= y);
  }

let is_empty heap =
  match heap.tree with
  | Empty -> true
  | _ -> false

(** Rank of the tree *)
let rank_tree t = match t with
  | Empty -> 0
  | Node (r, _, _, _) -> r

(** Make a balanced node labelled with [x], and subtrees [a] and [b] *)
let make_node x a b =
  if rank_tree a >= rank_tree b
    then Node (rank_tree b + 1, x, a, b)
    else Node (rank_tree a + 1, x, b, a)

let rec merge_tree leq t1 t2 =
  match t1, t2 with
  | t, Empty -> t
  | Empty, t -> t
  | Node (_, x, a1, b1), Node (_, y, a2, b2) ->
    if leq x y
      then make_node x a1 (merge_tree leq b1 t2)
      else make_node y a2 (merge_tree leq t1 b2)

let merge h1 h2 =
  let tree = merge_tree h1.leq h1.tree h2.tree in
  { tree; leq=h1.leq; }

let insert heap x =
  let tree = merge_tree heap.leq (Node (1, x, Empty, Empty)) heap.tree in
  { heap with tree; }

let find_min heap =
  match heap.tree with
  | Empty -> raise Not_found
  | Node (_, x, _, _) -> x

let extract_min heap =
  match heap.tree with
  | Empty -> raise Not_found
  | Node (_, x, a, b) ->
    let tree = merge_tree heap.leq a b in
    let heap' = { heap with tree; } in
    heap', x

let iter heap f =
  let rec iter t = match t with
    | Empty -> ()
    | Node (_, x, a, b) ->
      f x;
      iter a;
      iter b;
  in iter heap.tree

let size heap =
  let r = ref 0 in
  iter heap (fun _ -> incr r);
  !r

let of_seq heap seq =
  Sequence.fold insert heap seq

let to_seq heap =
  Sequence.from_iter (fun k -> iter heap k)
