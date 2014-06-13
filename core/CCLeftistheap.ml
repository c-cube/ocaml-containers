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

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option

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

let add = insert

let filter heap p =
  let rec filter tree p = match tree with
  | Empty -> Empty
  | Node (_, x, l, r) when p x ->
    merge_tree heap.leq (Node (1, x, Empty, Empty))
      (merge_tree heap.leq (filter l p) (filter r p))
  | Node (_, _, l, r) -> merge_tree heap.leq (filter l p) (filter r p)
  in
  { heap with tree = filter heap.tree p; }

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

let take heap = match heap.tree with
  | Empty -> None
  | Node (_, x, a, b) ->
    let tree = merge_tree heap.leq a b in
    let heap' = { heap with tree; } in
    Some (x, heap')

let iter f heap =
  let rec iter t = match t with
    | Empty -> ()
    | Node (_, x, a, b) ->
      f x;
      iter a;
      iter b;
  in iter heap.tree

let fold f acc h =
  let rec fold acc h = match h with
    | Empty -> acc
    | Node (_, x, a, b) ->
        let acc = f acc x in
        let acc = fold acc a in
        fold acc b
  in fold acc h.tree

let size heap =
  let r = ref 0 in
  iter (fun _ -> incr r) heap;
  !r

let of_seq heap seq =
  let h = ref heap in
  seq (fun x -> h := insert !h x);
  !h

let to_seq h k = iter k h

let rec of_klist h l = match l() with
  | `Nil -> h
  | `Cons (x, l') ->
      let h' = add h x in
      of_klist h' l'

let to_klist h =
  let rec next stack () = match stack with
    | [] -> `Nil
    | Empty :: stack' -> next stack' ()
    | Node (_, x, a, b) :: stack' ->
        `Cons (x, next (a :: b :: stack'))
  in
  next [h.tree]

let rec of_gen h g = match g () with
  | None -> h
  | Some x ->
      of_gen (add h x) g

let to_gen h =
  let stack = Stack.create () in
  Stack.push h.tree stack;
  let rec next () =
    if Stack.is_empty stack
    then None
    else match Stack.pop stack with
      | Empty -> next()
      | Node (_, x, a, b) ->
          Stack.push a stack;
          Stack.push b stack;
          Some x
  in next
