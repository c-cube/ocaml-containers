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

(** {1 Binary heaps} *)

type 'a t = {
  tree: 'a tree;
  lt: 'a -> 'a -> bool;
} (** A heap containing values of type 'a *)
and 'a tree =
  | Empty
  | Node of int * 'a tree * 'a * 'a tree
  (** The complete binary tree (int is max depth) *)

(** Create an empty heap *)
let empty ~lt =
  { tree = Empty;
    lt;
  }

(** Insert a value in the heap *)
let insert heap x =
  let rec insert tree x =
    match tree with
    | Empty -> Node (1, Empty, x, Empty)
    | Node (d, l, y, r) -> failwith "TODO"
  in
  { heap with tree = insert heap.tree x }

(** Check whether the heap is empty *)
let is_empty heap =
  match heap.tree with
  | Empty -> true
  | _ -> false

(** Access the minimal value of the heap, or raises Empty *)
let min heap =
  match heap.tree with
  | Node (_, _, x, _) -> x
  | Empty -> raise (Invalid_argument "Heap.min on empty heap")

(** Discard the minimal element *)
let junk heap = failwith "TODO: Heap.junk"

(** Iterate on the elements, in an unspecified order *)
let iter heap k =
  let rec iter tree = match tree with
  | Empty -> ()
  | Node (_, l, x, r) ->
    iter l;
    k x;
    iter r
  in iter heap.tree
