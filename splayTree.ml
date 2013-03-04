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

let rec bigger ~cmp pivot tree =
  match tree with
  | Empty -> Empty
  | Node (a, x, x_val, b) ->
    if cmp x pivot <= 0
      then bigger ~cmp pivot b
      else match a with
        | Empty -> Node (Empty, x, x_val, b)
        | Node (a1, y, y_val, a2) ->
          if cmp y pivot <= 0
            then Node (bigger ~cmp pivot a2, x, x_val, b)
            else Node (bigger ~cmp pivot a1, y, y_val, Node (a2, x, x_val, b))

let rec smaller ~cmp pivot tree =

(** Insert the pair (key -> value) in the tree *)
let insert (tree, cmp) k v =
  let tree' = Node (smaller ~cmp k tree, k, v, bigger ~cmp k tree) in
  tree', cmp

