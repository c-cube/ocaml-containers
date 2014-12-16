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

(** {1 Small set structure} *)

type 'a sequence = ('a -> unit) -> unit

type 'a t = {
  cmp : 'a -> 'a -> int;
  nodes : 'a node;
} (** Set of elements of type 'a *)
and 'a node =
  | Empty
  | Node of 'a * 'a node
  (** Sorted list of 'a *)

let empty ~cmp =
  { cmp;
    nodes = Empty;
  }

let is_empty set =
  match set.nodes with
  | Empty -> true
  | Node _ -> false

let mem set x =
  let cmp = set.cmp in
  let rec explore node = match node with
    | Empty -> false
    | Node (y, node') ->
      let c = cmp x y in
      if c = 0 then true
      else if c > 0 then explore node'
      else false
  in
  explore set.nodes

let add set x =
  let cmp = set.cmp in
  let rec insert node = match node with
    | Empty -> Node (x, Empty)  (* insert here *)
    | Node (y, node') ->
      let c = cmp x y in
      if c = 0 then node  (* already there *)
      else if c > 0
        then
          let node'' = insert node' in
          if node' == node'' then node else Node (y, node'')
      else Node (x, node) (* insert before y *)
  in
  let nodes = insert set.nodes in
  if nodes == set.nodes
    then set
    else { set with nodes; }

let rec remove set x =
  let cmp = set.cmp in
  let rec remove node = match node with
    | Empty -> Empty
    | Node (y, node') ->
      let c = cmp x y in
      if c = 0 then node'
      else if c > 0
        then
          let node'' = remove node' in
          if node' == node'' then node else Node (y, node'')
      else node (* not present *)
  in
  let nodes = remove set.nodes in
  if nodes == set.nodes
    then set
    else { set with nodes; }

let choose set =
  match set.nodes with
  | Empty -> raise Not_found
  | Node (x, _) -> x

let fold f acc set =
  let rec fold f acc node = match node with
    | Empty -> acc
    | Node (x, node') ->
      let acc' = f acc x in
      fold f acc' node'
  in fold f acc set.nodes

let iter f set =
  let rec iter f node = match node with
    | Empty -> ()
    | Node (x, node') ->
      f x;
      iter f node'
  in iter f set.nodes

let size set =
  let r = ref 0 in
  iter (fun _ -> incr r) set;
  !r

let to_seq set =
  fun k ->
    iter k set

let of_seq set seq =
  let set = ref set in
  seq (fun x -> set := add !set x);
  !set

let to_list set =
  let l = ref [] in
  to_seq set (fun x -> l := x :: !l);
  !l

let of_list set l =
  List.fold_left add set l

