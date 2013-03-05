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

type 'a t = ('a, unit) SplayTree.t ref
  (** The heap is a reference to a splay tree *)

(** Create an empty heap *)
let empty ~cmp =
  ref (SplayTree.empty ~cmp)

(** Insert a value in the heap *)
let insert heap x =
  heap := SplayTree.insert !heap x ()

(** Check whether the heap is empty *)
let is_empty heap =
  SplayTree.is_empty !heap

(** Access the minimal value of the heap, or raises Empty *)
let min (heap : 'a t) : 'a =
  let elt, _ = SplayTree.min !heap in
  elt

(** Discard the minimal element *)
let junk heap =
  let _, (), tree' = SplayTree.delete_min !heap in
  heap := tree'

(** Remove and return the mininal value (or raise Invalid_argument) *)
let pop heap =
  let elt, (), tree' = SplayTree.delete_min !heap in
  heap := tree';
  elt

(** Iterate on the elements, in an unspecified order *)
let iter heap k =
  SplayTree.iter !heap (fun elt _ -> k elt)

let to_seq heap =
  Sequence.from_iter (fun k -> iter heap k)

let of_seq heap seq =
  Sequence.iter (fun elt -> insert heap elt) seq
