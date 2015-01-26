
(*
copyright (c) 2013-2014, Simon Cruanes, Emmanuel Surleau
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 Rose Tree}

   A persistent, non-lazy tree where each node may have an arbitrary number of
   children.

   @since NEXT_RELEASE *)

(** The type of a tree node - a (value, children) pair.  *)
type +'a t = [`Node of 'a * 'a t list]

type 'a tree = 'a t

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

(**
   Folds over the tree. Takes a function [f node accumulator], an initial value
   for the accumulator, and the tree to operate on.
*)
val fold : f : ('a -> 'b -> 'b) -> 'b ->  'a t -> 'b

(** Iterate over the tree *)
val to_seq : 'a t -> 'a sequence

(**
   Tree pretty-printer. Takes a [Formatter], a function turning a node into a
   string, and the tree itself as parameters. Appends the result to the
   formatter.
*)
val print : 'a printer -> 'a t printer

(**
   {2 Zipper}

   A zipper to navigate and return modified versions of the tree.
*)
module Zipper : sig

  type 'a t

  (**
     Builds a zipper from a tree.
  *)
  val zipper : 'a tree -> 'a t

  (**
     Returns the tree associated to the zipper.
  *)
  val tree : 'a t -> 'a tree

  (**
     Moves to the left of the currently focused node, if possible. Returns [Some
     new_zipper], or [None] if the focused node had no left sibling.
  *)
  val left_sibling : 'a t -> ('a t) option

  (**
     Moves to the right of the currently focused node, if possible. Returns [Some
     new_zipper], or [None] if the focused node had no right sibling.
  *)
  val right_sibling : 'a t -> ('a t) option

  (**
     Moves one level up of the currently focused node, if possible. Returns
     [Some new_zipper], or [None] if the focused node was the root.
  *)
  val parent : 'a t -> ('a t) option

  (**
     Moves to the root of the tree.
  *)
  val root : 'a t -> 'a t

  (**
     Moves to the nth child of the current node. Accepts the child number,
     starting from zero. Returns [Some new_zipper], or [None] if there was no
     such child.
  *)
  val nth_child : int -> 'a t -> ('a t) option

  (**
     Inserts a new node as the leftmost child of the currently focused node.
     Returns a new zipper, focused on the newly inserted node.
  *)
  val append_child : 'a tree -> 'a t -> 'a t

  (**
     Inserts a new node to the left of the currently focused node.
     Returns [Some new_zipper], focused on the newly inserted node, if the
     focused node is not the root. If the currently focused node is the root,
     returns [None].
  *)
  val insert_left_sibling : 'a tree -> 'a t -> ('a t) option

  (**
     Inserts a new node to the right of the currently focused node.
     Returns [Some new_zipper], focused on the newly inserted node, if the
     focused node is not the root. If the currently focused node is the root,
     returns [None].
  *)
  val insert_right_sibling : 'a tree -> 'a t -> ('a t) option

  (**
     Replaces the currently focused node with a new node.
     Returns a new zipper, focused on the new node.
  *)
  val replace : 'a tree -> 'a t -> 'a t

  (**
     Deletes the currently focused node.
     If the currently focused node is the root, returns [None].
     Otherwise, returns a [Some new_zipper]. It is focused on the left sibling
     of the deleted node. If there is no left sibling available, the zipper is
     focused on the right sibling. If there are no siblings, the zipper is
     focused on the parent of the focused node.
  *)
  val delete : 'a t -> ('a t) option

end
