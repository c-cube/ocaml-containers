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

(** {1 Functional (persistent) extensible union-find} *)

(** {2 Persistent array} *)

module PArray : sig
  type 'a t

  val make : int -> 'a -> 'a t

  val init : int -> (int -> 'a) -> 'a t

  val get : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> 'a t

  val length : 'a t -> int

  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val extend : 'a t -> int -> 'a -> unit
    (** Extend [t] to the given [size], initializing new elements with [elt] *)

  val extend_init : 'a t -> int -> (int -> 'a) -> unit
    (** Extend [t] to the given [size], initializing elements with [f] *)
end

(** {2 Persistent Bitvector} *)

module PBitVector : sig
  type t

  val make : int -> t
    (** Create a new bitvector of the given initial size (in words) *)

  val get : t -> int -> bool
    (** [get bv i] gets the value of the [i]-th element of [bv] *)

  val set : t -> int -> bool -> t
    (** [set bv i v] sets the value of the [i]-th element of [bv] to [v] *)

  val clear : t -> t
    (** Bitvector with all bits set to 0 *)

  val set_true : t -> int -> t
  val set_false : t -> int -> t
end

(** {2 Type with unique identifier} *)

module type ID = sig
  type t
  val get_id : t -> int
    (** Unique integer ID for the element. Must be >= 0. *)
end

(** {2 Persistent Union-Find with explanations} *)

module type S = sig
  type elt
    (** Elements of the Union-find *)

  type 'e t
    (** An instance of the union-find, ie a set of equivalence classes; It
        is parametrized by the type of explanations. *)

  val create : int -> 'e t
    (** Create a union-find of the given size. *)

  val find : 'e t -> elt -> elt
    (** [find uf a] returns the current representative of [a] in the given
        union-find structure [uf]. By default, [find uf a = a]. *)

  val union : 'e t -> elt -> elt -> 'e -> 'e t
    (** [union uf a b why] returns an update of [uf] where [find a = find b],
        the merge being justified by [why]. *)

  val distinct : 'e t -> elt -> elt -> 'e t
    (** Ensure that the two elements are distinct. *)

  val must_be_distinct : _ t -> elt -> elt -> bool
    (** Should the two elements be distinct? *)

  val fold_equiv_class : _ t -> elt -> ('a -> elt -> 'a) -> 'a -> 'a
    (** [fold_equiv_class uf a f acc] folds on [acc] and every element
        that is congruent to [a] with [f]. *)

  val iter_equiv_class : _ t -> elt -> (elt -> unit) -> unit
    (** [iter_equiv_class uf a f] calls [f] on every element of [uf] that
        is congruent to [a], including [a] itself. *)

  val iter : _ t -> (elt -> unit) -> unit
  (** Iterate on all root values
      @since NExT_RELEASE *)

  val inconsistent : _ t -> (elt * elt * elt * elt) option
    (** Check whether the UF is inconsistent. It returns [Some (a, b, a', b')]
        in case of inconsistency, where a = b, a = a' and b = b' by congruence,
        and a' != b' was a call to [distinct]. *)

  val common_ancestor : 'e t -> elt -> elt -> elt
    (** Closest common ancestor of the two elements in the proof forest *)

  val explain_step : 'e t -> elt -> (elt * 'e) option
    (** Edge from the element to its parent in the proof forest; Returns
        None if the element is a root of the forest. *)

  val explain : 'e t -> elt -> elt -> 'e list
    (** [explain uf a b] returns a list of labels that justify why
        [find uf a = find uf b]. Such labels were provided by [union]. *)

  val explain_distinct : 'e t -> elt -> elt -> elt * elt
    (** [explain_distinct uf a b] gives the original pair [a', b'] that
        made [a] and [b] distinct by calling [distinct a' b']. The
        terms must be distinct, otherwise Failure is raised. *)
end

module Make(X : ID) : S with type elt = X.t
