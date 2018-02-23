(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bijection}
    Represents 1-to-1 mappings between two types. Each element from the "left"
    is mapped to one "right" value, and conversely.

    @since NEXT_RELEASE *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  type left
  type right

  val empty : t

  val is_empty : t -> bool

  val add : left -> right -> t -> t
  (** Add [left] and [right] correspondence to bijection such that
     [left] and [right] are unique in their respective sets and only
     correspond to each other. *)

  val mem : left -> right -> t -> bool
  (** Checks both sides for key membership. Can raise [Incoherence of
     string] but should never happen *)

  val mem_left : left -> t -> bool
  (** Checks for membership of correspondence using [left] key *)

  val mem_right : right -> t -> bool
  (** Checks for membership of correspondence using [right] key *)

  val find_left  : left ->  t -> right
  (** Raises [Not_found] if left is not found *)

  val find_right : right -> t -> left
  (** Raises [Not_found] if right is not found *)

  val remove : left -> right -> t -> t
  (** Removes the [left], [right] binding if it exists. Returns the
     same bijection otherwise. *)

  val remove_left : left -> t -> t
  (** Remove the binding with [left] key if it exists. Returns the
     same bijection otheriwse *)

  val remove_right : right -> t -> t
  (** Remove the binding with [right] key if it exists. Returns the
     same bijection otheriwse *)

  val list_left : t -> (left * right) list
  (** returns the bindings as a list of ([left], [right]) values *)

  val list_right : t -> (right * left) list
  (** returns the bindings as a list of ([right, [left]) values *)

end

module Make(L : OrderedType)(R : OrderedType) : S
  with type left = L.t and type right = R.t
