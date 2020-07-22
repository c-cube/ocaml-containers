(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bijection}
    Represents 1-to-1 mappings between two types. Each element from the "left"
    is mapped to one "right" value, and conversely.

    @since 2.1 *)

type 'a iter = ('a -> unit) -> unit

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

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val add : left -> right -> t -> t
  (** Add [left] and [right] correspondence to bijection such that
      [left] and [right] are unique in their respective sets and only
      correspond to each other. *)

  val cardinal : t -> int
  (** Number of bindings. O(n) time. *)

  val mem : left -> right -> t -> bool
  (** Check both sides for key membership. *)

  val mem_left : left -> t -> bool
  (** Check for membership of correspondence using [left] key. *)

  val mem_right : right -> t -> bool
  (** Check for membership of correspondence using [right] key. *)

  val find_left  : left ->  t -> right
  (** @raise Not_found if left is not found. *)

  val find_right : right -> t -> left
  (** @raise Not_found if right is not found. *)

  val remove : left -> right -> t -> t
  (** Remove the [left], [right] binding if it exists. Return the
      same bijection otherwise. *)

  val remove_left : left -> t -> t
  (** Remove the binding with [left] key if it exists. Return the
      same bijection otherwise. *)

  val remove_right : right -> t -> t
  (** Remove the binding with [right] key if it exists. Return the
      same bijection otherwise. *)

  val list_left : t -> (left * right) list
  (** Return the bindings as a list of ([left], [right]) values. *)

  val list_right : t -> (right * left) list
  (** Return the bindings as a list of [(right, left)] values. *)

  val add_iter : (left * right) iter -> t -> t

  val of_iter : (left * right) iter -> t

  val to_iter : t -> (left * right) iter

  val add_list : (left * right) list -> t -> t

  val of_list : (left * right) list -> t

  val to_list : t -> (left * right) list
end

module Make(L : OrderedType)(R : OrderedType) : S
  with type left = L.t and type right = R.t
