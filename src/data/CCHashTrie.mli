
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Tries}

  Trie indexed by the hash of the keys, where the branching factor is fixed.
  The goal is to have a quite efficient functional structure with fast
  update and access {b if} the hash function is good.
  The trie is not binary, to improve cache locality and decrease depth.

  {b status: experimental}

  @since NEXT_RELEASE
*)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Fixed-Size Arrays} *)
module type FIXED_ARRAY = sig
  type 'a t
  val create : 'a -> 'a t
  val length_log : int
  val length : int  (* 2 power length_log *)
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val update : 'a t -> int -> ('a -> 'a) -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

(** {2 Signature} *)
module type S = sig
  module A : FIXED_ARRAY

  type key

  type 'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val singleton : key -> 'a -> 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if key not present *)

  val remove : key -> 'a t -> 'a t

  val cardinal : _ t -> int

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val of_list : (key * 'a) list -> 'a t

  val print : key printer -> 'a printer -> 'a t printer

  val as_tree : 'a t -> [`L of int * (key * 'a) list | `N ] ktree
  (** For debugging purpose: explore the structure of the tree,
      with [`L (h,l)] being a leaf (with shared hash [h])
      and [`N] an inner node *)
end

(** {2 Type for keys} *)
module type KEY = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Functors} *)
module Make(K : KEY) : S with type key = K.t
