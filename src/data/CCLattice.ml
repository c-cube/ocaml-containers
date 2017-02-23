
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Lattice} *)

type 'a sequence = ('a -> unit) -> unit

module type SEMI_LATTICE = sig
  type t

  val equal : t -> t -> bool

  val leq : t -> t -> bool
  (** Partial order, which must satisfy:
      - [leq a b && leq b c => leq a c]
      - [leq a a]
  *)

  val meet : t -> t -> t
  (** Greatest lower bound.
      Must satisfy:
      - [leq (meet a b) a]
      - [leq (meet a b) b]
      - [meet a a = a]
      - [meet a b = meet b a]
  *)

  val bottom : t
  (** Minimal element, such that forall [x], [leq bottom x].
      In particular, this means that [meet bottom x = bottom] *)
end

module type S = sig
  type key

  type +'a t
  (** Map from [key] to values of type ['a] *)

  val empty : 'a t

  val find : key -> 'a t -> 'a

  val get : key -> 'a t -> 'a option

  val add : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val find_all_leq : key -> 'a t -> (key * 'a) sequence
  (** [find_all_leq x t] returns all elements of [t] that are smaller
      or equal to [x] *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_seq : (key * 'a) sequence -> 'a t

  val to_list : 'a t -> (key * 'a) list

  val of_list : (key * 'a) list -> 'a t
end

module Make(S : SEMI_LATTICE) = struct
  type key = S.t

  type bal = int (* -1,0,1 *)

  type +'a t =
    | Empty
    | Inner of key * 'a t * 'a t * bal (* AVL balancing *)
    | Leaf of key * 'a



end
