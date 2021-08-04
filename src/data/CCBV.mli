
(* This file is free software, part of containers. See file "license" for more details. *)

(** {2 Imperative Bitvectors}

    {b BREAKING CHANGES} since 1.2:
    size is now stored along with the bitvector. Some functions have
    a new signature.

    The size of the bitvector used to be rounded up to the multiple of 30 or 62.
    In other words some functions such as {!iter} would iterate on more
    bits than what was originally asked for. This is not the case anymore.
*)

type t
(** A resizable bitvector *)

val empty : unit -> t
(** Empty bitvector. *)

val create : size:int -> bool -> t
(** Create a bitvector of given size, with given default value. *)

val copy : t -> t
(** Copy of bitvector. *)

val cardinal : t -> int
(** Number of bits set to one, seen as a set of bits. *)

val length : t -> int
(** Size of underlying bitvector.
    This is not related to the underlying implementation.
    Changed at 1.2
*)

val capacity : t -> int
(** The number of bits this bitvector can store without resizing.

    @since 1.2 *)

val resize : t -> int -> unit
(** Resize the BV so that it has the specified length. This can grow or shrink
    the underlying bitvector.

    @raise Invalid_arg on negative sizes. *)

val is_empty : t -> bool
(** Are there any true bits? *)

val set : t -> int -> unit
(** Set i-th bit, extending the bitvector if needed. *)

val get : t -> int -> bool
(** Is the i-th bit true? Return false if the index is too high. *)

val reset : t -> int -> unit
(** Set i-th bit to 0, extending the bitvector if needed. *)

val flip : t -> int -> unit
(** Flip i-th bit, extending the bitvector if needed. *)

val clear : t -> unit
(** Set every bit to 0. *)

val iter : t -> (int -> bool -> unit) -> unit
(** Iterate on all bits. *)

val iter_true : t -> (int -> unit) -> unit
(** Iterate on bits set to 1. *)

val to_list : t -> int list
(** List of indexes that are true. *)

val to_sorted_list : t -> int list
(** Same as {!to_list}, but also guarantees the list is sorted in
    increasing order. *)

val of_list : int list -> t
(** From a list of true bits.

    The bits are interpreted as indices into the returned bitvector, so the final
    bitvector [bv] will have [length bv] equal to 1 more than max of list indices.
*)

val first : t -> int option
(** First set bit, or return [None].
    Changed type at 1.2 *)

val first_exn : t -> int
(** First set bit, or
    @raise Not_found if all bits are 0.
    @since 1.2 *)

val filter : t -> (int -> bool) -> unit
(** [filter bv p] only keeps the true bits of [bv] whose [index]
    satisfies [p index]. *)

val negate_self : t -> unit
(** [negate_self t] flips all of the bits in [t].

    @since 1.2 *)

val negate : t -> t
(** [negate t] returns a copy of [t] with all of the bits flipped. *)

val union_into : into:t -> t -> unit
(** [union_into ~into bv] sets [into] to the union of itself and [bv].
    Also updates the length of [into] to be at least [length bv]. *)

val inter_into : into:t -> t -> unit
(** [inter_into ~into bv] sets [into] to the intersection of itself and [bv].
    Also updates the length of [into] to be at most [length bv]. *)

val union : t -> t -> t
(** [union bv1 bv2] returns the union of the two sets. *)

val inter : t -> t -> t
(** [inter bv1 bv2] returns the intersection of the two sets. *)

val diff_into : into:t -> t -> unit
(** [diff_into ~into t] modifies [into] with only the bits set but not in [t].

    @since 1.2 *)

val diff : t -> t -> t
(** [diff t1 t2] returns those bits found in [t1] but not in [t2].

    @since 1.2 *)

val select : t -> 'a array -> 'a list
(** [select arr bv] selects the elements of [arr] whose index
    corresponds to a true bit in [bv]. If [bv] is too short, elements of [arr]
    with too high an index cannot be selected and are therefore not
    selected. *)

val selecti : t -> 'a array -> ('a * int) list
(** Same as {!select}, but selected elements are paired with their indexes. *)

val equal : t -> t -> bool
(** Bitwise comparison, including the size ([equal a b] implies [length a=length b]).
    @since 3.5 *)

type 'a iter = ('a -> unit) -> unit

val to_iter : t -> int iter
val of_iter : int iter -> t

val pp : Format.formatter -> t -> unit
(** Print the bitvector as a string of bits.
    @since 0.13 *)


(**/**)
val __to_word_l : t -> int list
(**/**)
