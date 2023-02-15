(** Imperative Bitvectors.

    A bitvector is stored in some form of internal array (on the heap).
    Is it a bit similar to a more storage-efficient version of [bool
    CCVector.vector], with additional operations.

    {b BREAKING CHANGES} since 1.2:
    size is now stored along with the bitvector. Some functions have
    a new signature.

    The size of the bitvector used to be rounded up to the multiple of 30 or 62.
    In other words some functions such as {!val-iter} would iterate on more
    bits than what was originally asked for. This is not the case anymore.
*)

type t
(** A resizable bitvector *)

val empty : unit -> t
(** Empty bitvector. Length is 0. *)

val create : size:int -> bool -> t
(** Create a bitvector of given size, with given default value.
    Length of result is [size]. *)

val init : int -> (int -> bool) -> t
(** [init len f] initializes a bitvector of length [len], where bit [i]
    is true iff [f i] is.
    @since 3.9 *)

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
(** Resize the BV so that it has the specified length. This can grow
    the underlying array, but it will not shrink it, to minimize
    memory traffic.
    @raise Invalid_argument on negative sizes. *)

val resize_minimize_memory : t -> int -> unit
(** Same as {!resize}, but this can also shrink the underlying
    array if this reduces the size.
    @raise Invalid_argument on negative sizes.
    @since 3.9 *)

val is_empty : t -> bool
(** Are there any true bits? *)

val set : t -> int -> unit
(** Set i-th bit, extending the bitvector if needed. *)

val get : t -> int -> bool
(** Is the i-th bit true? Return false if the index is too high. *)

val reset : t -> int -> unit
(** Set i-th bit to 0, extending the bitvector if needed. *)

val set_bool : t -> int -> bool -> unit
(** Set or reset [i]-th bit.
    @since 3.9 *)

val flip : t -> int -> unit
(** Flip i-th bit, extending the bitvector if needed. *)

val clear : t -> unit
(** Set every bit to 0. Does not change the length. *)

val clear_and_shrink : t -> unit
(** Set every bit to 0, and set length to 0.
    @since 3.9 *)

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
    satisfies [p index].
    Length is unchanged. *)

val negate_self : t -> unit
(** [negate_self t] flips all of the bits in [t]. Length is unchanged.

    @since 1.2 *)

val negate : t -> t
(** [negate t] returns a copy of [t] with all of the bits flipped.
    Length is unchanged. *)

val union_into : into:t -> t -> unit
(** [union_into ~into bv] sets [into] to the union of itself and [bv].
    Also updates the length of [into] to be at least [length bv]. *)

val inter_into : into:t -> t -> unit
(** [inter_into ~into bv] sets [into] to the intersection of itself and [bv].
    Also updates the length of [into] to be at most [length bv].

    After executing:
    - [length ~into' = min (length into) (length bv)].
    - [for all i: get into' ==> get into i /\ get bv i]
  *)

val union : t -> t -> t
(** [union bv1 bv2] returns the union of the two sets. The length
     of the result is the max of the inputs' lengths. *)

val inter : t -> t -> t
(** [inter bv1 bv2] returns the intersection of the two sets. The length
    of the result is the min of the inputs' lengths. *)

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
(** Iterate over the true bits. *)

val of_iter : int iter -> t
(** Build from true bits. *)

val pp : Format.formatter -> t -> unit
(** Print the bitvector as a string of bits.
    @since 0.13 *)

(**/**)

module Internal_ : sig
  val __to_word_l : t -> char list
  val __popcount8 : int -> int
  val __lsb_mask : int -> int
  val __check_invariant : t -> unit
end

(**/**)
