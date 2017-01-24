
(* This file is free software, part of containers. See file "license" for more details. *)

(** {2 Imperative Bitvectors}

    The size of the bitvector is rounded up to the multiple of 30 or  62.
    In other words some functions such as {!iter} might iterate on more
    bits than what was originally asked for.
*)

type t
(** A resizable bitvector *)

val empty : unit -> t
(** Empty bitvector *)

val create : size:int -> bool -> t
(** Create a bitvector of given size, with given default value *)

val copy : t -> t
(** Copy of bitvector *)

val cardinal : t -> int
(** Number of bits set *)

val length : t -> int
(** Length of underlying array *)

val resize : t -> int -> unit
(** Resize the BV so that it has at least the given physical length
    [resize bv n] should make [bv] able to store [(Sys.word_size - 2)* n] bits *)

val is_empty : t -> bool
(** Any bit set? *)

val set : t -> int -> unit
(** Set i-th bit. *)

val get : t -> int -> bool
(** Is the i-th bit true? Returns false if the index is too high*)

val reset : t -> int -> unit
(** Set i-th bit to 0 *)

val flip : t -> int -> unit
(** Flip i-th bit *)

val clear : t -> unit
(** Set every bit to 0 *)

val iter : t -> (int -> bool -> unit) -> unit
(** Iterate on all bits *)

val iter_true : t -> (int -> unit) -> unit
(** Iterate on bits set to 1 *)

val to_list : t -> int list
(** List of indexes that are true *)

val to_sorted_list : t -> int list
(** Same as {!to_list}, but also guarantees the list is sorted in
    increasing order *)

val of_list : int list -> t
(** From a list of true bits *)

val first : t -> int
(** First set bit, or
    @raise Not_found if all bits are 0 *)

val filter : t -> (int -> bool) -> unit
(** [filter bv p] only keeps the true bits of [bv] whose [index]
    satisfies [p index] *)

val union_into : into:t -> t -> unit
(** [union ~into bv] sets [into] to the union of itself and [bv]. *)

val inter_into : into:t -> t -> unit
(** [inter ~into bv] sets [into] to the intersection of itself and [bv] *)

val union : t -> t -> t
(** [union bv1 bv2] returns the union of the two sets *)

val inter : t -> t -> t
(** [inter bv1 bv2] returns the intersection of the two sets *)

val select : t -> 'a array -> 'a list
(** [select arr bv] selects the elements of [arr] whose index
    corresponds to a true bit in [bv]. If [bv] is too short, elements of [arr]
    with too high an index cannot be selected and are therefore not
    selected. *)

val selecti : t -> 'a array -> ('a * int) list
(** Same as {!select}, but selected elements are paired with their index *)

type 'a sequence = ('a -> unit) -> unit

val to_seq : t -> int sequence
val of_seq : int sequence -> t

val print : Format.formatter -> t -> unit
(** Print the bitvector as a string of bits
    @since 0.13 *)
