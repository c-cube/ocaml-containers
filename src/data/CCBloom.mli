(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bloom Filter}

    {b status: experimental}

    @since 0.13 *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

type 'a hash_funs = ('a -> int) array
(** An array of [k] hash functions on values of type ['a].
    Never ever modify such an array after use! *)

val default_hash_funs : int -> 'a hash_funs
(** Use {!Hashtbl.seeded_hash} on [k] seeds
    @param k the number of hash functions required *)

(** {2 Bloom Filter} *)

type 'a t
(** Bloom filter containing values of type ['a] *)

val create : ?hash:('a hash_funs) -> int -> 'a t
(** [create ?hash size] creates a filter with given size, and functions.
    By default it uses {!default_hash_funs}
    @param size a hint for size *)

val create_default : ?hash_len:int -> int -> 'a t
(** [create_default ?hash_len size] is the same as
    [create ~hash:(default_hash_funs hash_len) size].
    It uses the given number of default hash functions.
    @param size a hint for size *)

val copy : 'a t -> 'a t
(** Copy of the filter *)

val size : _ t -> int
(** Length of the underlying array. Do not confuse with a cardinal function,
    which is impossible to write for bloom filters *)

val load : _ t -> float
(** Ratio of 1 bits in the underlying array. The closer to [1.], the less
    accurate {!mem} is *)

val mem : 'a t -> 'a -> bool
(** [mem f x] tests whether [x] (probably) belongs to [f] *)

val add : 'a t -> 'a -> unit
(** [add f x] adds [x] into [f] *)

val union_mut : into:'a t -> 'a t -> unit
(** [union_mut ~into f] changes [into] into the union of [into] and [f].
    [into] and [f] MUST have the same set of hash functions
    @raise Invalid_argument if the two sets do not have the same size *)

val union : 'a t -> 'a t -> 'a t
(** The sets MUST have the same set of hash functions
    @raise Invalid_argument if the two sets do not have the same size *)

val inter_mut : into:'a t -> 'a t -> unit
(** [inter_mut ~into f] changes [into] into the intersection of [into] and [f]
    [into] and [f] MUST have the same set of hash functions
    @raise Invalid_argument if the two sets do not have the same size *)

val inter : 'a t -> 'a t -> 'a t
(** The sets MUST have the same set of hash functions
    @raise Invalid_argument if the two sets do not have the same size *)

(** {2 Conversions} *)

val add_list : 'a t -> 'a list -> unit

val add_seq : 'a t -> 'a sequence -> unit

val add_gen : 'a t -> 'a gen -> unit
