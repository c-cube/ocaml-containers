(** Hash combinators with 64-bit state threading.

    State is threaded explicitly as a private [int64] through combinators.
    Finalize with {!finalize} (returns a positive [int]) or {!finalize64}
    (returns the raw [int64]).

    Typical use:
    {[
      let hash_my_record r =
        CCHash64.(finalize (string (int seed r.id) r.name))

      let hash_pair (a, b) =
        CCHash64.(finalize (pair int string seed (a, b)))
    ]}

    {b Implementation}: xorshift+multiply combiner with fmix64 (Murmur3)
    finalizer, via C stubs. Unboxed in native code.

    @since NEXT_RELEASE
*)

type state = private int64
(** Hash accumulator (64-bit). Create with {!seed}; finish with {!finalize}
    or {!finalize64}. *)

val seed : state
(** Initial hash state (golden-ratio constant). *)

val finalize64 : state -> int64
(** Apply fmix64 (Murmur3) and return the full 64-bit result.
    The result may be negative as a signed [int64]. *)

val finalize : state -> int
(** Apply fmix64 and return a non-negative [int] (strips sign bit). *)

type 'a t = state -> 'a -> state
(** A hash combiner: takes the current state, mixes in a value, returns the
    updated state. *)

val apply : 'a t -> 'a -> int64
(** Hash the input *)

val apply_int : 'a t -> 'a -> int
(** Hash the input and truncate to [int] *)

val int : int t
val bool : bool t
val char : char t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t
val string : string t

val bytes : bytes t
(** @since 3.5 *)

val slice : string -> int -> int t
(** [slice str ofs s len] mixes the byte slice [str[ofs .. ofs+len-1]] into [s]. *)

val opt : 'a t -> 'a option t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val map : ('a -> 'b) -> 'b t -> 'a t
(** [map proj f] applies [proj] before hashing with [f].
    Example: [map fst int] hashes only the first element of a pair.
    @since 3.5 *)

val if_ : bool -> 'a t -> 'a t -> 'a t
(** [if_ b t e] uses hasher [t] when [b] is true, [e] otherwise. *)

val combine2 : int64 -> int64 -> int64
val combine3 : int64 -> int64 -> int64 -> int64
val combine4 : int64 -> int64 -> int64 -> int64 -> int64

val poly : 'a t
(** Uses [Hashtbl.hash] internally. *)

val list_comm : 'a t -> 'a list t
(** Commutative: lists equal up to permutation hash the same. *)

val array_comm : 'a t -> 'a array t
(** Commutative: arrays equal up to permutation hash the same. *)

(** {2 Iterators} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val seq : 'a t -> 'a Seq.t t
val iter : 'a t -> 'a iter t
val gen : 'a t -> 'a gen t
