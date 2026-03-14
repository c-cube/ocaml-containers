(** XXHash bindings.

    Fast non-cryptographic hash functions from
    {{:https://github.com/Cyan4973/xxHash} xxHash}.

    Hashing uses XXH64. To hash a single value use the [hash_foo] convenience
    functions. To combine several values, fold with [mix_*] and call
    {!finalize}:

    {[
      let h =
        seed
        |> fun h -> mix_string h "hello"
        |> fun h -> mix_int h 42
        |> finalize
    ]}
*)

type state = private int64
(** Accumulated hash state. Represented as [int64] so the compiler can unbox
    it at call sites. *)

val seed : state
(** Initial state. Equal to [0L]. *)

external mix_int64 : (state[@unboxed]) -> (int64[@unboxed]) -> (state[@unboxed])
  = "caml_cc_xxhash_mix_int64_byte" "caml_cc_xxhash_mix_int64"
[@@noalloc]
(** Mix an [int64] value into the state. Noalloc and unboxed in native code. *)

external mix_int : (state[@unboxed]) -> (int[@untagged]) -> (state[@unboxed])
  = "caml_cc_xxhash_mix_int_byte" "caml_cc_xxhash_mix_int"
[@@noalloc]
(** Mix an [int] value into the state. Noalloc and untagged in native code. *)

external mix_int32 : (state[@unboxed]) -> (int32[@unboxed]) -> (state[@unboxed])
  = "caml_cc_xxhash_mix_int32_byte" "caml_cc_xxhash_mix_int32"
[@@noalloc]
(** Mix an [int32] value into the state. Noalloc and unboxed in native code. *)

val mix_bool : state -> bool -> state
(** Mix a [bool] into the state. *)

val mix_char : state -> char -> state
(** Mix a [char] into the state. *)

val mix_float : state -> float -> state
(** Mix a [float] into the state via [Int64.bits_of_float]. *)

val mix_string : state -> string -> state
(** Mix a [string] into the state using XXH64. *)

external finalize : (state[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_finalize_byte" "caml_cc_xxhash_finalize"
[@@noalloc]
(** Finalise the accumulated state into a 64-bit hash. Noalloc and unboxed in
    native code. *)

val hash_string : ?seed:state -> string -> int64
(** [hash_string ?seed s] is [finalize (mix_string seed s)]. *)

val hash_int64 : ?seed:state -> int64 -> int64
(** [hash_int64 ?seed v] is [finalize (mix_int64 seed v)]. *)

val hash_int : ?seed:state -> int -> int64
(** [hash_int ?seed v] is [finalize (mix_int seed v)]. *)

val hash_int32 : ?seed:state -> int32 -> int64
(** [hash_int32 ?seed v] is [finalize (mix_int32 seed v)]. *)

val hash_bool : ?seed:state -> bool -> int64
(** [hash_bool ?seed b] is [finalize (mix_bool seed b)]. *)

val hash_char : ?seed:state -> char -> int64
(** [hash_char ?seed c] is [finalize (mix_char seed c)]. *)

val hash_float : ?seed:state -> float -> int64
(** [hash_float ?seed f] is [finalize (mix_float seed f)]. *)
