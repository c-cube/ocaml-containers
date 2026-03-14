(** XXHash bindings.

    Fast non-cryptographic hash functions from
    {{:https://github.com/Cyan4973/xxHash} xxHash}.

    String hashing uses XXH3_64bits (modern, fastest).
    Integer hashing delegates to the string hasher via a stack-allocated buffer.
    The mixer and finalizer use the XXH64 primitive.
*)

val hash_string : ?seed:int64 -> string -> int64
(** [hash_string ?seed s] hashes string [s] with optional [seed] (default [0L])
    using XXH3_64bits_withSeed. *)

external hash_int64 :
  (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int64_byte" "caml_cc_xxhash_int64"
[@@noalloc]
(** [hash_int64 v seed] hashes [v] with [seed] using XXH3_64bits_withSeed.
    Noalloc and unboxed in native code. *)

external hash_int : (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "caml_cc_xxhash_int_byte" "caml_cc_xxhash_int"
[@@noalloc]
(** [hash_int v seed] hashes [v] (an OCaml int) with [seed].
    Noalloc and untagged in native code. *)

external mix64 : (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_mix64_byte" "caml_cc_xxhash_mix64"
[@@noalloc]
(** [mix64 a b] mixes two int64 values using XXH64: [XXH64(&a, 8, b)].
    Suitable for combining hash values. Noalloc and unboxed in native code. *)

external finalize64 : (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_finalize64_byte" "caml_cc_xxhash_finalize64"
[@@noalloc]
(** [finalize64 h] finalizes/avalanches a hash value using XXH64: [XXH64(&h, 8, 0)].
    Noalloc and unboxed in native code. *)
