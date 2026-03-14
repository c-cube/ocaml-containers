(** XXHash bindings.

    Fast non-cryptographic hash functions from
    {{:https://github.com/Cyan4973/xxHash} xxHash}.

    All functions use XXH64 and are noalloc in native code.
*)

external hash_string : string -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_string_byte" "caml_cc_xxhash_string"
[@@noalloc]
(** [hash_string s seed] hashes [s] with [seed] using XXH64. *)

external hash_int64 :
  (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int64_byte" "caml_cc_xxhash_int64"
[@@noalloc]
(** [hash_int64 v seed] hashes the 8-byte representation of [v] with [seed]. *)

external hash_int32 :
  (int32[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int32_byte" "caml_cc_xxhash_int32"
[@@noalloc]
(** [hash_int32 v seed] hashes the 4-byte representation of [v] with [seed]. *)

external hash_int : (int[@untagged]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int_byte" "caml_cc_xxhash_int"
[@@noalloc]
(** [hash_int v seed] hashes [v] as a 64-bit integer with [seed].
    Noalloc and untagged in native code. *)

val hash_bool : bool -> int64 -> int64
(** [hash_bool b seed] hashes [b] as an integer (0 or 1) with [seed]. *)

val hash_char : char -> int64 -> int64
(** [hash_char c seed] hashes [c] as its character code with [seed]. *)
