(** XXHash bindings.

    Fast non-cryptographic hash functions from
    {{:https://github.com/Cyan4973/xxHash} xxHash}.

    All functions use XXH64 and are noalloc in native code.
*)

(** Raw bindings with explicit seed argument. *)
module Raw : sig
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
  (** [hash_int v seed] hashes [v] as a 64-bit integer with [seed]. Noalloc
      and untagged in native code. *)
end

val hash_string : string -> int64
(** [hash_string s] hashes [s] using XXH64 with seed [0L]. *)

val hash_string_seed : string -> int64 -> int64
(** [hash_string_seed s seed] hashes [s] with an explicit seed. *)

val hash_int64 : int64 -> int64
(** [hash_int64 v] hashes the 8-byte representation of [v] with seed [0L]. *)

val hash_int64_seed : int64 -> int64 -> int64
(** [hash_int64_seed v seed] hashes [v] with an explicit seed. *)

val hash_int32 : int32 -> int64
(** [hash_int32 v] hashes the 4-byte representation of [v] with seed [0L]. *)

val hash_int32_seed : int32 -> int64 -> int64
(** [hash_int32_seed v seed] hashes [v] with an explicit seed. *)

val hash_int : int -> int64
(** [hash_int v] hashes [v] as a 64-bit integer with seed [0L]. *)

val hash_int_seed : int -> int64 -> int64
(** [hash_int_seed v seed] hashes [v] with an explicit seed. *)

val hash_bool : bool -> int64
(** [hash_bool b] hashes [b] as an integer (0 or 1) with seed [0L]. *)

val hash_bool_seed : bool -> int64 -> int64
(** [hash_bool_seed b seed] hashes [b] with an explicit seed. *)

val hash_char : char -> int64
(** [hash_char c] hashes [c] as its character code with seed [0L]. *)

val hash_char_seed : char -> int64 -> int64
(** [hash_char_seed c seed] hashes [c] with an explicit seed. *)
