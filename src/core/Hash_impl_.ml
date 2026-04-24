(* This file is free software, part of containers. See file "license" for more details. *)

(** Internal hash implementation.

    Combiner: [state ^= chunk; state ^= state >> 32; state *= 0xc6a4a7935bd1e995]
    Finalizer: fmix64 (Murmur3).

    Multiplicative constant 0xc6a4a7935bd1e995 (MurmurHash2, Austin Appleby):
    https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp

    fmix64 constants (Murmur3, Austin Appleby):
    https://github.com/aappleby/smhasher

    Not part of the public API; use {!CCHash} instead. *)

(** Initial hash state (golden-ratio constant). *)
let seed : int64 = 0x9e3779b97f4a7c15L

external combine_int :
  (int64[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "caml_cc_hash_combine_int_byte" "caml_cc_hash_combine_int"
[@@noalloc]
(** [combine_int state x] mixes OCaml int [x] into [state]. *)

external combine_i64 :
  (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_hash_combine_i64_byte" "caml_cc_hash_combine_i64"
[@@noalloc]
(** [combine_i64 state chunk] mixes [chunk] into [state]. *)

external combine_i32 :
  (int64[@unboxed]) -> (int32[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_hash_combine_i32_byte" "caml_cc_hash_combine_i32"
[@@noalloc]
(** [combine_i32 state chunk] mixes [chunk] into [state]. *)

external combine_char :
  (int64[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "caml_cc_hash_combine_char_byte" "caml_cc_hash_combine_char"
[@@noalloc]
(** [combine_char state c] mixes character code [c] into [state]. *)

external combine_string : (int64[@unboxed]) -> string -> (int64[@unboxed])
  = "caml_cc_hash_combine_string_byte" "caml_cc_hash_combine_string"
[@@noalloc]
(** [combine_string state s] mixes all bytes of [s] into [state] in 8-byte chunks. *)

external fmix64 : (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_hash_fmix64_byte" "caml_cc_hash_fmix64"
[@@noalloc]
(** [fmix64 state] applies the Murmur3 finalizer. Result may be negative. *)

external finalize : (int64[@unboxed]) -> (int[@untagged])
  = "caml_cc_hash_finalize_byte" "caml_cc_hash_finalize"
[@@noalloc]
(** [finalize state] applies fmix64 and returns a non-negative [int]. *)
