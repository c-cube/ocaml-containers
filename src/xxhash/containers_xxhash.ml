external hash_string_aux : string -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_string_byte" "caml_cc_xxhash_string"
[@@noalloc]

let[@inline] hash_string ?(seed = 0L) s = hash_string_aux s seed

external hash_int64 :
  (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int64_byte" "caml_cc_xxhash_int64"
[@@noalloc]

external hash_int : (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "caml_cc_xxhash_int_byte" "caml_cc_xxhash_int"
[@@noalloc]

external mix64 : (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_mix64_byte" "caml_cc_xxhash_mix64"
[@@noalloc]

external finalize64 : (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_finalize64_byte" "caml_cc_xxhash_finalize64"
[@@noalloc]
