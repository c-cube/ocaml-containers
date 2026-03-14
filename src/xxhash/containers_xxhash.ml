external hash_string : string -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_string_byte" "caml_cc_xxhash_string"
[@@noalloc]

external hash_int64 :
  (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int64_byte" "caml_cc_xxhash_int64"
[@@noalloc]

external hash_int32 :
  (int32[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int32_byte" "caml_cc_xxhash_int32"
[@@noalloc]

external hash_int : (int[@untagged]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_int_byte" "caml_cc_xxhash_int"
[@@noalloc]

let[@inline] hash_bool b seed = hash_int (Bool.to_int b) seed
let[@inline] hash_char c seed = hash_int (Char.code c) seed
