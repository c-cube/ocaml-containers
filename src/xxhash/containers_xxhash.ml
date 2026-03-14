module Raw = struct
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
end

let[@inline] hash_string s = Raw.hash_string s 0L
let[@inline] hash_string_seed s seed = Raw.hash_string s seed
let[@inline] hash_int64 v = Raw.hash_int64 v 0L
let[@inline] hash_int64_seed v seed = Raw.hash_int64 v seed
let[@inline] hash_int32 v = Raw.hash_int32 v 0L
let[@inline] hash_int32_seed v seed = Raw.hash_int32 v seed
let[@inline] hash_int v = Raw.hash_int v 0L
let[@inline] hash_int_seed v seed = Raw.hash_int v seed
let[@inline] hash_bool b = Raw.hash_int (Bool.to_int b) 0L
let[@inline] hash_bool_seed b seed = Raw.hash_int (Bool.to_int b) seed
let[@inline] hash_char c = Raw.hash_int (Char.code c) 0L
let[@inline] hash_char_seed c seed = Raw.hash_int (Char.code c) seed
