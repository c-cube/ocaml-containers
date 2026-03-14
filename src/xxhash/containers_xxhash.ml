type state = int64

let seed : state = 0L

external mix_int64 : (state[@unboxed]) -> (int64[@unboxed]) -> (state[@unboxed])
  = "caml_cc_xxhash_mix_int64_byte" "caml_cc_xxhash_mix_int64"
[@@noalloc]

external mix_int : (state[@unboxed]) -> (int[@untagged]) -> (state[@unboxed])
  = "caml_cc_xxhash_mix_int_byte" "caml_cc_xxhash_mix_int"
[@@noalloc]

external mix_int32 : (state[@unboxed]) -> (int32[@unboxed]) -> (state[@unboxed])
  = "caml_cc_xxhash_mix_int32_byte" "caml_cc_xxhash_mix_int32"
[@@noalloc]

let[@inline] mix_bool h b = mix_int h (Bool.to_int b)
let[@inline] mix_char h c = mix_int h (Char.code c)
let[@inline] mix_float h f = mix_int64 h (Int64.bits_of_float f)

external mix_string_aux : (state[@unboxed]) -> string -> (state[@unboxed])
  = "caml_cc_xxhash_mix_string_byte" "caml_cc_xxhash_mix_string"
[@@noalloc]

let[@inline] mix_string h s = mix_string_aux h s

external finalize : (state[@unboxed]) -> (int64[@unboxed])
  = "caml_cc_xxhash_finalize_byte" "caml_cc_xxhash_finalize"
[@@noalloc]

let[@inline] hash_string ?(seed = seed) s = finalize (mix_string seed s)
let[@inline] hash_int64 ?(seed = seed) v = finalize (mix_int64 seed v)
let[@inline] hash_int ?(seed = seed) v = finalize (mix_int seed v)
let[@inline] hash_int32 ?(seed = seed) v = finalize (mix_int32 seed v)
let[@inline] hash_bool ?(seed = seed) b = finalize (mix_bool seed b)
let[@inline] hash_char ?(seed = seed) c = finalize (mix_char seed c)
let[@inline] hash_float ?(seed = seed) f = finalize (mix_float seed f)
