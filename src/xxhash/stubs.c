#define XXH_NO_XXH3
#define XXH_NO_STREAM
#define XXH_INLINE_ALL
#include "xxhash.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

/* hash_string: (value string, int64_t seed) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_string(value v_s, int64_t seed) {
  return (int64_t)XXH64(String_val(v_s), caml_string_length(v_s),
                        (XXH64_hash_t)seed);
}
CAMLprim value caml_cc_xxhash_string_byte(value v_s, value v_seed) {
  CAMLparam2(v_s, v_seed);
  int64_t result =
    caml_cc_xxhash_string(v_s, (int64_t)Int64_val(v_seed));
  CAMLreturn(caml_copy_int64(result));
}

/* hash_int64: (int64_t v, int64_t seed) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_int64(int64_t v, int64_t seed) {
  return (int64_t)XXH64(&v, sizeof(v), (XXH64_hash_t)seed);
}
CAMLprim value caml_cc_xxhash_int64_byte(value v_v, value v_seed) {
  CAMLparam2(v_v, v_seed);
  int64_t result = caml_cc_xxhash_int64(Int64_val(v_v), Int64_val(v_seed));
  CAMLreturn(caml_copy_int64(result));
}

/* hash_int32: (int32_t v, int64_t seed) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_int32(int32_t v, int64_t seed) {
  return (int64_t)XXH64(&v, sizeof(v), (XXH64_hash_t)seed);
}
CAMLprim value caml_cc_xxhash_int32_byte(value v_v, value v_seed) {
  CAMLparam2(v_v, v_seed);
  int64_t result = caml_cc_xxhash_int32(Int32_val(v_v), Int64_val(v_seed));
  CAMLreturn(caml_copy_int64(result));
}

/* hash_int: (intnat v, int64_t seed) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_int(intnat v, int64_t seed) {
  int64_t v64 = (int64_t)(uintnat)v; /* zero-extend on 32-bit platforms */
  return (int64_t)XXH64(&v64, sizeof(v64), (XXH64_hash_t)seed);
}
CAMLprim value caml_cc_xxhash_int_byte(value v_v, value v_seed) {
  CAMLparam2(v_v, v_seed);
  int64_t result = caml_cc_xxhash_int(Long_val(v_v), Int64_val(v_seed));
  CAMLreturn(caml_copy_int64(result));
}
