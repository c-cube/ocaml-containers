#define XXH_NO_XXH3
#define XXH_NO_STREAM
#define XXH_INLINE_ALL
#include "xxhash.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

/* mix_int64: (int64_t state, int64_t value) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_mix_int64(int64_t state, int64_t value) {
  return (int64_t)XXH64(&value, sizeof(value), (XXH64_hash_t)state);
}
CAMLprim value caml_cc_xxhash_mix_int64_byte(value v_state, value v_value) {
  CAMLparam2(v_state, v_value);
  int64_t result = caml_cc_xxhash_mix_int64(Int64_val(v_state), Int64_val(v_value));
  CAMLreturn(caml_copy_int64(result));
}

/* mix_int: (int64_t state, intnat value) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_mix_int(int64_t state, intnat value) {
  int64_t v = (int64_t)(uintnat)value; /* zero-extend on 32-bit platforms */
  return (int64_t)XXH64(&v, sizeof(v), (XXH64_hash_t)state);
}
CAMLprim value caml_cc_xxhash_mix_int_byte(value v_state, value v_value) {
  CAMLparam2(v_state, v_value);
  int64_t result = caml_cc_xxhash_mix_int(Int64_val(v_state), Long_val(v_value));
  CAMLreturn(caml_copy_int64(result));
}

/* mix_int32: (int64_t state, int32_t value) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_mix_int32(int64_t state, int32_t value) {
  int64_t v = (int64_t)value;
  return (int64_t)XXH64(&v, sizeof(v), (XXH64_hash_t)state);
}
CAMLprim value caml_cc_xxhash_mix_int32_byte(value v_state, value v_value) {
  CAMLparam2(v_state, v_value);
  int64_t result = caml_cc_xxhash_mix_int32(Int64_val(v_state), Int32_val(v_value));
  CAMLreturn(caml_copy_int64(result));
}

/* mix_string: native signature: (int64_t state, value string) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_mix_string(int64_t state, value v_s) {
  const char *s = String_val(v_s);
  size_t len = caml_string_length(v_s);
  return (int64_t)XXH64(s, len, (XXH64_hash_t)state);
}
CAMLprim value caml_cc_xxhash_mix_string_byte(value v_state, value v_s) {
  CAMLparam2(v_state, v_s);
  int64_t result = caml_cc_xxhash_mix_string(Int64_val(v_state), v_s);
  CAMLreturn(caml_copy_int64(result));
}

/* finalize: int64_t state -> int64_t */
CAMLprim int64_t caml_cc_xxhash_finalize(int64_t state) {
  return (int64_t)XXH64(&state, sizeof(state), 0);
}
CAMLprim value caml_cc_xxhash_finalize_byte(value v_state) {
  CAMLparam1(v_state);
  CAMLreturn(caml_copy_int64(caml_cc_xxhash_finalize(Int64_val(v_state))));
}
