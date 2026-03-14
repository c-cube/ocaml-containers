#define XXH_NO_XXH3
#define XXH_NO_STREAM
#define XXH_INLINE_ALL
#include "xxhash.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

/* hash_string: native signature: (value, int64_t) -> int64_t
   string is passed as OCaml value (can't be unboxed), seed is unboxed int64 */
CAMLprim int64_t caml_cc_xxhash_string(value v_s, int64_t seed) {
  const char *s = String_val(v_s);
  size_t len = caml_string_length(v_s);
  return (int64_t)XXH64(s, len, (XXH64_hash_t)seed);
}

CAMLprim value caml_cc_xxhash_string_byte(value v_s, value v_seed) {
  CAMLparam2(v_s, v_seed);
  int64_t seed = Int64_val(v_seed);
  const char *s = String_val(v_s);
  size_t len = caml_string_length(v_s);
  int64_t result = (int64_t)XXH64(s, len, (XXH64_hash_t)seed);
  CAMLreturn(caml_copy_int64(result));
}

/* hash_int64: unboxed (int64_t, int64_t) -> int64_t */
CAMLprim int64_t caml_cc_xxhash_int64(int64_t v, int64_t seed) {
  return (int64_t)XXH64(&v, sizeof(v), (XXH64_hash_t)seed);
}

CAMLprim value caml_cc_xxhash_int64_byte(value v_v, value v_seed) {
  CAMLparam2(v_v, v_seed);
  int64_t v = Int64_val(v_v);
  int64_t seed = Int64_val(v_seed);
  int64_t result = caml_cc_xxhash_int64(v, seed);
  CAMLreturn(caml_copy_int64(result));
}

/* hash_int: untagged (intnat, intnat) -> intnat */
CAMLprim intnat caml_cc_xxhash_int(intnat v, intnat seed) {
  int64_t v64 = (int64_t)v;
  int64_t seed64 = (int64_t)seed;
  return (intnat)caml_cc_xxhash_int64(v64, seed64);
}

CAMLprim value caml_cc_xxhash_int_byte(value v_v, value v_seed) {
  intnat v = Long_val(v_v);
  intnat seed = Long_val(v_seed);
  return Val_long(caml_cc_xxhash_int(v, seed));
}

/* mix64: unboxed (int64_t, int64_t) -> int64_t  [uses XXH64] */
CAMLprim int64_t caml_cc_xxhash_mix64(int64_t a, int64_t b) {
  return (int64_t)XXH64(&a, sizeof(a), (XXH64_hash_t)b);
}

CAMLprim value caml_cc_xxhash_mix64_byte(value v_a, value v_b) {
  CAMLparam2(v_a, v_b);
  int64_t a = Int64_val(v_a);
  int64_t b = Int64_val(v_b);
  CAMLreturn(caml_copy_int64(caml_cc_xxhash_mix64(a, b)));
}

/* finalize64: unboxed int64_t -> int64_t  [uses XXH64 with seed=0] */
CAMLprim int64_t caml_cc_xxhash_finalize64(int64_t h) {
  return (int64_t)XXH64(&h, sizeof(h), 0);
}

CAMLprim value caml_cc_xxhash_finalize64_byte(value v_h) {
  CAMLparam1(v_h);
  int64_t h = Int64_val(v_h);
  CAMLreturn(caml_copy_int64(caml_cc_xxhash_finalize64(h)));
}
