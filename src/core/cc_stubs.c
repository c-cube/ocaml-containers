#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <stdint.h>

/* FNV-1a hash for a 64-bit integer.
   https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function */

static inline int64_t cc_fnv_hash_int64(int64_t n) {
  uint64_t un = (uint64_t)n;
  uint64_t h = UINT64_C(0xcbf29ce484222325);
  const uint64_t prime = UINT64_C(0x100000001b3);
  for (int k = 0; k < 8; k++) {
    h ^= (un >> (k * 8)) & 0xff;
    h *= prime;
  }
  return (int64_t)h;
}

/* Mask to the OCaml int range (63 bits on 64-bit, 31 on 32-bit)
   before hashing, so negative OCaml ints hash the same as
   the unsigned representation seen by OCaml's [lsr]. */
#define OCAML_INT_MASK ((UINT64_C(1) << (8 * sizeof(value) - 1)) - 1)

/* native: untagged int in, untagged int out */
CAMLprim intnat caml_cc_hash_int(intnat n) {
  int64_t projected = (int64_t)((uint64_t)n & OCAML_INT_MASK);
  return (intnat)((uint64_t)cc_fnv_hash_int64(projected) & Max_long);
}

/* bytecode: boxed value in, boxed value out */
CAMLprim value caml_cc_hash_int_byte(value v_n) {
  return Val_long(caml_cc_hash_int(Long_val(v_n)));
}

/* native: unboxed int64 in, untagged int out */
CAMLprim intnat caml_cc_hash_int64(int64_t n) {
  return (intnat)((uint64_t)cc_fnv_hash_int64(n) & Max_long);
}

/* bytecode: boxed int64 value in, boxed value out */
CAMLprim value caml_cc_hash_int64_byte(value v_n) {
  return Val_long(caml_cc_hash_int64(Int64_val(v_n)));
}

/* native: unboxed int64 in, unboxed int64 out.
   Masks to non-negative int64 (matches OCaml's Int64.max_int). */
CAMLprim int64_t caml_cc_hash_int64_to_int64(int64_t n) {
  return cc_fnv_hash_int64(n) & INT64_MAX;
}

/* bytecode: boxed int64 in, boxed int64 out */
CAMLprim value caml_cc_hash_int64_to_int64_byte(value v_n) {
  return caml_copy_int64(cc_fnv_hash_int64(Int64_val(v_n)) & INT64_MAX);
}
