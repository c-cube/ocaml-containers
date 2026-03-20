/* This file is free software, part of containers. See file "license" for more details. */

/* Hash implementation: xorshift+multiply combiner with fmix64 finalizer.
   Combiner: state ^= chunk; state ^= state >> 32; state *= 0xd6e8feb86659fd93
   Finalizer (fmix64, Murmur3): three rounds of xorshift-multiply.

   Multiplicative constant 0xd6e8feb86659fd93 (rrmxmx family, Pelle Evensen, 2018):
     https://mostlymangling.blogspot.com/2018/07/on-mixing-functions-in-fast-hashing.html
   Also evaluated in Chris Wellons' hash-prospector:
     https://github.com/skeeto/hash-prospector

   fmix64 constants 0xff51afd7ed558ccd / 0xc4ceb9fe1a85ec53 (Murmur3, Austin Appleby):
     https://github.com/aappleby/smhasher
*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>
#include <string.h>

// from murmur2: https://chromium.googlesource.com/external/smhasher/+/c8e8bf81bc6041d6d836365a501a0a96830d2d81/MurmurHash2.cpp
#define HASH_MUL UINT64_C(0xc6a4a7935bd1e995)

// from murmur3: https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp#L81
#define FMIX_C1  UINT64_C(0xff51afd7ed558ccd)
#define FMIX_C2  UINT64_C(0xc4ceb9fe1a85ec53)

static inline uint64_t hash_combine(uint64_t state, uint64_t chunk)
{
  state ^= chunk;
  state ^= state >> 32;
  state *= HASH_MUL;
  return state;
}

// fmix64 from murmur3
static inline uint64_t fmix64(uint64_t h)
{
  h ^= h >> 33;
  h *= FMIX_C1;
  h ^= h >> 33;
  h *= FMIX_C2;
  h ^= h >> 33;
  return h;
}

/* --- combine_i64 --------------------------------------------------------- */

CAMLprim int64_t caml_cc_hash_combine_i64(int64_t state, int64_t chunk)
{
  return (int64_t)hash_combine((uint64_t)state, (uint64_t)chunk);
}

CAMLprim value caml_cc_hash_combine_i64_byte(value v_state, value v_chunk)
{
  CAMLparam2(v_state, v_chunk);
  uint64_t r = hash_combine((uint64_t)Int64_val(v_state),
                            (uint64_t)Int64_val(v_chunk));
  CAMLreturn(caml_copy_int64((int64_t)r));
}

/* --- combine_i32 --------------------------------------------------------- */

CAMLprim int64_t caml_cc_hash_combine_i32(int64_t state, int32_t chunk)
{
  return (int64_t)hash_combine((uint64_t)state, (uint64_t)(uint32_t)chunk);
}

CAMLprim value caml_cc_hash_combine_i32_byte(value v_state, value v_chunk)
{
  CAMLparam2(v_state, v_chunk);
  uint64_t r = hash_combine((uint64_t)Int64_val(v_state),
                            (uint64_t)(uint32_t)Int32_val(v_chunk));
  CAMLreturn(caml_copy_int64((int64_t)r));
}

/* --- combine_int --------------------------------------------------------- */

/* chunk is an OCaml int (intnat), passed untagged */
CAMLprim int64_t caml_cc_hash_combine_int(int64_t state, intnat chunk)
{
  return (int64_t)hash_combine((uint64_t)state, (uint64_t)chunk);
}

CAMLprim value caml_cc_hash_combine_int_byte(value v_state, value v_chunk)
{
  CAMLparam2(v_state, v_chunk);
  uint64_t r = hash_combine((uint64_t)Int64_val(v_state),
                            (uint64_t)Long_val(v_chunk));
  CAMLreturn(caml_copy_int64((int64_t)r));
}

/* --- combine_char -------------------------------------------------------- */

/* c is passed as untagged int (Char.code) */
CAMLprim int64_t caml_cc_hash_combine_char(int64_t state, intnat c)
{
  return (int64_t)hash_combine((uint64_t)state, (uint64_t)(unsigned char)c);
}

CAMLprim value caml_cc_hash_combine_char_byte(value v_state, value v_c)
{
  CAMLparam2(v_state, v_c);
  uint64_t r = hash_combine((uint64_t)Int64_val(v_state),
                            (uint64_t)(unsigned char)Long_val(v_c));
  CAMLreturn(caml_copy_int64((int64_t)r));
}

/* --- combine_string ------------------------------------------------------ */

/* Hashes all bytes of [str] into [state] using 8-byte chunks where possible.
   [str] is a regular OCaml value; [state] is unboxed int64. */
CAMLprim int64_t caml_cc_hash_combine_string(int64_t state, value str)
{
  const char *data = String_val(str);
  mlsize_t    len  = caml_string_length(str);
  uint64_t    s    = (uint64_t)state;
  mlsize_t    i    = 0;

  for (; i + 8 <= len; i += 8) {
    uint64_t chunk;
    memcpy(&chunk, data + i, 8);
    s = hash_combine(s, chunk);
  }
  if (i < len) {
    uint64_t chunk = 0;
    memcpy(&chunk, data + i, len - i);
    s = hash_combine(s, chunk);
  }
  return (int64_t)s;
}

CAMLprim value caml_cc_hash_combine_string_byte(value v_state, value str)
{
  CAMLparam2(v_state, str);
  int64_t r = caml_cc_hash_combine_string(Int64_val(v_state), str);
  CAMLreturn(caml_copy_int64(r));
}

/* --- fmix64 -------------------------------------------------------------- */

/* Returns full 64-bit fmix64 result; may be "negative" as signed int64. */
CAMLprim int64_t caml_cc_hash_fmix64(int64_t state)
{
  return (int64_t)fmix64((uint64_t)state);
}

CAMLprim value caml_cc_hash_fmix64_byte(value v_state)
{
  CAMLparam1(v_state);
  CAMLreturn(caml_copy_int64((int64_t)fmix64((uint64_t)Int64_val(v_state))));
}

/* --- finalize ------------------------------------------------------------ */

/* Applies fmix64 and masks to Max_long (positive OCaml int). */
CAMLprim intnat caml_cc_hash_finalize(int64_t state)
{
  return (intnat)(fmix64((uint64_t)state) & (uint64_t)Max_long);
}

CAMLprim value caml_cc_hash_finalize_byte(value v_state)
{
  CAMLparam1(v_state);
  intnat r = (intnat)(fmix64((uint64_t)Int64_val(v_state)) & (uint64_t)Max_long);
  CAMLreturn(Val_long(r));
}
