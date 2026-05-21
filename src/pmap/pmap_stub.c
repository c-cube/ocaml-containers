/* This file is free software, part of containers. See file "license" for more details. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/* Popcount for 32-bit integer (used by pmap's sparse array).
   We only need to count bits in a 16-bit bitmap, but we operate
   on the full int for generality. */
static inline intnat pmap_popcount(intnat v)
{
  uint32_t x = (uint32_t)v;
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F;
  x = x + (x >> 8);
  x = x + (x >> 16);
  return (intnat)(x & 0x7F);
}

/* Native: unboxed int -> int */
CAMLprim intnat caml_cc_pmap_popcount(intnat v)
{
  return pmap_popcount(v);
}

/* Bytecode: value -> value */
CAMLprim value caml_cc_pmap_popcount_byte(value v)
{
  return Val_long(pmap_popcount(Long_val(v)));
}
