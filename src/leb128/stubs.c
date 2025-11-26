
// readapted from ocaml-protoc, original code also from c-cube

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

static inline int ix_leb128_varint_size(uint64_t i) {
/* generated with:
for i in range(1,10):
  ceiling = (1 << (i*7))-1
  print(f'if (i <= {ceiling}L) return {i};')
*/

  if (i <= 127L) return 1;
  if (i <= 16383L) return 2;
  if (i <= 2097151L) return 3;
  if (i <= 268435455L) return 4;
  if (i <= 34359738367L) return 5;
  if (i <= 4398046511103L) return 6;
  if (i <= 562949953421311L) return 7;
  if (i <= 72057594037927935L) return 8;
  if (i <= 9223372036854775807L) return 9;
  return 10;
}

// number of bytes for i
CAMLprim value caml_cc_leb128_varint_size(int64_t i) {
  int res = ix_leb128_varint_size(i);
  return Val_int(res);
}

// boxed version, for bytecode
CAMLprim value caml_cc_leb128_varint_size_byte(value v_i) {
  CAMLparam1(v_i);

  int64_t i = Int64_val(v_i);
  int res = ix_leb128_varint_size(i);
  CAMLreturn(Val_int(res));
}

// write i at str[idx…] in varint
static inline void ix_leb128_varint(unsigned char *str, uint64_t i) {
  while (true) {
    uint64_t cur = i & 0x7f;
    if (cur == i) {
      *str = (unsigned char)cur;
      break;
    } else {
      *str = (unsigned char)(cur | 0x80);
      i = i >> 7;
      ++str;
    }
  }
}

// write `i` starting at `idx`
CAMLprim value caml_cc_leb128_varint(value _str, intnat idx, int64_t i) {
  unsigned char *str = Bytes_val(_str);
  ix_leb128_varint(str + idx, i);
  return Val_unit;
}

CAMLprim value caml_cc_leb128_varint_byte(value _str, value _idx, value _i) {
  CAMLparam3(_str, _idx, _i);
  unsigned char *str = Bytes_val(_str);
  int idx = Int_val(_idx);
  int64_t i = Int64_val(_i);
  ix_leb128_varint(str + idx, i);
  CAMLreturn(Val_unit);
}
