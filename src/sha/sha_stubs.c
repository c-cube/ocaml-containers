
#include <caml/mlvalues.h>
#include "sha.h"

CAMLprim value caml_cc_sha256_add(value ctx, value bytes, value off, value len)
{
  CAMLparam4 (ctx, bytes, off, len);

  CAMLreturn (Val_unit);
}

