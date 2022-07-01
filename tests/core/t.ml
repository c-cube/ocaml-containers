

Containers_testlib.run_all ~descr:"containers" [
  T_list.get();
  T_array.get();
  T_bool.get();
  T_byte_buffer.get();
  T_canonical_sexp.get();
  T_char.get();
  T_either.get();
  T_eq.get();
  T_float.get();
  T_format.get();
  T_fun.get ();
  T_hash.get();
];;
