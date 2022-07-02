

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
  T_hashtbl.get();
  T_heap.get();
  T_IO.get();
  T_int.get();
  T_int32.get();
  T_int64.get();
  T_map.get();
  T_nativeint.get();
  T_option.get();
  T_ord.get();
  T_parse.get();
  T_random.get();
  T_result.get();
  T_set.get();
  T_seq.get();
  T_sexp.get();
  T_string.get();
  T_utf8string.get();
  T_vector.get();
  T_bencode.get();
  T_unix.get();
];;
