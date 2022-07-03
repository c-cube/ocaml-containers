
Containers_testlib.run_all ~descr:"containers-data" [
  T_bv.Test.get();
  T_bijection.Test.get();
  T_bitfield.Test.get();
  T_cache.Test.get();
  T_deque.Test.get();
  T_fqueue.Test.get();
  T_fun_vec.Test.get();
  T_graph.Test.get();
  T_hashset.Test.get();
  T_hashtrie.Test.get();
  T_het.Test.get();
  T_immutarray.Test.get();
  T_intmap.Test.get();
  T_lazylist.Test.get();
  T_misc.Test.get();
  T_mutheap.Test.get();
  T_persistenthashtbl.Test.get();
  T_ral.Test.get();
  T_ringbuffer.Test.get();
  T_simplequeue.Test.get();
  T_trie.Test.get();
  T_wbt.Test.get();
  T_zipper.Test.get();
];;
