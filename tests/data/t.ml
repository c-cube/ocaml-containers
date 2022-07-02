
Containers_testlib.run_all ~descr:"containers-data" [
  T_bv.Test.get();
  T_bijection.Test.get();
  T_bitfield.Test.get();
  T_cache.Test.get();
  T_deque.Test.get();
];;
