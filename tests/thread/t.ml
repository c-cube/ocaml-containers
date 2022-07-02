
Containers_testlib.run_all ~descr:"containers-thread" [
  T_bq.Test.get();
  T_lock.Test.get();
  T_pool.Test.get();
  T_semaphore.Test.get();
  T_thread.Test.get();
  T_timer.Test.get();
];;
