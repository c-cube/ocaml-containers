open OUnit

(* TODO more tests *)

let suite =
  "all_tests" >:::
    [ Test_pHashtbl.suite;
      Test_deque.suite;
      Test_fHashtbl.suite;
      Test_fQueue.suite;
      Test_flatHashtbl.suite;
      Test_heap.suite;
      Test_graph.suite;
      Test_univ.suite;
    ]

let _ =
  run_test_tt_main suite
