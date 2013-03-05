open OUnit

(* TODO more tests *)

let suite =
  "all_tests" >:::
    [ Test_pHashtbl.suite;
      Test_heap.suite;
      Test_graph.suite;
    ]

let _ =
  run_test_tt_main suite
