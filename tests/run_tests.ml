open OUnit

(* TODO more tests *)

let suite =
  "all_tests" >:::
    [ Test_pHashtbl.suite;
      Test_PersistentHashtbl.suite;
      Test_bv.suite;
      Test_PiCalculus.suite;
      Test_splayMap.suite;
      Test_CCHeap.suite;
      Test_cc.suite;
      Test_puf.suite;
      Test_vector.suite;
      Test_gen.suite;
      Test_deque.suite;
      Test_fHashtbl.suite;
      Test_fQueue.suite;
      Test_flatHashtbl.suite;
      Test_heap.suite;
      Test_graph.suite;
      Test_univ.suite;
    ]

let props =
  QCheck.flatten
    [ Test_PersistentHashtbl.props
    ; Test_bv.props
    ; Test_vector.props
    ]

let _ =
  ignore (QCheck.run_tests props);
  ignore (run_test_tt_main suite);
  ()
