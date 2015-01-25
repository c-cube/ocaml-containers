open OUnit

let suite =
  "all_tests" >:::
    [ Test_pHashtbl.suite;
      Test_PersistentHashtbl.suite;
      Test_bv.suite;
      Test_CCHeap.suite;
      Test_puf.suite;
      Test_vector.suite;
      Test_deque.suite;
      Test_fQueue.suite;
      Test_univ.suite;
      Test_mixtbl.suite;
      Test_RoseTree.suite;
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
