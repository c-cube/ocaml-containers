open OUnit

let suite =
  "all_tests" >:::
    [ Test_pHashtbl.suite;
      Test_puf.suite;
      Test_univ.suite;
      Test_RoseTree.suite;
    ]

let () =
  ignore (run_test_tt_main suite);
  ()
