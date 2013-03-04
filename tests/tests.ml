open OUnit

(* TODO more tests *)

let suite =
  "all_tests" >:::
    [ Test_pHashtbl.suite;
    ]

let _ =
  run_test_tt_main suite
