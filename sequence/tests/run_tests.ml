
open OUnit

let suite =
    "run_tests" >:::
      [ Test_sequence.suite; ]

let _ =
  OUnit.run_test_tt_main suite
