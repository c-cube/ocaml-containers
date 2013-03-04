
open OUnit

let test_add () =
  let h = PHashtbl.create 5 in
  PHashtbl.replace h 42 "foo";
  OUnit.assert_equal (PHashtbl.find h 42) "foo"

let suite =
  "test_pHashtbl" >:::
    [ "test_add" >:: test_add;
    ]
