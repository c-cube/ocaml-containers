
open OUnit

module Sequence = CCSequence

let test1 () =
  let empty = SplayMap.empty () in
  let m = SplayMap.of_seq empty (Sequence.of_list [1, "1"; 2, "2"; 3, "3"]) in
  OUnit.assert_equal ~printer:(fun s -> s) "2" (SplayMap.find m 2);
  OUnit.assert_equal ~printer:(fun s -> s) "2" (SplayMap.find m 2);
  OUnit.assert_equal ~printer:(fun s -> s) "3" (SplayMap.find m 3);
  OUnit.assert_equal ~printer:(fun s -> s) "1" (SplayMap.find m 1);
  OUnit.assert_raises Not_found (fun () -> SplayMap.find m 4);
  ()

let test_remove () =
  let n = 100 in
  let m = SplayMap.of_seq (SplayMap.empty ())
    (Sequence.zip (Sequence.zip_i (Sequence.int_range ~start:0 ~stop:n))) in
  for i = 0 to n do
    OUnit.assert_equal ~printer:string_of_int i (SplayMap.find m i);
  done;
  let m = SplayMap.remove m (n/2) in
  OUnit.assert_equal ~printer:string_of_int n (SplayMap.find m n);
  OUnit.assert_raises Not_found (fun () -> SplayMap.find m (n/2));
  ()

let test_big () =
  let n = 100_000 in
  let m = SplayMap.of_seq (SplayMap.empty ())
    (Sequence.zip (Sequence.zip_i (Sequence.int_range ~start:0 ~stop:n))) in
  for i = 0 to n do
    OUnit.assert_equal ~printer:string_of_int i (SplayMap.find m i);
  done;
  OUnit.assert_equal ~printer:string_of_int (n+1) (SplayMap.size m);
  ()

let suite =
  "test_splayMap" >:::
    [ "test1" >:: test1;
      "test_remove" >:: test_remove;
      "test_big" >:: test_big;
    ]
