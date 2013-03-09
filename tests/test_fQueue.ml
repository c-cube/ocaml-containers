
open OUnit

let test_empty () =
  let q = FQueue.empty in
  OUnit.assert_bool "is_empty" (FQueue.is_empty q)

let test_push () =
  let q = List.fold_left FQueue.push FQueue.empty [1;2;3;4;5] in
  let q = FQueue.junk q in
  let q = List.fold_left FQueue.push q [6;7;8] in
  let l = Sequence.to_list (FQueue.to_seq q) in
  OUnit.assert_equal [2;3;4;5;6;7;8] l

let test_pop () =
  let q = FQueue.of_seq (Sequence.of_list [1;2;3;4]) in
  let x, q = FQueue.pop q in
  OUnit.assert_equal 1 x;
  let q = List.fold_left FQueue.push q [5;6;7] in
  OUnit.assert_equal 2 (FQueue.peek q);
  let x, q = FQueue.pop q in
  OUnit.assert_equal 2 x;
  ()

let test_append () =
  let q1 = FQueue.of_seq (Sequence.of_list [1;2;3;4]) in 
  let q2 = FQueue.of_seq (Sequence.of_list [5;6;7;8]) in 
  let q = FQueue.append q1 q2 in
  let l = Sequence.to_list (FQueue.to_seq q) in
  OUnit.assert_equal [1;2;3;4;5;6;7;8] l

let test_fold () =
  let q = FQueue.of_seq (Sequence.of_list [1;2;3;4]) in
  let n = FQueue.fold (+) 0 q in
  OUnit.assert_equal 10 n;
  ()

let suite =
  "test_pQueue" >:::
    [ "test_empty" >:: test_empty;
      "test_push" >:: test_push;
      "test_pop" >:: test_pop;
      "test_fold" >:: test_fold;
    ]
