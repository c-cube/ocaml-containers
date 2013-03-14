
open OUnit

open Sequence.Infix

let plist l = Utils.sprintf "%a" (Sequence.pp_seq Format.pp_print_int) (Sequence.of_list l)
let pint i = string_of_int i

let test_length () =
  let d = Deque.of_seq (1 -- 10) in
  OUnit.assert_equal ~printer:pint 10 (Deque.length d)

let test_front () =
  let d = Deque.of_seq (1 -- 10) in
  let printer = pint in
  OUnit.assert_equal ~printer 1 (Deque.peek_front d); 
  Deque.push_front d 42;
  OUnit.assert_equal ~printer 42 (Deque.peek_front d);
  OUnit.assert_equal ~printer 42 (Deque.take_front d);
  OUnit.assert_equal ~printer 1 (Deque.take_front d);
  OUnit.assert_equal ~printer 2 (Deque.take_front d);
  OUnit.assert_equal ~printer 3 (Deque.take_front d);
  OUnit.assert_equal ~printer 10 (Deque.peek_back d);
  ()

let test_back () =
  let d = Deque.of_seq (1 -- 10) in
  let printer = pint in
  OUnit.assert_equal ~printer 1 (Deque.peek_front d); 
  Deque.push_back d 42;
  OUnit.assert_equal ~printer 42 (Deque.peek_back d);
  OUnit.assert_equal ~printer 42 (Deque.take_back d);
  OUnit.assert_equal ~printer 10 (Deque.take_back d);
  OUnit.assert_equal ~printer 9 (Deque.take_back d);
  OUnit.assert_equal ~printer 8 (Deque.take_back d);
  OUnit.assert_equal ~printer 1 (Deque.peek_front d);
  ()

let test_iter () =
  let d = Deque.of_seq (1 -- 5) in
  let s = Sequence.from_iter (fun k -> Deque.iter k d) in
  let l = Sequence.to_list s in
  OUnit.assert_equal ~printer:plist [1;2;3;4;5] l;
  ()

let suite =
  "test_deque" >:::
    [ "test_length" >:: test_length;
      "test_front" >:: test_front;
      "test_back" >:: test_back;
      "test_iter" >:: test_iter;
    ]
