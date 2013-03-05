(** Test heaps *)

open OUnit
open Helpers

let test_empty () =
  let h = Heap.empty ~cmp:(fun x y -> x - y) in
  OUnit.assert_bool "is_empty empty" (Heap.is_empty h)

let test_sort () =
  let h = Heap.empty ~cmp:(fun x y -> x - y) in
  (* Heap sort *)
  let l = [3;4;2;1;6;5;0;7;10;9;8] in
  Heap.of_seq h (Sequence.of_list l);
  let l' = Sequence.to_list (Heap.to_seq h) in
  OUnit.assert_equal ~printer:print_int_list l' [0;1;2;3;4;5;6;7;8;9;10]

let test_remove () =
  let h = Heap.empty ~cmp:(fun x y -> x - y) in
  let l = [3;4;2;1;6;5;0;7;10;9;8] in
  Heap.of_seq h (Sequence.of_list l);
  (* check pop *)
  OUnit.assert_equal (Heap.pop h) 0;
  OUnit.assert_equal (Heap.pop h) 1;
  OUnit.assert_equal (Heap.pop h) 2;
  OUnit.assert_equal (Heap.pop h) 3;
  (* check that elements have been removed *)
  let l' = Sequence.to_list (Heap.to_seq h) in
  OUnit.assert_equal ~printer:print_int_list l' [4;5;6;7;8;9;10]

let suite =
  "test_heaps" >:::
    [ "test_empty" >:: test_empty;
      "test_sort" >:: test_sort;
      "test_remove" >:: test_remove;
    ]
