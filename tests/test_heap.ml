(** Test heaps *)

open OUnit
open Helpers
module Sequence = CCSequence

let test_empty () =
  let h = Heap.empty ~cmp:(fun x y -> x - y) in
  OUnit.assert_bool "is_empty empty" (Heap.is_empty h);
  Heap.insert h 42;
  OUnit.assert_bool "not empty" (not (Heap.is_empty h));
  ()

let test_sort () =
  let h = Heap.empty ~cmp:(fun x y -> x - y) in
  (* Heap sort *)
  let l = [3;4;2;1;6;5;0;7;10;9;8] in
  Heap.of_seq h (Sequence.of_list l);
  OUnit.assert_equal ~printer:string_of_int 11 (Heap.size h);
  let l' = Sequence.to_list (Heap.to_seq h) in
  OUnit.assert_equal ~printer:print_int_list [0;1;2;3;4;5;6;7;8;9;10] l'

let test_remove () =
  let h = Heap.empty ~cmp:(fun x y -> x - y) in
  let l = [3;4;2;1;6;5;0;7;10;9;8] in
  Heap.of_seq h (Sequence.of_list l);
  (* check pop *)
  OUnit.assert_equal 0 (Heap.pop h);
  OUnit.assert_equal 1 (Heap.pop h);
  OUnit.assert_equal 2 (Heap.pop h);
  OUnit.assert_equal 3 (Heap.pop h);
  (* check that elements have been removed *)
  let l' = Sequence.to_list (Heap.to_seq h) in
  OUnit.assert_equal ~printer:print_int_list [4;5;6;7;8;9;10] l'

let suite =
  "test_heaps" >:::
    [ "test_empty" >:: test_empty;
      "test_sort" >:: test_sort;
      "test_remove" >:: test_remove;
    ]
