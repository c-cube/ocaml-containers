
open OUnit

open Sequence.Infix

let test_clear () =
  let v = Vector.of_seq (1 -- 10) in
  OUnit.assert_equal 10 (Vector.size v);
  Vector.clear v;
  OUnit.assert_equal 0 (Vector.size v);
  OUnit.assert_bool "empty_after_clear" (Sequence.is_empty (Vector.to_seq v));
  ()

let test_append () =
  let a = Vector.of_seq (1 -- 5) in
  let b = Vector.of_seq (6 -- 10) in
  Vector.append a b;
  OUnit.assert_equal 10 (Vector.size a);
  OUnit.assert_equal (Sequence.to_array (1 -- 10)) (Vector.to_array a);
  OUnit.assert_equal (Sequence.to_array (6 -- 10)) (Vector.to_array b);
  ()

let test_copy () =
  let v = Vector.of_seq (1 -- 100) in
  OUnit.assert_equal 100 (Vector.size v);
  let v' = Vector.copy v in
  OUnit.assert_equal 100 (Vector.size v');
  Vector.clear v';
  OUnit.assert_bool "empty" (Vector.is_empty v');
  OUnit.assert_bool "not_empty" (not (Vector.is_empty v));
  ()

let test_shrink () =
  let v = Vector.of_seq (1 -- 10) in
  Vector.shrink v 5;
  OUnit.assert_equal [1;2;3;4;5] (Vector.to_list v);
  ()

let suite = 
  "test_vector" >:::
    [ "test_clear" >:: test_clear;
      "test_append" >:: test_append;
      "test_copy" >:: test_copy;
      "test_shrink" >:: test_shrink;
    ]
