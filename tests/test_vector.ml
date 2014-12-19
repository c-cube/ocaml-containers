
open OUnit

module Vector = CCVector


let test_clear () =
  let v = Vector.of_seq Sequence.(1 -- 10) in
  OUnit.assert_equal 10 (Vector.size v);
  Vector.clear v;
  OUnit.assert_equal 0 (Vector.size v);
  OUnit.assert_bool "empty_after_clear" (Sequence.is_empty (Vector.to_seq v));
  ()

let test_append () =
  let a = Vector.of_seq Sequence.(1 -- 5) in
  let b = Vector.of_seq Sequence.(6 -- 10) in
  Vector.append a b;
  OUnit.assert_equal 10 (Vector.size a);
  OUnit.assert_equal (Sequence.to_array Sequence.(1 -- 10)) (Vector.to_array a);
  OUnit.assert_equal (Sequence.to_array Sequence.(6 -- 10)) (Vector.to_array b);
  ()

let test_copy () =
  let v = Vector.of_seq Sequence.(1 -- 100) in
  OUnit.assert_equal 100 (Vector.size v);
  let v' = Vector.copy v in
  OUnit.assert_equal 100 (Vector.size v');
  Vector.clear v';
  OUnit.assert_bool "empty" (Vector.is_empty v');
  OUnit.assert_bool "not_empty" (not (Vector.is_empty v));
  ()

let test_shrink () =
  let v = Vector.of_seq Sequence.(1 -- 10) in
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

open QCheck
module V = Vector

let gen sub = Arbitrary.(lift V.of_list (list sub))
let pp v = PP.(list string) (List.map string_of_int (V.to_list v))

let check_append =
  let gen = Arbitrary.(pair (gen small_int) (gen small_int)) in
  let prop (v1, v2) =
    let l1 = V.to_list v1 in
    V.append v1 v2;
    Sequence.to_list (V.to_seq v1) =
      Sequence.(to_list (append (of_list l1) (V.to_seq v2)))
  in
  let name = "vector_append" in
  mk_test ~name ~pp:PP.(pair pp pp) gen prop

let check_sort =
  let gen = Arbitrary.(gen small_int) in
  let prop v =
    let v' = V.copy v in
    V.sort' Pervasives.compare v';
    let l = V.to_list v' in
    List.sort compare l = l
  in
  let name = "vector_sort" in
  mk_test ~name ~pp gen prop

let check_shrink =
  let gen = Arbitrary.(gen small_int) in
  let prop v =
    let n = V.size v / 2 in
    let l = V.to_list v in
    let h = Sequence.(to_list (take n (of_list l))) in
    let v' = V.copy v in
    V.shrink v' n;
    h = V.to_list v'
  in
  let name = "vector_shrink" in
  mk_test ~name ~pp gen prop

let props =
  [ check_append
  ; check_sort
  ; check_shrink
  ]
