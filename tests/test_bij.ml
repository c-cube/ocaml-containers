
open OUnit

open Bij

let test_int2 () =
  let bij = pair int_ int_ in
  let s = Sexp.to_string bij (1,2) in
  OUnit.assert_equal ~printer:(fun x -> x) "(1 2)" s

let test_escape () =
  let bij = pair int_ (pair string_ string_) in
  let s = Sexp.to_string bij (1,("foo()","bar\n hello")) in
  OUnit.assert_equal ~printer:(fun x -> x) "(1 (foo(\\) bar\\n\\ hello))" s

let test_intlist () =
  let bij = list_ int_ in
  let l = Sequence.to_list (Sequence.int_range ~start:0 ~stop:10000) in
  let s = Sexp.to_string ~bij l in
  let l' = Sexp.of_string ~bij s in
  OUnit.assert_equal l l'

let suite =
  "test_bij" >:::
    [ "test_int2" >:: test_int2;
      "test_escape" >:: test_escape;
      "test_intlist" >:: test_intlist;
    ]
