
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

let pp_int_list l =
  let b = Buffer.create 4 in
  Format.fprintf (Format.formatter_of_buffer b) "%a@?"
    (Sequence.pp_seq Format.pp_print_int) (Sequence.of_list l);
  Buffer.contents b

let test_intlist n () =
  let bij = list_ int_ in
  let l = Sequence.to_list (Sequence.int_range ~start:0 ~stop:n) in
  let s = Sexp.to_string ~bij l in
  let l' = Sexp.of_string ~bij s in
  OUnit.assert_equal ~printer:pp_int_list l l'

let suite =
  "test_bij" >:::
    [ "test_int2" >:: test_int2;
      "test_escape" >:: test_escape;
      "test_intlist10" >:: test_intlist 10;
      "test_intlist100" >:: test_intlist 100;
      "test_intlist10_000" >:: test_intlist 10_000;
    ]
