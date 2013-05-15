
open OUnit

open Bij

let test_int2 () =
  let bij = pair int_ int_ in
  let s = SexpStr.to_string bij (1,2) in
  OUnit.assert_equal ~printer:(fun x -> x) "(1 2)" s

let test_escape () =
  let bij = pair int_ (pair string_ string_) in
  let s = SexpStr.to_string bij (1,("foo()","bar\n hello")) in
  OUnit.assert_equal ~printer:(fun x -> x) "(1 (foo(\\) bar\\n\\ hello))" s

let pp_int_list l =
  let b = Buffer.create 4 in
  Format.fprintf (Format.formatter_of_buffer b) "%a@?"
    (Sequence.pp_seq Format.pp_print_int) (Sequence.of_list l);
  Buffer.contents b

let test_intlist n () =
  let bij = list_ int_ in
  let l = Sequence.to_list (Sequence.int_range ~start:0 ~stop:n) in
  let s = SexpStr.to_string ~bij l in
  let l' = SexpStr.of_string ~bij s in
  OUnit.assert_equal ~printer:pp_int_list l l'

type term =
  | Const of string
  | Int of int
  | App of term list

let bij_term =
  let rec mk_bij () =
    switch
    ~inject:(fun t -> match t with
      | Const s -> 'c', BranchTo (string_, s, t)
      | Int i -> 'i', BranchTo (int_, i, t)
      | App l -> 'a', BranchTo (list_ (mk_bij ()), l, t))
    ~extract:(function
      | 'c' -> BranchFrom (string_, fun x -> Const x)
      | 'i' -> BranchFrom (int_, fun x -> Int x)
      | 'a' -> BranchFrom (list_ (mk_bij ()), fun l -> App l)
      | _ -> raise (DecodingError "unexpected case switch"))
  in mk_bij ()

let test_rec () =
  let t = App [Const "foo"; App [Const "bar"; Int 1; Int 2]; Int 3; Const "hello"] in
  let s = SexpStr.to_string ~bij:bij_term t in
  Printf.printf "to: %s\n" s;
  let t' = SexpStr.of_string ~bij:bij_term s in
  OUnit.assert_equal t t'

let suite =
  "test_bij" >:::
    [ "test_int2" >:: test_int2;
      "test_escape" >:: test_escape;
      "test_intlist10" >:: test_intlist 10;
      "test_intlist100" >:: test_intlist 100;
      "test_intlist10_000" >:: test_intlist 10_000;
      "test_rec" >:: test_rec;
    ]
