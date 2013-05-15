
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
    ~inject:(function
      | Const s -> 'c', BranchTo (string_, s)
      | Int i -> 'i', BranchTo (int_, i)
      | App l -> 'a', BranchTo (list_ (mk_bij ()), l))
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

let random_str len =
  let s = String.make len ' ' in
  for i = 0 to len - 1 do
    s.[i] <- "abcdefghijklmnopqrstuvwxyz".[Random.int 26]
  done;
  s

let rec random_term depth =
  if depth = 0
    then if Random.bool ()
      then Const (random_str (1 + Random.int 5))
      else Int (Random.int 20)
    else
      let len = Random.int (1 + Random.int 10) in
      let seq = Sequence.map (fun _ -> random_term (depth-1))
        (Sequence.int_range ~start:1 ~stop:len) in
      App (Sequence.to_list seq)

let test_term_random ?(depth=5) n () =
  for i = 0 to n - 1 do
    let t = random_term depth in
    let s = SexpStr.to_string ~bij:bij_term t in
    let t' = SexpStr.of_string ~bij:bij_term s in
    OUnit.assert_equal t t'
  done

let test_complicated () =
  let bij = triple int_ (pair bool_ (many float_))
    (map ~inject:(fun (a,b) -> (b,a)) ~extract:(fun (b,a) -> a,b) (pair int_ bool_)) in
  let x = (1, (true, [1.; 2.; 3.]), (false, 42)) in
  let s = SexpStr.to_string ~bij x in
  let x' = SexpStr.of_string ~bij s in
  OUnit.assert_equal x x'

let suite =
  "test_bij" >:::
    [ "test_int2" >:: test_int2;
      "test_escape" >:: test_escape;
      "test_intlist10" >:: test_intlist 10;
      "test_intlist100" >:: test_intlist 100;
      "test_intlist10_000" >:: test_intlist 10_000;
      "test_rec" >:: test_rec;
      "test_term_random100" >:: test_term_random 100;
      "test_term_random100_depth10" >:: test_term_random ~depth:10 100;
      "test_complicated" >:: test_complicated;
    ]
