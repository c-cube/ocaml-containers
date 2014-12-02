
open OUnit
open Gen.Infix

module GR = Gen.Restart

let pint i = string_of_int i
let pilist l =
  let b = Buffer.create 15 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "%a@?"
    (Gen.pp Format.pp_print_int) (Gen.of_list l);
  Buffer.contents b
let pi2list l =
  let b = Buffer.create 15 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "%a@?"
    (Gen.pp (fun fmt (a,b) -> Format.fprintf fmt "%d,%d" a b))
    (Gen.of_list l);
  Buffer.contents b
let pstrlist l =
  let b = Buffer.create 15 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "%a@?"
    (Gen.pp Format.pp_print_string) (Gen.of_list l);
  Buffer.contents b

let test_singleton () =
  let gen = Gen.singleton 42 in
  OUnit.assert_equal (Some 42) (Gen.get gen);
  OUnit.assert_equal None (Gen.get gen);
  let gen = Gen.singleton 42 in
  OUnit.assert_equal 1 (Gen.length gen);
  ()

let test_iter () =
  let e = GR.(1 -- 10) in
  OUnit.assert_equal ~printer:pint 10 (GR.length e);
  OUnit.assert_equal [1;2] GR.(to_list (1 -- 2));
  OUnit.assert_equal [1;2;3;4;5] (GR.to_list (GR.take 5 e));
  ()

let test_map () =
  let e = 1 -- 10 in
  let e' = Gen.map string_of_int e in
  OUnit.assert_equal ~printer:pstrlist ["9"; "10"] (Gen.to_list (Gen.drop 8 e'));
  ()

let test_append () =
  let e = Gen.append (1 -- 5) (6 -- 10) in
  OUnit.assert_equal [10;9;8;7;6;5;4;3;2;1] (Gen.to_rev_list e);
  ()

let test_flat_map () =
  let e = 1 -- 3 in
  let e' = e >>= (fun x -> x -- (x+1)) in
  OUnit.assert_equal [1;2;2;3;3;4] (Gen.to_list e');
  ()

let test_zip () =
  let e = Gen.zip_with (+) (Gen.repeat 1) (4--7) in
  OUnit.assert_equal [5;6;7;8] (Gen.to_list e);
  ()

let test_filter_map () =
  let f x = if x mod 2 = 0 then Some (string_of_int x) else None in
  let e = Gen.filter_map f (1 -- 10) in
  OUnit.assert_equal ["2"; "4"; "6"; "8"; "10"] (Gen.to_list e);
  ()

let test_merge () =
  let e = Gen.of_list [1--3; 4--6; 7--9] in
  let e' = Gen.merge e in
  OUnit.assert_equal [1;2;3;4;5;6;7;8;9] (Gen.to_list e' |> List.sort compare);
  ()

let test_persistent () =
  let i = ref 0 in
  let gen () =
    let j = !i in
    if j > 5 then None else (incr i; Some j)
  in
  let e = Gen.persistent gen in
  OUnit.assert_equal [0;1;2;3;4;5] (GR.to_list e);
  OUnit.assert_equal [0;1;2;3;4;5] (GR.to_list e);
  OUnit.assert_equal [0;1;2;3;4;5] (GR.to_list e);
  ()

let test_round_robin () =
  let e = GR.round_robin ~n:2 GR.(1--10) in
  match e with
  | [a;b] ->
    OUnit.assert_equal [1;3;5;7;9] (Gen.to_list a);
    OUnit.assert_equal [2;4;6;8;10] (Gen.to_list b)
  | _ -> OUnit.assert_failure "wrong list lenght"

let test_big_rr () =
  let e = GR.round_robin ~n:3 GR.(1 -- 999) in
  let l = List.map Gen.length e in
  OUnit.assert_equal [333;333;333] l;
  ()

let test_merge_sorted () =
  [Gen.of_list [1;3;5]; Gen.of_list [0;1;1;3;4;6;10]; Gen.of_list [2;2;11]]
    |> Gen.sorted_merge_n ?cmp:None
    |> Gen.to_list
    |> OUnit.assert_equal ~printer:pilist [0;1;1;1;2;2;3;3;4;5;6;10;11]

let test_interleave () =
  let e1 = Gen.of_list [1;3;5;7;9] in
  let e2 = Gen.of_list [2;4;6;8;10] in
  let e = Gen.interleave e1 e2 in
  OUnit.assert_equal [1;2;3;4;5;6;7;8;9;10] (Gen.to_list e);
  ()

let test_intersperse () =
  let e = 1 -- 5 in
  let e' = Gen.intersperse 0 e in
  OUnit.assert_equal [1;0;2;0;3;0;4;0;5] (Gen.to_list e');
  ()

let test_product () =
  let printer = pi2list in
  let e = Gen.product (1--3) (4--5) in
  OUnit.assert_equal ~printer [1,4; 1,5; 2,4; 2,5; 3,4; 3,5]
    (List.sort compare (Gen.to_list e));
  ()

let suite =
  "test_gen" >:::
    [ "test_singleton" >:: test_singleton;
      "test_iter" >:: test_iter;
      "test_map" >:: test_map;
      "test_append" >:: test_append;
      "test_flat_map" >:: test_flat_map;
      "test_zip" >:: test_zip;
      "test_filter_map" >:: test_filter_map;
      "test_merge" >:: test_merge;
      "test_persistent" >:: test_persistent;
      "test_round_robin" >:: test_round_robin;
      "test_big_rr" >:: test_big_rr;
      "test_merge_sorted" >:: test_merge_sorted;
      "test_interleave" >:: test_interleave;
      "test_intersperse" >:: test_intersperse;
      "test_product" >:: test_product;
    ]
