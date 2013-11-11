
open OUnit
open Gen.Infix

let pint i = string_of_int i
let plist l = Utils.sprintf "%a"
  (Sequence.pp_seq Format.pp_print_int) (Sequence.of_list l)
let pstrlist l = Utils.sprintf "%a"
  (Sequence.pp_seq Format.pp_print_string) (Sequence.of_list l)

let test_singleton () =
  let e = Gen.singleton 42 in
  let gen = Gen.start e in
  OUnit.assert_equal 42 (Gen.Gen.next gen);
  OUnit.assert_raises Gen.EOG (fun () -> Gen.Gen.next gen);
  OUnit.assert_equal 1 (Gen.length e);
  ()

let test_iter () =
  let e = 1 -- 10 in
  OUnit.assert_equal ~printer:pint 10 (Gen.length e);
  OUnit.assert_equal [1;2] (Gen.to_list (1 -- 2));
  OUnit.assert_equal [1;2;3;4;5] (Gen.to_list (Gen.take 5 e));
  ()

let test_map () =
  let e = 1 -- 10 in
  let e' = Gen.map string_of_int e in
  OUnit.assert_equal ~printer:pstrlist ["9"; "10"] (Gen.to_list (Gen.drop 8 e'));
  ()

let test_append () =
  let e = (1 -- 5) @@ (6 -- 10) in
  OUnit.assert_equal [10;9;8;7;6;5;4;3;2;1] (Gen.to_rev_list e);
  ()

let test_flatMap () =
  let e = 1 -- 3 in
  let e' = e >>= (fun x -> x -- (x+1)) in
  OUnit.assert_equal [1;2;2;3;3;4] (Gen.to_list e');
  ()

let test_zip () =
  let e = Gen.zipWith (+) (Gen.repeat 1) (4--7) in
  OUnit.assert_equal [5;6;7;8] (Gen.to_list e);
  ()

let test_filterMap () =
  let f x = if x mod 2 = 0 then Some (string_of_int x) else None in
  let e = Gen.filterMap f (1 -- 10) in
  OUnit.assert_equal ["2"; "4"; "6"; "8"; "10"] (Gen.to_list e);
  ()

let test_merge () =
  let e = Gen.of_list [1--3; 4--6; 7--9] in
  let e' = Gen.merge e in
  OUnit.assert_equal [1;4;7;2;5;8;3;6;9] (Gen.to_list e');
  ()

let test_persistent () =
  let i = ref 0 in
  let gen () =
    let j = !i in
    if j > 5 then raise Gen.EOG else (incr i; j)
  in
  let e = Gen.persistent gen in
  OUnit.assert_equal [0;1;2;3;4;5] (Gen.to_list e);
  OUnit.assert_equal [0;1;2;3;4;5] (Gen.to_list e);
  OUnit.assert_equal [0;1;2;3;4;5] (Gen.to_list e);
  ()

let test_round_robin () =
  let e = Gen.round_robin ~n:2 (1--10) in
  let e = Gen.map Gen.persistent e in
  let l = Gen.to_list e in
  match l with
  | [a;b] ->
    OUnit.assert_equal [1;3;5;7;9] (Gen.to_list a);
    OUnit.assert_equal [2;4;6;8;10] (Gen.to_list b)
  | _ -> OUnit.assert_failure "wrong list lenght"

let test_big_rr () =
  let e = Gen.round_robin ~n:3 (1 -- 999) in
  let l = Gen.to_list e in
  let l' = List.map Gen.Gen.length l in
  OUnit.assert_equal [333;333;333] l';
  ()

let test_merge_sorted () =
  Gen.of_list [Gen.of_list [1;3;5]; Gen.of_list [0;1;1;3;4;6;10]; Gen.of_list [2;2;11]]
    |> Gen.sorted_merge_n ?cmp:None
    |> Gen.to_list
    |> OUnit.assert_equal ~printer:Helpers.print_int_list [0;1;1;1;2;2;3;3;4;5;6;10;11]

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
  let e = Gen.product (1--3) (4--5) in
  OUnit.assert_equal [1,4; 1,5; 2,4; 2,5; 3,4; 3,5] (Gen.to_list e);
  ()

let test_fair_product () =
  let e = Gen.fair_product (Gen.repeat ()) (1--3) in
  let _ = Gen.take 10 e in  (* succeeds -> ok *)
  ()

let suite =
  "test_gen" >:::
    [ "test_singleton" >:: test_singleton;
      "test_iter" >:: test_iter;
      "test_map" >:: test_map;
      "test_append" >:: test_append;
      "test_flatMap" >:: test_flatMap;
      "test_zip" >:: test_zip;
      "test_filterMap" >:: test_filterMap;
      "test_merge" >:: test_merge;
      "test_persistent" >:: test_persistent;
      "test_round_robin" >:: test_round_robin;
      "test_big_rr" >:: test_big_rr;
      "test_merge_sorted" >:: test_merge_sorted;
      "test_interleave" >:: test_interleave;
      "test_intersperse" >:: test_intersperse;
      "test_product" >:: test_product;
      "test_fair_product" >:: test_fair_product;
    ]
