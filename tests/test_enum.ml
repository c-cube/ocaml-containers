
open OUnit
open Enum.Infix

let pint i = string_of_int i
let plist l = Utils.sprintf "%a"
  (Sequence.pp_seq Format.pp_print_int) (Sequence.of_list l)
let pstrlist l = Utils.sprintf "%a"
  (Sequence.pp_seq Format.pp_print_string) (Sequence.of_list l)

let test_singleton () =
  let e = Enum.singleton 42 in
  let gen = Enum.start e in
  OUnit.assert_equal 42 (Enum.next gen);
  OUnit.assert_raises Enum.EOG (fun () -> Enum.next gen);
  OUnit.assert_equal 1 (Enum.length e);
  ()

let test_iter () =
  let e = 1 -- 10 in
  OUnit.assert_equal ~printer:pint 10 (Enum.length e);
  OUnit.assert_equal [1;2] (Enum.to_list (1 -- 2));
  OUnit.assert_equal [1;2;3;4;5] (Enum.to_list (Enum.take 5 e));
  ()

let test_map () =
  let e = 1 -- 10 in
  let e' = Enum.map string_of_int e in
  OUnit.assert_equal ~printer:pstrlist ["9"; "10"] (Enum.to_list (Enum.drop 8 e'));
  ()

let test_append () =
  let e = (1 -- 5) @@ (6 -- 10) in
  OUnit.assert_equal [10;9;8;7;6;5;4;3;2;1] (Enum.to_rev_list e);
  ()

let test_flatMap () =
  let e = 1 -- 3 in
  let e' = e >>= (fun x -> x -- (x+1)) in
  OUnit.assert_equal [1;2;2;3;3;4] (Enum.to_list e');
  ()

let test_zip () =
  let e = Enum.zipWith (+) (Enum.repeat 1) (4--7) in
  OUnit.assert_equal [5;6;7;8] (Enum.to_list e);
  ()

let test_filterMap () =
  let f x = if x mod 2 = 0 then Some (string_of_int x) else None in
  let e = Enum.filterMap f (1 -- 10) in
  OUnit.assert_equal ["2"; "4"; "6"; "8"; "10"] (Enum.to_list e);
  ()

let test_round_robin () =
  let e = Enum.of_list [1--3; 4--6; 7--9] in
  let e' = Enum.round_robin e in
  OUnit.assert_equal [1;4;7;2;5;8;3;6;9] (Enum.to_list e');
  ()

let test_persistent () =
  let i = ref 0 in
  let gen () =
    let j = !i in
    if j > 5 then raise Enum.EOG else (incr i; j)
  in
  let e = Enum.persistent gen in
  OUnit.assert_equal [0;1;2;3;4;5] (Enum.to_list e);
  OUnit.assert_equal [0;1;2;3;4;5] (Enum.to_list e);
  OUnit.assert_equal [0;1;2;3;4;5] (Enum.to_list e);
  ()

let test_tee () =
  let e = Enum.tee ~n:2 (1--10) in
  let e = Enum.map Enum.persistent e in
  let l = Enum.to_list e in
  match l with
  | [a;b] ->
    OUnit.assert_equal [1;3;5;7;9] (Enum.to_list a);
    OUnit.assert_equal [2;4;6;8;10] (Enum.to_list b)
  | _ -> OUnit.assert_failure "wrong list lenght"

let test_interleave () =
  let e1 = Enum.of_list [1;3;5;7;9] in
  let e2 = Enum.of_list [2;4;6;8;10] in
  let e = Enum.interleave e1 e2 in
  OUnit.assert_equal [1;2;3;4;5;6;7;8;9;10] (Enum.to_list e);
  ()

let test_intersperse () =
  let e = 1 -- 5 in
  let e' = Enum.intersperse 0 e in
  OUnit.assert_equal [1;0;2;0;3;0;4;0;5] (Enum.to_list e');
  ()

let test_product () =
  let e = Enum.product (1--3) (4--5) in
  OUnit.assert_equal [1,4; 1,5; 2,4; 2,5; 3,4; 3,5] (Enum.to_list e);
  ()

let suite =
  "test_enum" >:::
    [ "test_singleton" >:: test_singleton;
      "test_iter" >:: test_iter;
      "test_map" >:: test_map;
      "test_append" >:: test_append;
      "test_flatMap" >:: test_flatMap;
      "test_zip" >:: test_zip;
      "test_filterMap" >:: test_filterMap;
      "test_round_robin" >:: test_round_robin;
      "test_persistent" >:: test_persistent;
      "test_tee" >:: test_tee;
      "test_interleave" >:: test_interleave;
      "test_intersperse" >:: test_intersperse;
      "test_product" >:: test_product;
    ]
