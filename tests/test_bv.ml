open OUnit

let test_cardinal () =
  let bv1 = BV.create ~size:87 true in
  assert_equal ~printer:string_of_int 87 (BV.cardinal bv1);
  ()

let test_get () =
  let bv = BV.create ~size:99 false in
  assert_bool "32 must be false" (not (BV.get bv 32));
  assert_bool "88 must be false" (not (BV.get bv 88));
  assert_bool "5 must be false" (not (BV.get bv 5));
  BV.set bv 32;
  BV.set bv 88;
  BV.set bv 5;
  assert_bool "32 must be true" (BV.get bv 32);
  assert_bool "88 must be true" (BV.get bv 88);
  assert_bool "5 must be true" (BV.get bv 5);
  assert_bool "33 must be false" (not (BV.get bv 33));
  assert_bool "44 must be false" (not (BV.get bv 44));
  assert_bool "1 must be false" (not (BV.get bv 1));
  ()

let test_list () =
  let bv = BV.of_list [1; 5; 156; 0; 222] in
  assert_equal ~printer:string_of_int 5 (BV.cardinal bv);
  BV.set bv 201;
  assert_equal ~printer:string_of_int 6 (BV.cardinal bv);
  let l = BV.to_list bv in
  let l = List.sort compare l in
  assert_equal [0;1;5;156;201;222] l;
  ()

let test_clear () =
  let bv = BV.of_list [1; 5; 200] in
  assert_equal ~printer:string_of_int 3 (BV.cardinal bv);
  BV.clear bv;
  assert_equal ~printer:string_of_int 0 (BV.cardinal bv);
  assert_bool "must be empty" (BV.is_empty bv);
  ()

let test_union () =
  let bv1 = BV.of_list [1;2;3;4] in
  let bv2 = BV.of_list [4;200;3] in
  let bv = BV.union bv1 bv2 in
  let l = List.sort compare (BV.to_list bv) in
  assert_equal [1;2;3;4;200] l;
  ()

let test_inter () =
  let bv1 = BV.of_list [1;2;3;4] in
  let bv2 = BV.of_list [4;200;3] in
  BV.inter_into ~into:bv1 bv2;
  let l = List.sort compare (BV.to_list bv1) in
  assert_equal [3;4] l;
  ()

let test_select () =
  let bv = BV.of_list [1;2;5;400] in
  let arr = [|"a"; "b"; "c"; "d"; "e"; "f"|] in
  let l = List.sort compare (BV.selecti bv arr) in
  assert_equal [("b",1); ("c",2); ("f",5)] l;
  ()

let suite = "test_bv" >:::
  [ "test_cardinal" >:: test_cardinal
  ; "test_get" >:: test_get
  ; "test_list" >:: test_list
  ; "test_clear" >:: test_clear
  ; "test_union" >:: test_union
  ; "test_inter" >:: test_inter
  ; "test_select" >:: test_select
  ]

open QCheck

let check_create_cardinal =
  let gen = Arbitrary.small_int in
  let prop n = BV.cardinal (BV.create ~size:n true) = n in
  let name = "bv_create_cardinal" in
  mk_test ~name ~pp:string_of_int gen prop

let pp bv = PP.(list string) (List.map string_of_int (BV.to_list bv))

let check_iter_true =
  let gen = Arbitrary.(lift BV.of_list (list small_int)) in
  let prop bv =
    let l' = Sequence.to_rev_list (BV.iter_true bv) in
    let bv' = BV.of_list l' in
    BV.cardinal bv = BV.cardinal bv'
  in
  let name = "bv_iter_true" in
  mk_test ~pp ~size:BV.cardinal ~name gen prop

let props =
  [ check_create_cardinal
  ; check_iter_true
  ]
