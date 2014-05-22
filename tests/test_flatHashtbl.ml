
open OUnit

module Sequence = CCSequence

module IHashtbl = FlatHashtbl.Make(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

let test_add () =
  let h = IHashtbl.create 5 in
  IHashtbl.replace h 42 "foo";
  OUnit.assert_equal (IHashtbl.find h 42) "foo"

let my_list = 
  [ 1, "a";
    2, "b";
    3, "c";
    4, "d";
  ]

let my_seq = Sequence.of_list my_list

let test_of_seq () =
  let h = IHashtbl.create 5 in
  IHashtbl.of_seq h my_seq;
  OUnit.assert_equal (IHashtbl.find h 2) "b";
  OUnit.assert_equal (IHashtbl.find h 1) "a";
  OUnit.assert_raises Not_found (fun () -> IHashtbl.find h 42);
  ()

let test_to_seq () =
  let h = IHashtbl.create 5 in
  IHashtbl.of_seq h my_seq;
  let l = Sequence.to_list (IHashtbl.to_seq h) in
  OUnit.assert_equal my_list (List.sort compare l)

let test_resize () =
  let h = IHashtbl.create 5 in
  for i = 0 to 10 do
    IHashtbl.replace h i (string_of_int i);
  done;
  OUnit.assert_bool "must have been resized" (IHashtbl.length h > 5);
  ()

let test_eq () =
  let h = IHashtbl.create 3 in
  IHashtbl.replace h 1 "odd";
  IHashtbl.replace h 2 "even";
  OUnit.assert_equal (IHashtbl.find h 1) "odd";
  OUnit.assert_equal (IHashtbl.find h 2) "even";
  ()

let test_copy () =
  let h = IHashtbl.create 2 in
  IHashtbl.replace h 1 "one";
  OUnit.assert_equal (IHashtbl.find h 1) "one";
  OUnit.assert_raises Not_found (fun () -> IHashtbl.find h 2);
  let h' = IHashtbl.copy h in
  IHashtbl.replace h' 2 "two";
  OUnit.assert_equal (IHashtbl.find h' 1) "one";
  OUnit.assert_equal (IHashtbl.find h' 2) "two";
  OUnit.assert_equal (IHashtbl.find h 1) "one";
  OUnit.assert_raises Not_found (fun () -> IHashtbl.find h 2);
  ()

let test_remove () =
  let h = IHashtbl.create 3 in
  IHashtbl.of_seq h my_seq;
  OUnit.assert_equal (IHashtbl.find h 2) "b";
  OUnit.assert_equal (IHashtbl.find h 3) "c";
  OUnit.assert_equal (IHashtbl.find h 4) "d";
  OUnit.assert_equal (IHashtbl.length h) 4;
  IHashtbl.remove h 2;
  OUnit.assert_equal (IHashtbl.find h 3) "c";
  OUnit.assert_equal (IHashtbl.length h) 3;
  (* test that 2 has been removed *)
  OUnit.assert_raises Not_found (fun () -> IHashtbl.find h 2)

let suite =
  "test_flatHashtbl" >:::
    [ "test_add" >:: test_add;
      "test_of_seq" >:: test_of_seq;
      "test_to_seq" >:: test_to_seq;
      "test_resize" >:: test_resize;
      "test_eq" >:: test_eq;
      "test_copy" >:: test_copy;
      "test_remove" >:: test_remove;
    ]

