
open OUnit
open Containers_misc



let test_add () =
  let h = PHashtbl.create 5 in
  PHashtbl.replace h 42 "foo";
  OUnit.assert_equal (PHashtbl.find h 42) "foo"

let my_list = 
  [ 1, "a";
    2, "b";
    3, "c";
    4, "d";
  ]

let my_seq = Sequence.of_list my_list

let test_of_seq () =
  let h = PHashtbl.create 5 in
  PHashtbl.of_seq h my_seq;
  OUnit.assert_equal (PHashtbl.find h 2) "b";
  OUnit.assert_equal (PHashtbl.find h 1) "a";
  OUnit.assert_raises Not_found (fun () -> PHashtbl.find h 42);
  ()

let test_to_seq () =
  let h = PHashtbl.create 5 in
  PHashtbl.of_seq h my_seq;
  let l = Sequence.to_list (PHashtbl.to_seq h) in
  OUnit.assert_equal my_list (List.sort compare l)

let test_resize () =
  let h = PHashtbl.create 5 in
  for i = 0 to 10 do
    PHashtbl.add h i (string_of_int i);
  done;
  OUnit.assert_bool "must have been resized" (PHashtbl.length h > 5);
  ()

let test_eq () =
  let h = PHashtbl.create 3
    ~eq:(fun x y -> x mod 2 = y mod 2)
    ~hash:(fun i -> i mod 2) in
  PHashtbl.add h 1 "odd";
  PHashtbl.add h 2 "even";
  OUnit.assert_equal (PHashtbl.find h 3) "odd";
  OUnit.assert_equal (PHashtbl.find h 51) "odd";
  OUnit.assert_equal (PHashtbl.find h 42) "even";
  ()

let test_copy () =
  let h = PHashtbl.create 2 in
  PHashtbl.add h 1 "one";
  OUnit.assert_equal (PHashtbl.find h 1) "one";
  OUnit.assert_raises Not_found (fun () -> PHashtbl.find h 2);
  let h' = PHashtbl.copy h in
  PHashtbl.add h' 2 "two";
  OUnit.assert_equal (PHashtbl.find h' 1) "one";
  OUnit.assert_equal (PHashtbl.find h' 2) "two";
  OUnit.assert_equal (PHashtbl.find h 1) "one";
  OUnit.assert_raises Not_found (fun () -> PHashtbl.find h 2);
  ()

let test_remove () =
  let h = PHashtbl.create 3 in
  PHashtbl.of_seq h my_seq;
  OUnit.assert_equal (PHashtbl.find h 2) "b";
  OUnit.assert_equal (PHashtbl.find h 3) "c";
  OUnit.assert_equal (PHashtbl.find h 4) "d";
  OUnit.assert_equal (PHashtbl.length h) 4;
  PHashtbl.remove h 2;
  OUnit.assert_equal (PHashtbl.find h 3) "c";
  OUnit.assert_equal (PHashtbl.length h) 3;
  (* test that 2 has been removed *)
  OUnit.assert_raises Not_found (fun () -> PHashtbl.find h 2)

let test_filter () =
  let h = PHashtbl.create 5 in
  PHashtbl.of_seq h my_seq;
  OUnit.assert_equal (PHashtbl.length h) 4;
  PHashtbl.filter (fun k _ -> (k mod 2) = 0) h;
  OUnit.assert_equal (PHashtbl.length h) 2;
  OUnit.assert_bool "4 mem" (PHashtbl.mem h 4);
  OUnit.assert_bool "2 mem" (PHashtbl.mem h 2);
  OUnit.assert_bool "1 not mem" (not (PHashtbl.mem h 1));
  OUnit.assert_bool "3 not mem" (not (PHashtbl.mem h 3));
  ()

let test_map () =
  let h = PHashtbl.create 5 in
  PHashtbl.of_seq h my_seq;
  OUnit.assert_equal (PHashtbl.length h) 4;
  let h' = PHashtbl.map (fun k v -> String.uppercase v) h in
  OUnit.assert_equal (PHashtbl.length h') 4;
  OUnit.assert_equal (PHashtbl.find h' 1) "A";
  OUnit.assert_equal (PHashtbl.find h' 2) "B";
  OUnit.assert_equal (PHashtbl.find h' 3) "C";
  OUnit.assert_equal (PHashtbl.find h' 4) "D"

let suite =
  "test_pHashtbl" >:::
    [ "test_add" >:: test_add;
      "test_of_seq" >:: test_of_seq;
      "test_to_seq" >:: test_to_seq;
      "test_resize" >:: test_resize;
      "test_eq" >:: test_eq;
      "test_copy" >:: test_copy;
      "test_remove" >:: test_remove;
      "test_filter" >:: test_filter;
      "test_map" >:: test_map;
    ]
