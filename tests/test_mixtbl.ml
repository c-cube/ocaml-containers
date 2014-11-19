
open OUnit
open Containers_misc
open CCFun

let example () =
  let inj_int = Mixtbl.create_inj () in
  let tbl = Mixtbl.create 10 in
  OUnit.assert_equal None (Mixtbl.get ~inj:inj_int tbl "a");
  Mixtbl.set inj_int tbl "a" 1;
  OUnit.assert_equal (Some 1) (Mixtbl.get ~inj:inj_int tbl "a");
  let inj_string = Mixtbl.create_inj () in
  Mixtbl.set inj_string tbl "b" "Hello";
  OUnit.assert_equal (Some "Hello") (Mixtbl.get inj_string tbl "b");
  OUnit.assert_equal None (Mixtbl.get inj_string tbl "a");
  OUnit.assert_equal (Some 1) (Mixtbl.get inj_int tbl "a");
  Mixtbl.set inj_string tbl "a" "Bye";
  OUnit.assert_equal None (Mixtbl.get inj_int tbl "a");
  OUnit.assert_equal (Some "Bye") (Mixtbl.get inj_string tbl "a");
  ()

let test_length () =
  let inj_int = Mixtbl.create_inj () in
  let tbl = Mixtbl.create 5 in
  Mixtbl.set ~inj:inj_int tbl "foo" 1;
  Mixtbl.set ~inj:inj_int tbl "bar" 2;
  OUnit.assert_equal 2 (Mixtbl.length tbl);
  OUnit.assert_equal 2 (Mixtbl.find ~inj:inj_int tbl "bar");
  Mixtbl.set ~inj:inj_int tbl "foo" 42;
  OUnit.assert_equal 2 (Mixtbl.length tbl);
  Mixtbl.remove tbl "bar";
  OUnit.assert_equal 1 (Mixtbl.length tbl);
  ()

let test_clear () =
  let inj_int = Mixtbl.create_inj () in
  let inj_str = Mixtbl.create_inj () in
  let tbl = Mixtbl.create 5 in
  Mixtbl.set ~inj:inj_int tbl "foo" 1;
  Mixtbl.set ~inj:inj_int tbl "bar" 2;
  Mixtbl.set ~inj:inj_str tbl "baaz" "hello";
  OUnit.assert_equal 3 (Mixtbl.length tbl);
  Mixtbl.clear tbl;
  OUnit.assert_equal 0 (Mixtbl.length tbl);
  ()

let test_mem () =
  let inj_int = Mixtbl.create_inj () in
  let inj_str = Mixtbl.create_inj () in
  let tbl = Mixtbl.create 5 in
  Mixtbl.set ~inj:inj_int tbl "foo" 1;
  Mixtbl.set ~inj:inj_int tbl "bar" 2;
  Mixtbl.set ~inj:inj_str tbl "baaz" "hello";
  OUnit.assert_bool "mem foo int" (Mixtbl.mem ~inj:inj_int tbl "foo");
  OUnit.assert_bool "mem bar int" (Mixtbl.mem ~inj:inj_int tbl "bar");
  OUnit.assert_bool "not mem baaz int" (not (Mixtbl.mem ~inj:inj_int tbl "baaz"));
  OUnit.assert_bool "not mem foo str" (not (Mixtbl.mem ~inj:inj_str tbl "foo"));
  OUnit.assert_bool "not mem bar str" (not (Mixtbl.mem ~inj:inj_str tbl "bar"));
  OUnit.assert_bool "mem baaz str" (Mixtbl.mem ~inj:inj_str tbl "baaz");
  ()

let test_keys () =
  let inj_int = Mixtbl.create_inj () in
  let inj_str = Mixtbl.create_inj () in
  let tbl = Mixtbl.create 5 in
  Mixtbl.set ~inj:inj_int tbl "foo" 1;
  Mixtbl.set ~inj:inj_int tbl "bar" 2;
  Mixtbl.set ~inj:inj_str tbl "baaz" "hello";
  let l = Mixtbl.keys_seq tbl |> CCSequence.to_list in
  OUnit.assert_equal ["baaz"; "bar"; "foo"] (List.sort compare l);
  ()

let test_bindings () =
  let inj_int = Mixtbl.create_inj () in
  let inj_str = Mixtbl.create_inj () in
  let tbl = Mixtbl.create 5 in
  Mixtbl.set ~inj:inj_int tbl "foo" 1;
  Mixtbl.set ~inj:inj_int tbl "bar" 2;
  Mixtbl.set ~inj:inj_str tbl "baaz" "hello";
  Mixtbl.set ~inj:inj_str tbl "str" "rts";
  let l_int = Mixtbl.bindings_of tbl ~inj:inj_int |> CCSequence.to_list in
  OUnit.assert_equal ["bar", 2; "foo", 1] (List.sort compare l_int);
  let l_str = Mixtbl.bindings_of tbl ~inj:inj_str |> CCSequence.to_list in
  OUnit.assert_equal ["baaz", "hello"; "str", "rts"] (List.sort compare l_str);
  ()

let suite =
  "mixtbl" >:::
    [ "example" >:: example;
      "length" >:: test_length;
      "clear" >:: test_clear;
      "mem" >:: test_mem;
      "bindings" >:: test_bindings;
    ]

