
open OUnit
open Containers_misc

(** Test Univ embedding *)

let test_val () =
  let e1 = Univ.embed () in
  let e2 = Univ.embed () in
  let v1 = Univ.pack e1 42 in
  let v2 = Univ.pack e2 "hello" in
  OUnit.assert_equal (Some 42) (Univ.unpack e1 v1);
  OUnit.assert_equal None (Univ.unpack e1 v2);
  OUnit.assert_equal (Some "hello") (Univ.unpack e2 v2);
  OUnit.assert_equal None (Univ.unpack e2 v1);
  ()

let test_compatible () =
  let e1 = Univ.embed () in
  let e2 = Univ.embed () in
  let v1 = Univ.pack e1 42 in
  let v2 = Univ.pack e2 "hello" in
  OUnit.assert_bool "compatible" (Univ.compatible e1 v1);
  OUnit.assert_bool "not compatible" (not (Univ.compatible e1 v2));
  OUnit.assert_bool "compatible" (Univ.compatible e2 v2);
  OUnit.assert_bool "not compatible" (not (Univ.compatible e2 v1));
  ()

let test_set () =
  let e1 = (Univ.embed () : int Univ.embedding) in
  let e2 = (Univ.embed () : string Univ.embedding) in
  (* create val *)
  let v = Univ.pack e1 42 in
  OUnit.assert_equal (Some 42) (Univ.unpack e1 v);
  OUnit.assert_equal None (Univ.unpack e2 v);
  (* set content, keeping type *)
  Univ.set e1 v 100;
  OUnit.assert_equal (Some 100) (Univ.unpack e1 v);
  OUnit.assert_equal None (Univ.unpack e2 v);
  (* set content, changing type *)
  Univ.set e2 v "hello";
  OUnit.assert_equal None (Univ.unpack e1 v);
  OUnit.assert_equal (Some "hello") (Univ.unpack e2 v);
  ()

let suite =
  "test_univ" >:::
    [ "test_val" >:: test_val;
      "test_compatible" >:: test_compatible;
      "test_set" >:: test_set;
    ]

