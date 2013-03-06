
open OUnit

(** Test Univ embedding *)

let test_val () =
  let e1 = Univ.embed () in
  let e2 = Univ.embed () in
  let v1 = e1.Univ.pack 42 in
  let v2 = e2.Univ.pack "hello" in
  OUnit.assert_equal (Some 42) (e1.Univ.unpack v1);
  OUnit.assert_equal None (e1.Univ.unpack v2);
  OUnit.assert_equal (Some "hello") (e2.Univ.unpack v2);
  OUnit.assert_equal None (e2.Univ.unpack v1);
  ()

let test_compatible () =
  let e1 = Univ.embed () in
  let e2 = Univ.embed () in
  let v1 = e1.Univ.pack 42 in
  let v2 = e2.Univ.pack "hello" in
  OUnit.assert_bool "compatible" (e1.Univ.compatible v1);
  OUnit.assert_bool "not compatible" (not (e1.Univ.compatible v2));
  OUnit.assert_bool "compatible" (e2.Univ.compatible v2);
  OUnit.assert_bool "not compatible" (not (e2.Univ.compatible v1));
  ()

let test_set () =
  let e1 = (Univ.embed () : int Univ.embedding) in
  let e2 = (Univ.embed () : string Univ.embedding) in
  (* create val *)
  let v = e1.Univ.pack 42 in
  OUnit.assert_equal (Some 42) (e1.Univ.unpack v);
  OUnit.assert_equal None (e2.Univ.unpack v);
  (* set content, keeping type *)
  e1.Univ.set v 100;
  OUnit.assert_equal (Some 100) (e1.Univ.unpack v);
  OUnit.assert_equal None (e2.Univ.unpack v);
  (* set content, changing type *)
  e2.Univ.set v "hello";
  OUnit.assert_equal None (e1.Univ.unpack v);
  OUnit.assert_equal (Some "hello") (e2.Univ.unpack v);
  ()

let suite =
  "test_univ" >:::
    [ "test_val" >:: test_val;
      "test_compatible" >:: test_compatible;
      "test_set" >:: test_set;
    ]

