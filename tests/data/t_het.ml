

module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCHet;;

t @@ fun () ->
  let k1 : int Key.t = Key.create() in
  let k2 : int Key.t = Key.create() in
  let k3 : string Key.t = Key.create() in
  let k4 : float Key.t = Key.create() in

  let tbl = Tbl.create () in

  Tbl.add tbl k1 1;
  Tbl.add tbl k2 2;
  Tbl.add tbl k3 "k3";

  assert_equal (Some 1) (Tbl.find tbl k1);
  assert_equal (Some 2) (Tbl.find tbl k2);
  assert_equal (Some "k3") (Tbl.find tbl k3);
  assert_equal None (Tbl.find tbl k4);
  assert_equal 3 (Tbl.length tbl);

  Tbl.add tbl k1 10;
  assert_equal (Some 10) (Tbl.find tbl k1);
  assert_equal 3 (Tbl.length tbl);
  assert_equal None (Tbl.find tbl k4);

  Tbl.add tbl k4 0.0;
  assert_equal (Some 0.0) (Tbl.find tbl k4);
  true
