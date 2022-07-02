
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

open CCHashtbl;;

eq "c" (let tbl = of_list [1,"a"; 2,"b"] in get_or tbl 3 ~default:"c");;
eq "b" (let tbl = of_list [1,"a"; 2,"b"] in get_or tbl 2 ~default:"c");;

t @@ fun () -> of_list [1,"a"; 2,"b"] |> map_list (fun x y -> string_of_int x ^ y)
    |> List.sort Stdlib.compare = ["1a"; "2b"];;

t @@ fun () ->
  let tbl = Hashtbl.create 32 in
  update tbl ~f:(fun _ _ -> Some "1") ~k:1;
  assert_equal (Some "1") (get tbl 1);
  update tbl ~f:(fun _ v->match v with Some _ -> assert false | None -> Some "2") ~k:2;
  assert_equal (Some "2") (get tbl 2);
  assert_equal 2 (Hashtbl.length tbl);
  update tbl ~f:(fun _ _ -> None) ~k:1;
  assert_equal None (get tbl 1);
  true
;;

t @@ fun () ->
  let tbl = Hashtbl.create 32 in
  let v1 = get_or_add tbl ~f:(fun _ -> "1") ~k:1 in
  assert_equal "1" v1;
  assert_equal (Some "1") (get tbl 1);
  let v2 = get_or_add tbl ~f:(fun _ ->"2") ~k:2 in
  assert_equal "2" v2;
  assert_equal (Some "2") (get tbl 2);
  assert_equal "2" (get_or_add tbl ~f:(fun _ -> assert false) ~k:2);
  assert_equal 2 (Hashtbl.length tbl);
  true
;;

module TI = Make(CCInt);;

eq  "c" (let tbl = TI.of_list [1,"a"; 2,"b"] in TI.get_or tbl 3 ~default:"c");;
eq  "b" (let tbl = TI.of_list [1,"a"; 2,"b"] in TI.get_or tbl 2 ~default:"c");;

t @@ fun () ->
  let tbl = TI.create 32 in
  TI.incr tbl 1 ;
  TI.incr tbl 2;
  TI.incr tbl 1;
  assert_equal 2 (TI.find tbl 1);
  assert_equal 1 (TI.find tbl 2);
  assert_equal 2 (TI.length tbl);
  TI.decr tbl 2;
  assert_equal 0 (TI.get_or tbl 2 ~default:0);
  assert_equal 1 (TI.length tbl);
  assert(not (TI.mem tbl 2));
  true;;
