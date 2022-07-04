module Test = (val Containers_testlib.make ~__FILE__ ())
open Test

module Mixmap = struct
  t @@ fun () ->
  let module M = CCMixmap.Make (CCInt) in
  let inj_int = CCMixmap.create_inj () in
  let inj_str = CCMixmap.create_inj () in
  let inj_list_int = CCMixmap.create_inj () in

  let m =
    M.empty |> M.add ~inj:inj_int 1 1 |> M.add ~inj:inj_str 2 "2"
    |> M.add ~inj:inj_list_int 3 [ 3; 3; 3 ]
  in

  assert_equal (M.get ~inj:inj_int 1 m) (Some 1);
  assert_equal (M.get ~inj:inj_str 1 m) None;
  assert_equal (M.get ~inj:inj_str 2 m) (Some "2");
  assert_equal (M.get ~inj:inj_int 2 m) None;
  assert_equal (M.get ~inj:inj_list_int 3 m) (Some [ 3; 3; 3 ]);
  assert_equal (M.get ~inj:inj_str 3 m) None;
  true
end

module Mixset = struct
  open CCMixset;;

  t @@ fun () ->
  let k1 : int key = newkey () in
  let k2 : int key = newkey () in
  let k3 : string key = newkey () in
  let set = empty |> set ~key:k1 1 |> set ~key:k2 2 |> set ~key:k3 "3" in
  assert (get ~key:k1 set = Some 1);
  assert (get ~key:k2 set = Some 2);
  assert (get ~key:k3 set = Some "3");
  true
end

module Mixtbl = struct
  open CCFun
  open CCMixtbl;;

  t @@ fun () ->
  let inj_int = create_inj () in
  let tbl = create 10 in
  assert_equal None (get ~inj:inj_int tbl "a");
  set ~inj:inj_int tbl "a" 1;
  assert_equal (Some 1) (get ~inj:inj_int tbl "a");
  let inj_string = create_inj () in
  set ~inj:inj_string tbl "b" "Hello";
  assert_equal (Some "Hello") (get ~inj:inj_string tbl "b");
  assert_equal None (get ~inj:inj_string tbl "a");
  assert_equal (Some 1) (get ~inj:inj_int tbl "a");
  set ~inj:inj_string tbl "a" "Bye";
  assert_equal None (get ~inj:inj_int tbl "a");
  assert_equal (Some "Bye") (get ~inj:inj_string tbl "a");
  true
  ;;

  t @@ fun () ->
  let inj_int = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  assert_equal 2 (length tbl);
  assert_equal 2 (find ~inj:inj_int tbl "bar");
  set ~inj:inj_int tbl "foo" 42;
  assert_equal 2 (length tbl);
  remove tbl "bar";
  assert_equal 1 (length tbl);
  true
  ;;

  t @@ fun () ->
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  assert_equal 3 (length tbl);
  clear tbl;
  assert_equal 0 (length tbl);
  true
  ;;

  t @@ fun () ->
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  assert_bool "mem foo int" (mem ~inj:inj_int tbl "foo");
  assert_bool "mem bar int" (mem ~inj:inj_int tbl "bar");
  assert_bool "not mem baaz int" (not (mem ~inj:inj_int tbl "baaz"));
  assert_bool "not mem foo str" (not (mem ~inj:inj_str tbl "foo"));
  assert_bool "not mem bar str" (not (mem ~inj:inj_str tbl "bar"));
  assert_bool "mem baaz str" (mem ~inj:inj_str tbl "baaz");
  true
  ;;

  t @@ fun () ->
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  let l = keys_iter tbl |> Iter.to_list in
  assert_equal [ "baaz"; "bar"; "foo" ] (List.sort compare l);
  true
  ;;

  t @@ fun () ->
  let inj_int = create_inj () in
  let inj_str = create_inj () in
  let tbl = create 5 in
  set ~inj:inj_int tbl "foo" 1;
  set ~inj:inj_int tbl "bar" 2;
  set ~inj:inj_str tbl "baaz" "hello";
  set ~inj:inj_str tbl "str" "rts";
  let l_int = bindings_of ~inj:inj_int tbl |> Iter.to_list in
  assert_equal [ "bar", 2; "foo", 1 ] (List.sort compare l_int);
  let l_str = bindings_of ~inj:inj_str tbl |> Iter.to_list in
  assert_equal [ "baaz", "hello"; "str", "rts" ] (List.sort compare l_str);
  true
end

module Multiset = struct
  t @@ fun () ->
  let module S = CCMultiSet.Make (String) in
  S.count (S.add_mult S.empty "a" 5) "a" = 5
  ;;

  t @@ fun () ->
  let module S = CCMultiSet.Make (String) in
  S.count (S.remove_mult (S.add_mult S.empty "a" 5) "a" 3) "a" = 2
  ;;

  t @@ fun () ->
  let module S = CCMultiSet.Make (String) in
  S.count (S.remove_mult (S.add_mult S.empty "a" 4) "a" 6) "a" = 0
end

module PersistentArray = struct
  open CCPersistentArray;;

  t @@ fun () ->
  of_list
    [
      of_list [ 1 ];
      of_list [];
      of_list [ 2; 3; 4 ];
      of_list [ 5 ];
      of_list [ 6; 7 ];
    ]
  |> flatten |> to_list = [ 1; 2; 3; 4; 5; 6; 7 ]
  ;;

  t @@ fun () ->
  of_list [ of_list []; of_list []; of_list [] ] |> flatten |> length = 0
  ;;

  t @@ fun () -> of_list [] |> flatten |> length = 0;;
  q Q.(list int) (fun l -> of_list l |> to_gen |> of_gen |> to_list = l)
end
