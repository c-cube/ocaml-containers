module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCFun_vec

let _listuniq =
  let g = Q.(small_list (pair small_int small_int)) in
  Q.map_same_type
    (fun l ->
      CCList.sort_uniq ~cmp:(fun a b -> Stdlib.compare (fst a) (fst b)) l)
    g
;;

t @@ fun () -> is_empty empty;;
t @@ fun () -> not (is_empty (return 2));;
t @@ fun () -> length (return 2) = 1;;

q _listuniq (fun l ->
    let m = of_list l in
    List.for_all (fun (i, y) -> get_exn i m = y) @@ List.mapi CCPair.make l)
;;

(* regression test for #298 *)
t @@ fun () ->
let rec consume x =
  match CCFun_vec.pop x with
  | None -> ()
  | Some (_, x) -> consume x
in
consume (of_list CCList.(1 -- 100));
true
;;

q
  Q.(pair int (small_list int))
  (fun (x, l) ->
    let q0 = of_list l in
    let q = push x q0 in
    assert_equal (length q) (length q0 + 1);
    let y, q = pop_exn q in
    assert_equal x y;
    assert_equal (to_list q) (to_list q0);
    true)
;;

q
  Q.(pair (fun1 Observable.int bool) (small_list int))
  (fun (f, l) ->
    let f = Q.Fn.apply f in
    List.map f l = (of_list l |> map f |> to_list))
;;

q
  Q.(pair (small_list int) (small_list int))
  (fun (l1, l2) -> l1 @ l2 = (append (of_list l1) (of_list l2) |> to_list))
;;

q Q.(small_list int) (fun l -> l = to_list (of_list l));;

q _listuniq (fun l ->
    List.sort Stdlib.compare l
    = (l |> Iter.of_list |> of_iter |> to_iter |> Iter.to_list
     |> List.sort Stdlib.compare))
;;

q _listuniq (fun l ->
    List.sort Stdlib.compare l
    = (l |> Gen.of_list |> of_gen |> to_gen |> Gen.to_list
     |> List.sort Stdlib.compare))
;;

t @@ fun () -> choose empty = None;;
t @@ fun () -> choose (of_list [ 1, 1; 2, 2 ]) <> None
