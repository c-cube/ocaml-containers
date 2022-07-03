
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCGraph;;

t @@ fun () ->
  let l =
    let tbl = mk_table ~eq:CCInt.equal 128 in
    Traverse.Event.dfs ~tbl ~eq:CCInt.equal ~graph:divisors_graph (Iter.return 345614)
    |> Iter.to_list in
  let expected =
  [`Enter (345614, 0, []); `Edge (345614, (), 172807, `Forward);
   `Enter (172807, 1, [(345614, (), 172807)]); `Edge (172807, (), 1, `Forward);
   `Enter (1, 2, [(172807, (), 1); (345614, (), 172807)]); `Exit 1; `Exit 172807;
   `Edge (345614, (), 2, `Forward); `Enter (2, 3, [(345614, (), 2)]);
   `Edge (2, (), 1, `Cross); `Exit 2; `Edge (345614, (), 1, `Cross);
   `Exit 345614]
  in
  assert_equal expected l;
  true;;

t @@ fun () ->
  let tbl = mk_table ~eq:CCInt.equal 128 in
  let l = topo_sort ~eq:CCInt.equal ~tbl ~graph:divisors_graph (Iter.return 42) in
  List.for_all (fun (i,j) ->
    let idx_i = CCList.find_idx ((=)i) l |> CCOption.get_exn_or "" |> fst in
    let idx_j = CCList.find_idx ((=)j) l |> CCOption.get_exn_or "" |> fst in
    idx_i < idx_j)
  [ 42, 21; 14, 2; 3, 1; 21, 7; 42, 3];;

t @@ fun () ->
  let tbl = mk_table ~eq:CCInt.equal 128 in
  let l = topo_sort ~eq:CCInt.equal ~rev:true ~tbl ~graph:divisors_graph (Iter.return 42) in
  List.for_all (fun (i,j) ->
    let idx_i = CCList.find_idx ((=)i) l |> CCOption.get_exn_or "" |> fst in
    let idx_j = CCList.find_idx ((=)j) l |> CCOption.get_exn_or "" |> fst in
    idx_i > idx_j)
    [ 42, 21; 14, 2; 3, 1; 21, 7; 42, 3];;

(* example from https://en.wikipedia.org/wiki/Strongly_connected_component *)
t @@ fun () ->
  let set_eq ?(eq=(=)) l1 l2 = CCList.subset ~eq l1 l2 && CCList.subset ~eq l2 l1 in
  let graph = of_list ~eq:CCString.equal
    [ "a", "b"
    ; "b", "e"
    ; "e", "a"
    ; "b", "f"
    ; "e", "f"
    ; "f", "g"
    ; "g", "f"
    ; "b", "c"
    ; "c", "g"
    ; "c", "d"
    ; "d", "c"
    ; "d", "h"
    ; "h", "d"
    ; "h", "g"
  ] in
  let tbl = mk_table ~eq:CCString.equal 128 in
  let res = scc ~tbl ~graph (Iter.return "a") |> Iter.to_list in
  assert_bool "scc"
    (set_eq ~eq:(set_eq ?eq:None) res
      [ [ "a"; "b"; "e" ]
      ; [ "f"; "g" ]
      ; [ "c"; "d"; "h" ]
      ]
    );
  true;;
