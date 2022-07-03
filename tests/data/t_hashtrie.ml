

module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCHashTrie;;


module M = Make(CCInt) ;;

let _listuniq =
  let g = Q.(list (pair small_int small_int)) in
  Q.map_same_type
    (fun l ->
      CCList.sort_uniq ~cmp:(fun a b -> Stdlib.compare (fst a)(fst b)) l
    ) g
;;

t @@ fun () -> M.is_empty M.empty;;

t @@ fun () -> not (M.is_empty (M.singleton 1 2));;
t @@ fun () -> M.cardinal (M.singleton 1 2) = 1;;
t @@ fun () -> popcount 5L = 2;;
t @@ fun () -> popcount 256L = 1;;
t @@ fun () -> popcount 255L = 8;;
t @@ fun () -> popcount 0xFFFFL = 16;;
t @@ fun () -> popcount 0xFF1FL = 13;;
t @@ fun () -> popcount 0xFFFFFFFFL = 32;;
t @@ fun () -> popcount 0xFFFFFFFFFFFFFFFFL = 64;;

q Q.int (fun i -> let i = Int64.of_int i in popcount i <= 64);;

q  _listuniq (fun l ->
    let m = M.of_list l in
    List.for_all (fun (x,y) -> M.get_exn x m = y) l);;

q  _listuniq (fun l ->
      let m = List.fold_left (fun m (x,y) -> M.add x y m) M.empty l in
      List.for_all (fun (x,y) -> M.get_exn x m = y) l);;

t @@ fun () ->
  let lsort = List.sort Stdlib.compare in
  let m = M.of_list [1, 1; 2, 2] in
  let id = Transient.create() in
  let m' = M.add_mut ~id 3 3 m in
  let m' = M.add_mut ~id 4 4 m' in
  assert_equal [1, 1; 2, 2] (M.to_list m |> lsort);
  assert_equal [1, 1; 2, 2; 3,3; 4,4] (M.to_list m' |> lsort);
  Transient.freeze id;
  assert_bool "must raise"
    (try ignore(M.add_mut ~id 5 5 m'); false with Transient.Frozen -> true);
  true;;


q _listuniq (fun l ->
    let m = M.of_list l in
    List.for_all
      (fun (x,_) ->
        let m' = M.remove x m in
        not (M.mem x m') &&
        M.cardinal m' = M.cardinal m - 1 &&
        List.for_all
          (fun (y,v) -> y = x || M.get_exn y m' = v)
          l
    ) l
);;

t @@ fun () ->
  let m = M.of_list [1, 1; 2, 2; 5, 5] in
  let m' = M.update 4
    ~f:(function
    | None -> Some 4
    | Some _ -> Some 0
    ) m
  in
  assert_equal [1,1; 2,2; 4,4; 5,5] (M.to_list m' |> List.sort Stdlib.compare);
  true;;

t @@ fun () ->
  let l = CCList.(1 -- 10 |> map (fun x->x,x)) in
  M.of_list l
    |> M.fold ~f:(fun acc x y -> (x,y)::acc) ~x:[]
    |> List.sort Stdlib.compare = l ;;

q _listuniq (fun l ->
    (List.sort Stdlib.compare l) =
      (l |> Iter.of_list |> M.of_iter |> M.to_iter |> Iter.to_list
    |> List.sort Stdlib.compare) );;

q _listuniq (fun l ->
    (List.sort Stdlib.compare l) =
      (l |> Gen.of_list |> M.of_gen |> M.to_gen |> Gen.to_list
    |> List.sort Stdlib.compare) );;

t @@ fun () -> M.choose M.empty = None;;
t @@ fun () -> M.choose M.(of_list [1,1; 2,2]) <> None;;

t @@ fun () ->
  let m = M.of_list CCList.( (501 -- 1000) @ (500 -- 1) |> map (fun i->i,i)) in
  assert_equal ~printer:CCInt.to_string 1000 (M.cardinal m);
  assert_bool "check all get"
    (Iter.for_all (fun i -> i = M.get_exn i m) Iter.(1 -- 1000));
  let m = Iter.(501 -- 1000 |> fold (fun m i -> M.remove i m) m) in
  assert_equal ~printer:CCInt.to_string 500 (M.cardinal m);
  assert_bool "check all get after remove"
    (Iter.for_all (fun i -> i = M.get_exn i m) Iter.(1 -- 500));
  assert_bool "check all get after remove"
    (Iter.for_all (fun i -> None = M.get i m) Iter.(501 -- 1000));
  true;;
