
open CCList

let lsort l = CCList.sort Stdlib.compare l;;

q ~__FILE__ ~__LINE__
  Q.(pair small_nat (list int)) (fun (i,l) ->
  nth_opt l i = get_at_idx i l);;

q ~__FILE__ ~__LINE__
  Q.(pair (list int) (list int)) (fun (l1,l2) ->
  CCOrd.equiv (CCList.compare_lengths l1 l2)
  (CCInt.compare (length l1)(length l2)));;

q ~__FILE__ ~__LINE__
  Q.(pair (list int) small_int) (fun (l,n) ->
    CCOrd.equiv (CCList.compare_length_with l n)
  (CCInt.compare (length l) n));;

q ~__FILE__ ~__LINE__
  (Q.list Q.small_int) (fun l ->
    let f x = x+1 in
    List.rev (List.rev_map f l) = map f l);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  [1;2;3] @ [4;5;6] = [1;2;3;4;5;6];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  (1-- 10_000) @ (10_001 -- 20_000) = 1 -- 20_000;;

q ~__FILE__ ~__LINE__
  Q.(small_list int)(fun l -> List.rev l = List.fold_left cons' [] l);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  cons_maybe (Some 1) [2;3] = [1;2;3];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  cons_maybe None [2;3] = [2;3];;

eq ~__FILE__ ~__LINE__ ~printer:CCInt.to_string
  500 (filter (fun x->x mod 2 = 0) (1 -- 1000) |> List.length);;

eq ~__FILE__ ~__LINE__ ~printer:CCInt.to_string
  50_000 (filter (fun x->x mod 2 = 0) (1 -- 100_000) |> List.length);;

eq ~__FILE__ ~__LINE__ ~printer:CCInt.to_string
  500_000 (filter (fun x->x mod 2 = 0) (1 -- 1_000_000) |> List.length);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  fold_right (+) (1 -- 1_000_000) 0 =
    List.fold_left (+) 0 (1 -- 1_000_000);;

q ~__FILE__ ~__LINE__
  (Q.list Q.small_int) (fun l ->
    l = fold_right (fun x y->x::y) l []);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop)
    0 [true;true;false;true] = 2 ;;

eq ~__FILE__ ~__LINE__
  (6, ["1"; "2"; "3"])
    (fold_map (fun acc x->acc+x, string_of_int x) 0 [1;2;3]);;

q ~__FILE__ ~__LINE__
  Q.(list int) (fun l ->
    fold_map (fun acc x -> x::acc, x) [] l = (List.rev l, l));;

eq ~__FILE__ ~__LINE__
  6 (fold_on_map ~f:int_of_string ~reduce:(+) 0 ["1";"2";"3"]);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(option int)
  (Some 15) (reduce (+) [1; 2; 3; 4; 5]);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(option int)
  (Some 3) (reduce CCInt.min [5; 3; 8; 9]);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.string
  "hello world" (reduce_exn (^) ["hello"; " "; "world"]);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  try ignore (reduce_exn (+.) []); false with Invalid_argument _ -> true;;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(list int)
  [0;1;3;6] (scan_left (+) 0 [1;2;3]);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(list int)
  [0] (scan_left (+) 0 []);;

q ~__FILE__ ~__LINE__
  Q.(list int) (fun l -> 
    List.length l + 1 = List.length (scan_left (+) 0 l));;

eq ~__FILE__ ~__LINE__
  (310, ["1 10"; "2 0"; "3 100"]) 
    (fold_map2 (fun acc x y->acc+x*y, string_of_int x ^ " " ^ string_of_int y) 
    0 [1;2;3] [10;0;100]);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  (try ignore (fold_map2 (fun _ _ _ -> assert false) 42 [] [1]); false 
  with Invalid_argument _ -> true);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(pair int (list int))
  (List.fold_left (+) 0 (1--10), [2;4;6;8;10])
  (fold_filter_map (fun acc x -> acc+x, if x mod 2 = 0 then Some x else None)
  0 (1--10));;

eq ~__FILE__ ~__LINE__
  (6, ["1"; "a1"; "2"; "a2"; "3"; "a3"]) 
    (let pf = Printf.sprintf in 
    fold_flat_map (fun acc x->acc+x, [pf "%d" x; pf "a%d" x]) 0 [1;2;3]);;

q ~__FILE__ ~__LINE__
  Q.(list int) (fun l -> 
    fold_flat_map (fun acc x -> x::acc, [x;x+10]) [] l = 
      (List.rev l, flat_map (fun x->[x;x+10]) l) );;

t ~__FILE__ ~__LINE__ @@ fun () ->
  init 0 (fun _ -> 0) = [];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  init 1 (fun x->x) = [0];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  init 1000 (fun x->x) = 0--999;;

(* see: #256 *)
t ~__FILE__ ~__LINE__ @@ fun () ->
  let r = ref [] in
  ignore (CCList.init 5 (fun x -> r := x :: !r; ()));
  assert_equal ~printer:Q.Print.(list int) (List.rev !r) [0;1;2;3;4];
  true;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  let r = ref [] in
  ignore (CCList.init 200_000 (fun x -> r := x :: !r; ()));
  assert_equal ~printer:Q.Print.(list int) (List.rev !r) (0--(200_000-1));
  true ;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  equal CCInt.equal (1--1_000_000) (1--1_000_000);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  flat_map (fun x -> [x+1; x*2]) [10;100] = [11;20;101;200];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  List.length (flat_map (fun x->[x]) (1--300_000)) = 300_000;;

eq ~__FILE__ ~__LINE__
  [1;2;2;3;3;3] (flat_map_i (fun i x->replicate (i+1) x) [1;2;3]);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  flatten [[1]; [2;3;4]; []; []; [5;6]] = 1--6;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  flatten (init 300_001 (fun x->[x])) = 0--300_000;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  count (fun x -> x mod 2 = 0) [] = 0;;
t ~__FILE__ ~__LINE__ @@ fun () ->
  count (fun x -> x mod 2 = 0) [0; 0; 2; 4] = 4;;
t ~__FILE__ ~__LINE__ @@ fun () ->
  count (fun x -> x mod 2 = 0) [1; 3; 5; 7] = 0;;
t ~__FILE__ ~__LINE__ @@ fun () ->
  count (fun x -> x mod 2 = 0) [2; 6; 9; 4] = 3;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  count_true_false (fun x -> x mod 2 = 0) [] = (0, 0);;
t ~__FILE__ ~__LINE__ @@ fun () ->
  count_true_false (fun x -> x mod 2 = 0) [0; 0; 2; 4] = (4, 0);;
t ~__FILE__ ~__LINE__ @@ fun () ->
  count_true_false (fun x -> x mod 2 = 0) [1; 3; 5; 7] = (0, 4);;
t ~__FILE__ ~__LINE__ @@ fun () ->
  count_true_false (fun x -> x mod 2 = 0) [2; 6; 9; 4] = (3, 1);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  diagonal [] = [];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  diagonal [1] = [];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  diagonal [1;2] = [1,2];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  diagonal [1;2;3] |> List.sort Stdlib.compare = [1, 2; 1, 3; 2, 3];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  let l1, l2 =
    partition_map_either (function
      | n when n mod 2 = 0 -> CCEither.Left n
      | n -> CCEither.Right n
    ) [0;1;2;3;4]
  in
  assert_equal [0;2;4] l1;
  assert_equal [1;3] l2;
  true;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  let l1, l2 =
    partition_filter_map (function
      | n when n = 0 -> `Drop
      | n when n mod 2 = 0 -> `Left n
      | n -> `Right n
    ) [0;1;2;3;4]
  in
  assert_equal [2;4] l1;
  assert_equal [1;3] l2;
  true;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  try ignore (combine [1] []); false with Invalid_argument _ -> true;;
t ~__FILE__ ~__LINE__ @@ fun () ->
  try ignore (combine (1--1001) (1--1002)); false with Invalid_argument _ -> true;;
t ~__FILE__ ~__LINE__ @@ fun () ->
  combine [1;2;3] [3;2;1] = List.combine [1;2;3] [3;2;1];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  combine (1 -- 100_000) (1 -- 100_000) = List.combine (1 -- 100_000) (1 -- 100_000);;

q ~__FILE__ ~__LINE__
  Q.(let p = small_list int in pair p p)(fun (l1,l2) -> 
    if List.length l1=List.length l2 
    then CCList.combine l1 l2 = List.combine l1 l2 
    else Q.assume_fail() );;

q ~__FILE__ ~__LINE__
  Q.(let p = small_list int in pair p p)(fun (l1,l2) -> 
    let n = min (List.length l1) (List.length l2) in 
    let res1 = combine (take n l1) (take n l2) in 
    let res2 = combine_gen l1 l2 |> of_gen in 
    res1 = res2);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  (combine_shortest [] []) = [];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  (combine_shortest [1] []) = [];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  (combine_shortest [] [1]) = [];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  (combine_shortest (1--1025) (1--1026)) = List.combine (1--1025) (1--1025);;
t ~__FILE__ ~__LINE__ @@ fun () ->
  (combine_shortest (1--1026) (1--1025)) = List.combine (1--1025) (1--1025);;
t ~__FILE__ ~__LINE__ @@ fun () ->
  combine_shortest [1;2;3] [3;2;1] = List.combine [1;2;3] [3;2;1];;
t ~__FILE__ ~__LINE__ @@ fun () ->
  combine_shortest (1 -- 100_000) (1 -- 100_000) = List.combine (1 -- 100_000) (1 -- 100_000);;
t ~__FILE__ ~__LINE__ @@ fun () ->
  combine_shortest (1 -- 100_001) (1 -- 100_000) = List.combine (1 -- 100_000) (1 -- 100_000);;

q ~__FILE__ ~__LINE__
  (Q.(list_of_size Gen.(0--10_000) (pair small_int small_string))) (fun l -> 
    let (l1, l2) = split l in 
    List.length l1 = List.length l 
    && List.length l2 = List.length l);;

q ~__FILE__ ~__LINE__ 
  Q.(list_of_size Gen.(0--10_000) (pair small_int small_int)) (fun l -> 
    split l = List.split l);;

let cmp_lii_unord l1 l2 : bool =
  List.sort CCOrd.compare l1 = List.sort CCOrd.compare l2;;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(list (list int)) ~cmp:cmp_lii_unord
  [[1;3;4];[1;3;5];[1;3;6];[2;3;4];[2;3;5];[2;3;6]] 
    (cartesian_product [[1;2];[3];[4;5;6]]);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(list (list int)) ~cmp:cmp_lii_unord
  [] (cartesian_product [[1;2];[];[4;5;6]]);

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(list (list int)) ~cmp:cmp_lii_unord
  [[]] (cartesian_product []);;

eq ~__FILE__ ~__LINE__
  ~printer:Q.Print.(list (list int)) ~cmp:cmp_lii_unord
  [[1;3;4;5;6];[2;3;4;5;6]] 
    (cartesian_product [[1;2];[3];[4];[5];[6]]);;

q ~__FILE__ ~__LINE__ 
  Q.(list_of_size Gen.(1--4) (list_of_size Gen.(0--4) small_int)) (fun l-> 
    cmp_lii_unord (cartesian_product l) (map_product_l CCFun.id l));;

q ~__FILE__ ~__LINE__
  Q.(pair small_int (list small_int)) (fun (x,l) -> 
    sorted_mem ~cmp:CCInt.compare x (List.sort CCInt.compare l) = mem ~eq:CCInt.equal x l) ;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  equal CCInt.equal (List.sort CCInt.compare ([(( * )2); ((+)1)] <*> [10;100])) 
    [11; 20; 101; 200] ;;

t ~__FILE__ ~__LINE__ @@ fun () ->
  equal CCInt.equal (sorted_merge ~cmp:CCInt.compare [1;1;2] [1;2;3]) [1;1;1;2;2;3] ;;

q ~__FILE__ ~__LINE__
  Q.(pair (list int) (list int)) (fun (l1,l2) -> 
    List.length (sorted_merge ~cmp:CCInt.compare l1 l2) = List.length l1 + List.length l2);;

t ~__FILE__ ~__LINE__ @@ fun () ->
  equal CCInt.equal (sorted_diff ~cmp:CCInt.compare [0;1;1;2;4] [1;2;2;2;3]) [0;1;4];;

t ~__FILE__ ~__LINE__ @@ fun () ->
  equal CCInt.equal (sorted_diff ~cmp:CCInt.compare [2] [1;2;2;2;3]) [];;
