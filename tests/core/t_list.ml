open CCList
module T = (val Containers_testlib.make ~__FILE__ ())
include T

let lsort l = CCList.sort Stdlib.compare l;;

q Q.(pair small_nat (list int)) (fun (i, l) -> nth_opt l i = get_at_idx i l);;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    CCOrd.equiv
      (CCList.compare_lengths l1 l2)
      (CCInt.compare (length l1) (length l2)))
;;

q
  Q.(pair (list int) small_int)
  (fun (l, n) ->
    CCOrd.equiv (CCList.compare_length_with l n) (CCInt.compare (length l) n))
;;

q (Q.list Q.small_int) (fun l ->
    let f x = x + 1 in
    List.rev (List.rev_map f l) = map f l)
;;

t @@ fun () -> [ 1; 2; 3 ] @ [ 4; 5; 6 ] = [ 1; 2; 3; 4; 5; 6 ];;
t @@ fun () -> (1 -- 10_000) @ (10_001 -- 20_000) = 1 -- 20_000;;
q Q.(small_list int) (fun l -> List.rev l = List.fold_left cons' [] l);;
t @@ fun () -> cons_maybe (Some 1) [ 2; 3 ] = [ 1; 2; 3 ];;
t @@ fun () -> cons_maybe None [ 2; 3 ] = [ 2; 3 ];;

eq ~printer:CCInt.to_string 500
  (filter (fun x -> x mod 2 = 0) (1 -- 1000) |> List.length)
;;

eq ~printer:CCInt.to_string 50_000
  (filter (fun x -> x mod 2 = 0) (1 -- 100_000) |> List.length)
;;

eq ~printer:CCInt.to_string 500_000
  (filter (fun x -> x mod 2 = 0) (1 -- 1_000_000) |> List.length)
;;

t @@ fun () ->
fold_right ( + ) (1 -- 1_000_000) 0 = List.fold_left ( + ) 0 (1 -- 1_000_000)
;;

q (Q.list Q.small_int) (fun l -> l = fold_right (fun x y -> x :: y) l []);;

t @@ fun () ->
fold_while
  (fun acc b ->
    if b then
      acc + 1, `Continue
    else
      acc, `Stop)
  0
  [ true; true; false; true ]
= 2
;;

eq
  (6, [ "1"; "2"; "3" ])
  (fold_map (fun acc x -> acc + x, string_of_int x) 0 [ 1; 2; 3 ])
;;

q
  Q.(list int)
  (fun l -> fold_map (fun acc x -> x :: acc, x) [] l = (List.rev l, l))
;;

eq 6 (fold_on_map ~f:int_of_string ~reduce:( + ) 0 [ "1"; "2"; "3" ]);;
eq ~printer:Q.Print.(option int) (Some 15) (reduce ( + ) [ 1; 2; 3; 4; 5 ]);;
eq ~printer:Q.Print.(option int) (Some 3) (reduce CCInt.min [ 5; 3; 8; 9 ]);;

eq ~printer:Q.Print.string "hello world"
  (reduce_exn ( ^ ) [ "hello"; " "; "world" ])
;;

t @@ fun () ->
try
  ignore (reduce_exn ( +. ) []);
  false
with Invalid_argument _ -> true
;;

eq ~printer:Q.Print.(list int) [ 0; 1; 3; 6 ] (scan_left ( + ) 0 [ 1; 2; 3 ]);;
eq ~printer:Q.Print.(list int) [ 0 ] (scan_left ( + ) 0 []);;
q Q.(list int) (fun l -> List.length l + 1 = List.length (scan_left ( + ) 0 l))
;;

eq
  (310, [ "1 10"; "2 0"; "3 100" ])
  (fold_map2
     (fun acc x y -> acc + (x * y), string_of_int x ^ " " ^ string_of_int y)
     0 [ 1; 2; 3 ] [ 10; 0; 100 ])
;;

t @@ fun () ->
try
  ignore (fold_map2 (fun _ _ _ -> assert false) 42 [] [ 1 ]);
  false
with Invalid_argument _ -> true
;;

eq
  ~printer:Q.Print.(pair int (list int))
  (List.fold_left ( + ) 0 (1 -- 10), [ 2; 4; 6; 8; 10 ])
  (fold_filter_map
     (fun acc x ->
       ( acc + x,
         if x mod 2 = 0 then
           Some x
         else
           None ))
     0 (1 -- 10))
;;

eq
  (6, [ "1"; "a1"; "2"; "a2"; "3"; "a3" ])
  (let pf = Printf.sprintf in
   fold_flat_map (fun acc x -> acc + x, [ pf "%d" x; pf "a%d" x ]) 0 [ 1; 2; 3 ])
;;

q
  Q.(list int)
  (fun l ->
    fold_flat_map (fun acc x -> x :: acc, [ x; x + 10 ]) [] l
    = (List.rev l, flat_map (fun x -> [ x; x + 10 ]) l))
;;

t @@ fun () -> init 0 (fun _ -> 0) = [];;
t @@ fun () -> init 1 (fun x -> x) = [ 0 ];;
t @@ fun () -> init 1000 (fun x -> x) = 0 -- 999;;

(* see: #256 *)
t @@ fun () ->
let r = ref [] in
ignore
  (CCList.init 5 (fun x ->
       r := x :: !r;
       ()));
assert_equal ~printer:Q.Print.(list int) (List.rev !r) [ 0; 1; 2; 3; 4 ];
true
;;

t @@ fun () ->
let r = ref [] in
ignore
  (CCList.init 200_000 (fun x ->
       r := x :: !r;
       ()));
assert_equal ~printer:Q.Print.(list int) (List.rev !r) (0 -- (200_000 - 1));
true
;;

t @@ fun () -> equal CCInt.equal (1 -- 1_000_000) (1 -- 1_000_000);;

t @@ fun () ->
flat_map (fun x -> [ x + 1; x * 2 ]) [ 10; 100 ] = [ 11; 20; 101; 200 ]
;;

t @@ fun () -> List.length (flat_map (fun x -> [ x ]) (1 -- 300_000)) = 300_000
;;

eq [ 1; 2; 2; 3; 3; 3 ]
  (flat_map_i (fun i x -> replicate (i + 1) x) [ 1; 2; 3 ])
;;

t @@ fun () -> flatten [ [ 1 ]; [ 2; 3; 4 ]; []; []; [ 5; 6 ] ] = 1 -- 6;;
t @@ fun () -> flatten (init 300_001 (fun x -> [ x ])) = 0 -- 300_000;;
t @@ fun () -> count (fun x -> x mod 2 = 0) [] = 0;;
t @@ fun () -> count (fun x -> x mod 2 = 0) [ 0; 0; 2; 4 ] = 4;;
t @@ fun () -> count (fun x -> x mod 2 = 0) [ 1; 3; 5; 7 ] = 0;;
t @@ fun () -> count (fun x -> x mod 2 = 0) [ 2; 6; 9; 4 ] = 3;;
t @@ fun () -> count_true_false (fun x -> x mod 2 = 0) [] = (0, 0);;
t @@ fun () -> count_true_false (fun x -> x mod 2 = 0) [ 0; 0; 2; 4 ] = (4, 0);;
t @@ fun () -> count_true_false (fun x -> x mod 2 = 0) [ 1; 3; 5; 7 ] = (0, 4);;
t @@ fun () -> count_true_false (fun x -> x mod 2 = 0) [ 2; 6; 9; 4 ] = (3, 1);;
t @@ fun () -> diagonal [] = [];;
t @@ fun () -> diagonal [ 1 ] = [];;
t @@ fun () -> diagonal [ 1; 2 ] = [ 1, 2 ];;

t @@ fun () ->
diagonal [ 1; 2; 3 ] |> List.sort Stdlib.compare = [ 1, 2; 1, 3; 2, 3 ]
;;

t @@ fun () ->
let l1, l2 =
  partition_map_either
    (function
      | n when n mod 2 = 0 -> CCEither.Left n
      | n -> CCEither.Right n)
    [ 0; 1; 2; 3; 4 ]
in
assert_equal [ 0; 2; 4 ] l1;
assert_equal [ 1; 3 ] l2;
true
;;

t @@ fun () ->
let l1, l2 =
  partition_filter_map
    (function
      | n when n = 0 -> `Drop
      | n when n mod 2 = 0 -> `Left n
      | n -> `Right n)
    [ 0; 1; 2; 3; 4 ]
in
assert_equal [ 2; 4 ] l1;
assert_equal [ 1; 3 ] l2;
true
;;

t @@ fun () ->
try
  ignore (combine [ 1 ] []);
  false
with Invalid_argument _ -> true
;;

t @@ fun () ->
try
  ignore (combine (1 -- 1001) (1 -- 1002));
  false
with Invalid_argument _ -> true
;;

t @@ fun () ->
combine [ 1; 2; 3 ] [ 3; 2; 1 ] = List.combine [ 1; 2; 3 ] [ 3; 2; 1 ]
;;

t @@ fun () ->
combine (1 -- 100_000) (1 -- 100_000)
= List.combine (1 -- 100_000) (1 -- 100_000)
;;

q
  Q.(
    let p = small_list int in
    pair p p)
  (fun (l1, l2) ->
    if List.length l1 = List.length l2 then
      CCList.combine l1 l2 = List.combine l1 l2
    else
      Q.assume_fail ())
;;

q
  Q.(
    let p = small_list int in
    pair p p)
  (fun (l1, l2) ->
    let n = min (List.length l1) (List.length l2) in
    let res1 = combine (take n l1) (take n l2) in
    let res2 = combine_gen l1 l2 |> of_gen in
    res1 = res2)
;;

t @@ fun () -> combine_shortest [] [] = [];;
t @@ fun () -> combine_shortest [ 1 ] [] = [];;
t @@ fun () -> combine_shortest [] [ 1 ] = [];;

t @@ fun () ->
combine_shortest (1 -- 1025) (1 -- 1026) = List.combine (1 -- 1025) (1 -- 1025)
;;

t @@ fun () ->
combine_shortest (1 -- 1026) (1 -- 1025) = List.combine (1 -- 1025) (1 -- 1025)
;;

t @@ fun () ->
combine_shortest [ 1; 2; 3 ] [ 3; 2; 1 ] = List.combine [ 1; 2; 3 ] [ 3; 2; 1 ]
;;

t @@ fun () ->
combine_shortest (1 -- 100_000) (1 -- 100_000)
= List.combine (1 -- 100_000) (1 -- 100_000)
;;

t @@ fun () ->
combine_shortest (1 -- 100_001) (1 -- 100_000)
= List.combine (1 -- 100_000) (1 -- 100_000)
;;

q
  Q.(list_of_size Gen.(0 -- 10_000) (pair small_int small_string))
  (fun l ->
    let l1, l2 = split l in
    List.length l1 = List.length l && List.length l2 = List.length l)
;;

q
  Q.(list_of_size Gen.(0 -- 10_000) (pair small_int small_int))
  (fun l -> split l = List.split l)

let cmp_lii_unord l1 l2 : bool =
  List.sort CCOrd.poly l1 = List.sort CCOrd.poly l2
;;

eq
  ~printer:Q.Print.(list (list int))
  ~cmp:cmp_lii_unord
  [
    [ 1; 3; 4 ]; [ 1; 3; 5 ]; [ 1; 3; 6 ]; [ 2; 3; 4 ]; [ 2; 3; 5 ]; [ 2; 3; 6 ];
  ]
  (cartesian_product [ [ 1; 2 ]; [ 3 ]; [ 4; 5; 6 ] ])
;;

eq
  ~printer:Q.Print.(list (list int))
  ~cmp:cmp_lii_unord []
  (cartesian_product [ [ 1; 2 ]; []; [ 4; 5; 6 ] ]);

eq
  ~printer:Q.Print.(list (list int))
  ~cmp:cmp_lii_unord [ [] ] (cartesian_product [])
;;

eq
  ~printer:Q.Print.(list (list int))
  ~cmp:cmp_lii_unord
  [ [ 1; 3; 4; 5; 6 ]; [ 2; 3; 4; 5; 6 ] ]
  (cartesian_product [ [ 1; 2 ]; [ 3 ]; [ 4 ]; [ 5 ]; [ 6 ] ])
;;

q
  Q.(list_of_size Gen.(1 -- 4) (list_of_size Gen.(0 -- 4) small_int))
  (fun l -> cmp_lii_unord (cartesian_product l) (map_product_l CCFun.id l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    sorted_mem ~cmp:CCInt.compare x (List.sort CCInt.compare l)
    = mem ~eq:CCInt.equal x l)
;;

t @@ fun () ->
equal CCInt.equal
  (List.sort CCInt.compare ([ ( * ) 2; ( + ) 1 ] <*> [ 10; 100 ]))
  [ 11; 20; 101; 200 ]
;;

t @@ fun () ->
equal CCInt.equal
  (sorted_merge ~cmp:CCInt.compare [ 1; 1; 2 ] [ 1; 2; 3 ])
  [ 1; 1; 1; 2; 2; 3 ]
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    List.length (sorted_merge ~cmp:CCInt.compare l1 l2)
    = List.length l1 + List.length l2)
;;

t @@ fun () ->
equal CCInt.equal
  (sorted_diff ~cmp:CCInt.compare [ 0; 1; 1; 2; 4 ] [ 1; 2; 2; 2; 3 ])
  [ 0; 1; 4 ]
;;

t @@ fun () ->
equal CCInt.equal (sorted_diff ~cmp:CCInt.compare [ 2 ] [ 1; 2; 2; 2; 3 ]) []
;;

q
  Q.(pair (list small_int) (list small_int))
  (fun (l1, l2) ->
    List.length (sorted_merge ~cmp:CCInt.compare l1 l2)
    = List.length l1 + List.length l2)
;;

q
  Q.(pair (list small_int) (list small_int))
  (fun (l1, l2) ->
    let l =
      sorted_diff ~cmp:CCInt.compare
        (List.sort CCInt.compare l1)
        (List.sort CCInt.compare l2)
    in
    l = sort CCInt.compare l)
(* [is_sorted] is after this function *)
;;

q
  Q.(triple small_nat small_nat int)
  (fun (n1, n2, x) ->
    let l =
      sorted_diff ~cmp:CCInt.compare
        (CCList.init n1 (fun _ -> x))
        (CCList.init n2 (fun _ -> x))
    in
    count (CCInt.equal x) l = CCInt.max (n1 - n2) 0)
;;

q
  Q.(pair (list small_int) (list small_int))
  (fun (l1, l2) ->
    let l1 = List.sort CCInt.compare l1 in
    let l2 = List.sort CCInt.compare l2 in
    l1
    = sorted_diff ~cmp:CCInt.compare (sorted_merge ~cmp:CCInt.compare l1 l2) l2)
;;

t @@ fun () ->
sort_uniq ~cmp:CCInt.compare [ 1; 2; 5; 3; 6; 1; 4; 2; 3 ]
= [ 1; 2; 3; 4; 5; 6 ]
;;

t @@ fun () -> sort_uniq ~cmp:CCInt.compare [] = [];;

t @@ fun () ->
sort_uniq ~cmp:CCInt.compare [ 10; 10; 10; 10; 1; 10 ] = [ 1; 10 ]
;;

q
  Q.(list small_int)
  (fun l -> is_sorted ~cmp:CCInt.compare (List.sort Stdlib.compare l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    is_sorted ~cmp:CCInt.compare (sorted_insert ~cmp:CCInt.compare x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    is_sorted ~cmp:CCInt.compare
      (sorted_insert ~cmp:CCInt.compare ~uniq:true x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    is_sorted ~cmp:CCInt.compare
      (sorted_insert ~cmp:CCInt.compare ~uniq:false x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    let l' = sorted_insert ~cmp:CCInt.compare ~uniq:false x l in
    List.length l' = List.length l + 1)
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    List.mem x (sorted_insert ~cmp:CCInt.compare x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    is_sorted ~cmp:CCInt.compare (sorted_remove ~cmp:CCInt.compare x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    is_sorted ~cmp:CCInt.compare
      (sorted_remove ~cmp:CCInt.compare ~all:false x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    is_sorted ~cmp:CCInt.compare
      (sorted_remove ~cmp:CCInt.compare ~all:true x l))
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    let l' = sorted_remove ~cmp:CCInt.compare x l in
    List.length l'
    = List.length l
      -
      if List.mem x l then
        1
      else
        0)
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    let l' = sorted_remove ~cmp:CCInt.compare ~all:true x l in
    List.length l' = List.length l - count (CCInt.equal x) l)
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    let l' = sorted_remove ~cmp:CCInt.compare ~all:false x l in
    List.length l'
    = List.length l
      -
      if List.mem x l then
        1
      else
        0)
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    let l' = sorted_remove ~cmp:CCInt.compare x l in
    count (CCInt.equal x) l'
    = count (CCInt.equal x) l
      -
      if List.mem x l then
        1
      else
        0)
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    let l' = sorted_remove ~cmp:CCInt.compare ~all:false x l in
    count (CCInt.equal x) l'
    = count (CCInt.equal x) l
      -
      if List.mem x l then
        1
      else
        0)
;;

q
  Q.(pair small_int (list small_int))
  (fun (x, l) ->
    let l = List.sort Stdlib.compare l in
    not (List.mem x (sorted_remove ~cmp:CCInt.compare ~all:true x l)))
;;

t @@ fun () ->
uniq_succ ~eq:CCInt.equal [ 1; 1; 2; 3; 1; 6; 6; 4; 6; 1 ]
= [ 1; 2; 3; 1; 6; 4; 6; 1 ]
;;

t @@ fun () ->
group_succ ~eq:CCInt.equal [ 1; 2; 3; 1; 1; 2; 4 ]
= [ [ 1 ]; [ 2 ]; [ 3 ]; [ 1; 1 ]; [ 2 ]; [ 4 ] ]
;;

t @@ fun () -> group_succ ~eq:CCInt.equal [] = [];;
t @@ fun () -> group_succ ~eq:CCInt.equal [ 1; 1; 1 ] = [ [ 1; 1; 1 ] ];;

t @@ fun () ->
group_succ ~eq:CCInt.equal [ 1; 2; 2; 2 ] = [ [ 1 ]; [ 2; 2; 2 ] ]
;;

t @@ fun () ->
group_succ ~eq:(fun (x, _) (y, _) -> x = y) [ 1, 1; 1, 2; 1, 3; 2, 0 ]
= [ [ 1, 1; 1, 2; 1, 3 ]; [ 2, 0 ] ]
;;

t @@ fun () ->
sorted_merge_uniq ~cmp:CCInt.compare [ 1; 1; 2; 3; 5; 8 ]
  [ 1; 2; 3; 4; 6; 8; 9; 9 ]
= [ 1; 2; 3; 4; 5; 6; 8; 9 ]
;;

q
  Q.(list int)
  (fun l ->
    let l = List.sort Stdlib.compare l in
    sorted_merge_uniq ~cmp:CCInt.compare l [] = uniq_succ ~eq:CCInt.equal l)
;;

q
  Q.(list int)
  (fun l ->
    let l = List.sort Stdlib.compare l in
    sorted_merge_uniq ~cmp:CCInt.compare [] l = uniq_succ ~eq:CCInt.equal l)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    let l1 = List.sort Stdlib.compare l1 and l2 = List.sort Stdlib.compare l2 in
    let l3 = sorted_merge_uniq ~cmp:CCInt.compare l1 l2 in
    uniq_succ ~eq:CCInt.equal l3 = l3)
;;

t @@ fun () ->
sorted_diff_uniq ~cmp:CCInt.compare
  [ 1; 1; 1; 2; 2; 3; 5; 8; 8; 8 ]
  [ 1; 2; 2; 2; 2; 8; 13; 13; 13 ]
= [ 1; 3; 5; 8 ]
;;

q
  Q.(pair (list small_int) (list small_int))
  (fun (l1, l2) ->
    let l1 = List.sort CCInt.compare l1 in
    let l2 = List.sort CCInt.compare l2 in
    is_sorted ~cmp:CCInt.compare (sorted_diff_uniq ~cmp:CCInt.compare l1 l2))
;;

q
  Q.(pair (list small_int) (list small_int))
  (fun (l1, l2) ->
    let l1 = List.sort CCInt.compare l1 in
    let l2 = List.sort CCInt.compare l2 in
    sorted_diff_uniq ~cmp:CCInt.compare l1 l2
    = uniq_succ ~eq:CCInt.equal (sorted_diff ~cmp:CCInt.compare l1 l2))
;;

t @@ fun () -> take 2 [ 1; 2; 3; 4; 5 ] = [ 1; 2 ];;
t @@ fun () -> take 10_000 (range 0 100_000) |> List.length = 10_000;;
t @@ fun () -> take 10_000 (range 0 2_000) = range 0 2_000;;
t @@ fun () -> take 300_000 (1 -- 400_000) = 1 -- 300_000;;

q
  (Q.pair (Q.list Q.small_int) Q.int)
  (fun (l, i) ->
    let i = abs i in
    let l1 = take i l in
    List.length l1 <= i && List.length l1 = i = (List.length l >= i))
;;

t @@ fun () ->
try
  ignore (hd_tl []);
  false
with Failure _ -> true
;;

t @@ fun () -> hd_tl [ 1; 2; 3 ] = (1, [ 2; 3 ]);;

q
  (Q.pair (Q.list Q.small_int) Q.int)
  (fun (l, i) ->
    let i = abs i in
    let l1, l2 = take_drop i l in
    l1 @ l2 = l)

let subs = sublists_of_len;;

eq ~printer:Q.Print.(list (list int)) [ [ 1; 2; 3 ] ] (subs 3 [ 1; 2; 3; 4 ]);;

eq
  ~printer:Q.Print.(list (list int))
  [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]
  (subs 2 [ 1; 2; 3; 4; 5; 6 ])
;;

eq ~printer:Q.Print.(list (list int)) [] (subs 3 [ 1; 2 ]);;

eq
  ~printer:Q.Print.(list (list int))
  [ [ 1; 2 ]; [ 3; 4 ] ]
  (subs ~offset:2 2 [ 1; 2; 3; 4 ])
;;

eq
  ~printer:Q.Print.(list (list int))
  [ [ 1; 2 ]; [ 2; 3 ] ]
  (subs ~offset:1 2 [ 1; 2; 3 ])
;;

eq
  ~printer:Q.Print.(list (list int))
  [ [ 1; 2 ]; [ 4; 5 ] ]
  (subs ~offset:3 2 [ 1; 2; 3; 4; 5; 6 ])
;;

eq
  ~printer:Q.Print.(list (list int))
  [ [ 1; 2; 3 ]; [ 4 ] ]
  (subs ~last:CCOption.return 3 [ 1; 2; 3; 4 ])
;;

eq
  ~printer:Q.Print.(list (list int))
  [ [ 1; 2 ]; [ 3; 4 ] ]
  (subs 2 [ 1; 2; 3; 4; 5 ])
;;

q Q.(small_list small_int) (fun l -> l = (chunks 3 l |> List.flatten));;
q Q.(small_list small_int) (fun l -> l = (chunks 5 l |> List.flatten));;

q
  Q.(small_list small_int)
  (fun l -> List.for_all (fun u -> List.length u <= 5) (chunks 5 l))
;;

eq [] (intersperse 0 []);;
eq [ 1 ] (intersperse 0 [ 1 ]);;
eq [ 1; 0; 2; 0; 3; 0; 4 ] (intersperse 0 [ 1; 2; 3; 4 ]);;

q
  Q.(pair int (list int))
  (fun (x, l) ->
    length (intersperse x l)
    =
    if length l <= 1 then
      length l
    else
      (2 * length l) - 1)
;;

q
  Q.(pair int (list int))
  (fun (x, l) -> rev (intersperse x l) = intersperse x (rev l))
;;

eq [ 1; 2; 3; 4; 5 ] (interleave [ 1; 3 ] [ 2; 4; 5 ]);;
eq [ 1; 2; 3 ] (interleave [ 1 ] [ 2; 3 ]);;

q
  Q.(pair (small_list int) (small_list int))
  (fun (l1, l2) -> length (interleave l1 l2) = length l1 + length l2)
;;

q Q.(small_list int) (fun l -> l = interleave [] l);;
q Q.(small_list int) (fun l -> l = interleave l []);;
t @@ fun () -> take_while (fun x -> x < 10) (1 -- 20) = 1 -- 9;;
t @@ fun () -> take_while (fun x -> x <> 0) [ 0; 1; 2; 3 ] = [];;
t @@ fun () -> take_while (fun _ -> true) [] = [];;
t @@ fun () -> take_while (fun _ -> true) (1 -- 10) = 1 -- 10;;

q
  Q.(pair (fun1 Observable.int bool) (list small_int))
  (fun (f, l) ->
    let l1 = take_while (Q.Fn.apply f) l in
    List.for_all (Q.Fn.apply f) l1)
;;

q
  Q.(pair (fun1 Observable.int bool) (list small_int))
  (fun (f, l) -> take_while (Q.Fn.apply f) l @ drop_while (Q.Fn.apply f) l = l)
;;

q
  Q.(pair (fun1 Observable.int bool) (list small_int))
  (fun (f, l) ->
    let l1, l2 = take_drop_while (Q.Fn.apply f) l in
    l1 = take_while (Q.Fn.apply f) l && l2 = drop_while (Q.Fn.apply f) l)
;;

eq ~printer:Q.Print.(option (list int)) (Some [ 2; 3 ]) (tail_opt [ 1; 2; 3 ]);;
eq ~printer:Q.Print.(option (list int)) (Some []) (tail_opt [ 1 ]);;
eq ~printer:Q.Print.(option (list int)) None (tail_opt []);;
eq ~printer:Q.Print.(option int) (Some 1) (head_opt [ 1; 2; 3 ]);;
eq ~printer:Q.Print.(option int) (Some 1) (head_opt [ 1 ]);;
eq ~printer:Q.Print.(option int) None (head_opt []);;
eq ~printer:Q.Print.(option int) (Some 3) (last_opt [ 1; 2; 3 ]);;
eq ~printer:Q.Print.(option int) (Some 1) (last_opt [ 1 ]);;
eq ~printer:Q.Print.(option int) None (last_opt []);;
t @@ fun () -> find_pred (( = ) 4) [ 1; 2; 5; 4; 3; 0 ] = Some 4;;
t @@ fun () -> find_pred (fun _ -> true) [] = None;;
t @@ fun () -> find_pred (fun _ -> false) (1 -- 10) = None;;
t @@ fun () -> find_pred (fun x -> x < 10) (1 -- 9) = Some 1;;

t @@ fun () ->
find_map
  (fun x ->
    if x = 3 then
      Some "a"
    else
      None)
  [ 1; 2; 3; 4 ]
= Some "a"
;;

t @@ fun () ->
find_map
  (fun x ->
    if x = 3 then
      Some "a"
    else
      None)
  [ 1; 2; 4; 5 ]
= None
;;

t @@ fun () ->
remove ~eq:CCInt.equal ~key:1 [ 2; 1; 3; 3; 2; 1 ] = [ 2; 3; 3; 2 ]
;;

t @@ fun () -> remove ~eq:CCInt.equal ~key:10 [ 1; 2; 3 ] = [ 1; 2; 3 ];;

eq [ "2"; "4" ]
  (filter_map
     (fun x ->
       if x mod 2 = 0 then
         Some (string_of_int x)
       else
         None)
     [ 1; 2; 3; 4; 5 ])
;;

eq [ "2"; "4"; "6" ]
  (filter_map
     (fun x ->
       if x mod 2 = 0 then
         Some (string_of_int x)
       else
         None)
     [ 1; 2; 3; 4; 5; 6 ])
;;

eq (Some []) (all_some []);;
eq (Some [ 1; 2; 3 ]) (all_some [ Some 1; Some 2; Some 3 ]);;
eq None (all_some [ Some 1; None; None; Some 4 ]);;

t @@ fun () ->
let s1 = 1 -- 3 in
let s2 = [ "1"; "2" ] in
let join_row i j =
  if string_of_int i = j then
    Some (string_of_int i ^ " = " ^ j)
  else
    None
in
let s = join ~join_row s1 s2 in
assert_equal [ "1 = 1"; "2 = 2" ] s;
true
;;

eq
  [ 'a', [ "abc"; "attic" ]; 'b', [ "barbary"; "boom"; "bop" ]; 'c', [] ]
  (group_join_by
     (fun s -> s.[0])
     (CCString.to_list "abc")
     [ "abc"; "boom"; "attic"; "deleted"; "barbary"; "bop" ]
  |> map (fun (c, l) -> c, List.sort Stdlib.compare l)
  |> sort Stdlib.compare)
;;

eq (Ok []) (all_ok []);;
eq (Ok [ 1; 2; 3 ]) (all_ok [ Ok 1; Ok 2; Ok 3 ]);;
eq (Error "e2") (all_ok [ Ok 1; Error "e2"; Error "e3"; Ok 4 ]);;
q Q.(small_list small_int) (fun l -> mem 1 l = List.mem 1 l);;

q
  Q.(pair int (list int))
  (fun (x, l) ->
    remove_one ~eq:CCInt.equal x (add_nodup ~eq:CCInt.equal x l) = l)
;;

q
  Q.(pair int (list int))
  (fun (x, l) ->
    mem ~eq:CCInt.equal x l
    || List.length (add_nodup ~eq:CCInt.equal x l) = List.length l + 1)
;;

q
  Q.(pair int (list int))
  (fun (x, l) ->
    (not (mem ~eq:CCInt.equal x l))
    || List.length (remove_one ~eq:CCInt.equal x l) = List.length l - 1)
;;

t @@ fun () ->
uniq ~eq:CCInt.equal [ 1; 2; 3 ] |> List.sort Stdlib.compare = [ 1; 2; 3 ]
;;

t @@ fun () ->
uniq ~eq:CCInt.equal [ 1; 1; 2; 2; 3; 4; 4; 2; 4; 1; 5 ]
|> List.sort Stdlib.compare = [ 1; 2; 3; 4; 5 ]
;;

q
  Q.(small_list small_int)
  (fun l ->
    sort_uniq ~cmp:CCInt.compare l
    = (uniq ~eq:CCInt.equal l |> sort Stdlib.compare))
;;

t @@ fun () ->
union ~eq:CCInt.equal [ 1; 2; 4 ] [ 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ]
;;

t @@ fun () -> inter ~eq:CCInt.equal [ 1; 2; 4 ] [ 2; 3; 4; 5 ] = [ 2; 4 ];;
t @@ fun () -> mapi (fun i x -> i * x) [ 10; 10; 10 ] = [ 0; 10; 20 ];;
t @@ fun () -> get_at_idx 0 (range 0 10) = Some 0;;
t @@ fun () -> get_at_idx 5 (range 0 10) = Some 5;;
t @@ fun () -> get_at_idx 11 (range 0 10) = None;;
t @@ fun () -> get_at_idx (-1) (range 0 10) = Some 10;;
t @@ fun () -> get_at_idx 0 [] = None;;
t @@ fun () -> get_at_idx (-1) [] = None;;
t @@ fun () -> set_at_idx 0 10 [ 1; 2; 3 ] = [ 10; 2; 3 ];;
t @@ fun () -> set_at_idx 4 10 [ 1; 2; 3 ] = [ 1; 2; 3 ];;
t @@ fun () -> set_at_idx 1 10 [ 1; 2; 3 ] = [ 1; 10; 3 ];;
t @@ fun () -> set_at_idx (-2) 10 [ 1; 2; 3 ] = [ 1; 10; 3 ];;
t @@ fun () -> insert_at_idx 0 10 [ 1; 2; 3 ] = [ 10; 1; 2; 3 ];;
t @@ fun () -> insert_at_idx 4 10 [ 1; 2; 3 ] = [ 1; 2; 3; 10 ];;
t @@ fun () -> insert_at_idx 1 10 [ 1; 2; 3 ] = [ 1; 10; 2; 3 ];;
t @@ fun () -> insert_at_idx (-2) 10 [ 1; 2; 3 ] = [ 1; 10; 2; 3 ];;
t @@ fun () -> remove_at_idx 0 [ 1; 2; 3; 4 ] = [ 2; 3; 4 ];;
t @@ fun () -> remove_at_idx 3 [ 1; 2; 3; 4 ] = [ 1; 2; 3 ];;
t @@ fun () -> remove_at_idx 5 [ 1; 2; 3; 4 ] = [ 1; 2; 3; 4 ];;
t @@ fun () -> remove_at_idx (-1) [ 1; 2; 3; 4 ] = [ 1; 2; 3 ];;
t @@ fun () -> remove_at_idx (-2) [ 1; 2; 3; 4 ] = [ 1; 2; 4 ];;
t @@ fun () -> remove_at_idx (-3) [ 1; 2; 3; 4 ] = [ 1; 3; 4 ];;
t @@ fun () -> remove_at_idx (-4) [ 1; 2; 3; 4 ] = [ 2; 3; 4 ];;

(* note: the last test checks that no error occurs due to overflows. *)
t @@ fun () -> range_by ~step:1 0 0 = [ 0 ];;
t @@ fun () -> range_by ~step:1 5 0 = [];;
t @@ fun () -> range_by ~step:2 1 0 = [];;
t @@ fun () -> range_by ~step:2 0 4 = [ 0; 2; 4 ];;
t @@ fun () -> range_by ~step:2 0 5 = [ 0; 2; 4 ];;
t @@ fun () -> range_by ~step:~-1 0 0 = [ 0 ];;
t @@ fun () -> range_by ~step:~-1 0 5 = [];;
t @@ fun () -> range_by ~step:~-2 0 1 = [];;
t @@ fun () -> range_by ~step:~-2 5 1 = [ 5; 3; 1 ];;
t @@ fun () -> range_by ~step:~-2 5 0 = [ 5; 3; 1 ];;
t @@ fun () -> range_by ~step:max_int 0 2 = [ 0 ];;

q
  Q.(pair small_int small_int)
  (fun (i, j) ->
    let i = min i j and j = max i j in
    range_by ~step:1 i j = range i j)
;;

t @@ fun () -> range 0 5 = [ 0; 1; 2; 3; 4; 5 ];;
t @@ fun () -> range 0 0 = [ 0 ];;
t @@ fun () -> range 5 2 = [ 5; 4; 3; 2 ];;
t @@ fun () -> range' 0 0 = [];;
t @@ fun () -> range' 0 5 = [ 0; 1; 2; 3; 4 ];;
t @@ fun () -> range' 5 2 = [ 5; 4; 3 ];;
t @@ fun () -> append (range 0 100) (range 101 1000) = range 0 1000;;
t @@ fun () -> append (range 1000 501) (range 500 0) = range 1000 0;;

q
  Q.(pair small_int small_int)
  (fun (a, b) ->
    let l = a --^ b in
    not (List.mem b l))
;;

t @@ fun () -> repeat 2 [ 1; 2; 3 ] = [ 1; 2; 3; 1; 2; 3 ];;

q
  Q.(pair small_int (small_list int))
  (fun (n, l) ->
    if n > 0 then
      repeat n l = flat_map (fun _ -> l) (1 -- n)
    else
      Q.assume_fail ())
;;

t @@ fun () -> Assoc.get ~eq:CCInt.equal 1 [ 1, "1"; 2, "2" ] = Some "1";;
t @@ fun () -> Assoc.get ~eq:CCInt.equal 2 [ 1, "1"; 2, "2" ] = Some "2";;
t @@ fun () -> Assoc.get ~eq:CCInt.equal 3 [ 1, "1"; 2, "2" ] = None;;
t @@ fun () -> Assoc.get ~eq:CCInt.equal 42 [] = None;;

t @@ fun () ->
Assoc.set ~eq:CCInt.equal 2 "two" [ 1, "1"; 2, "2" ]
|> List.sort Stdlib.compare
= [ 1, "1"; 2, "two" ]
;;

t @@ fun () ->
Assoc.set ~eq:CCInt.equal 3 "3" [ 1, "1"; 2, "2" ]
|> List.sort Stdlib.compare
= [ 1, "1"; 2, "2"; 3, "3" ]
;;

t @@ fun () -> Assoc.mem ~eq:CCInt.equal 1 [ 1, "1"; 2, "2"; 3, "3" ];;
t @@ fun () -> not (Assoc.mem ~eq:CCInt.equal 4 [ 1, "1"; 2, "2"; 3, "3" ]);;

eq
  [ 1, "1"; 2, "22" ]
  (Assoc.update ~eq:CCInt.equal
     ~f:(function
       | Some "2" -> Some "22"
       | _ -> assert false)
     2
     [ 1, "1"; 2, "2" ]
  |> lsort)
;;

eq
  [ 1, "1"; 3, "3" ]
  (Assoc.update ~eq:CCInt.equal
     ~f:(function
       | Some "2" -> None
       | _ -> assert false)
     2
     [ 1, "1"; 2, "2"; 3, "3" ]
  |> lsort)
;;

eq
  [ 1, "1"; 2, "2"; 3, "3" ]
  (Assoc.update ~eq:CCInt.equal
     ~f:(function
       | None -> Some "3"
       | _ -> assert false)
     3
     [ 1, "1"; 2, "2" ]
  |> lsort)
;;

eq [ 1, "1" ] (Assoc.remove ~eq:CCInt.equal 2 [ 1, "1"; 2, "2" ] |> lsort);;

eq
  [ 1, "1"; 3, "3" ]
  (Assoc.remove ~eq:CCInt.equal 2 [ 1, "1"; 2, "2"; 3, "3" ] |> lsort)
;;

eq
  [ 1, "1"; 2, "2" ]
  (Assoc.remove ~eq:CCInt.equal 3 [ 1, "1"; 2, "2" ] |> lsort)
;;

t @@ fun () ->
let l = Ref.create () in
Ref.push l 1;
Ref.push_list l [ 2; 3 ];
!l = [ 3; 2; 1 ]
;;

t @@ fun () ->
random_len 10 CCInt.random_small (Random.State.make [||]) |> List.length = 10
;;

eq ~printer:(fun s -> s) (to_string string_of_int []) "";;
eq ~printer:(fun s -> s) (to_string ~start:"[" ~stop:"]" string_of_int []) "[]"
;;

eq
  ~printer:(fun s -> s)
  (to_string ~start:"[" ~stop:"]" string_of_int [ 1 ])
  "[1]"
;;

eq
  ~printer:(fun s -> s)
  (to_string ~start:"[" ~stop:"]" string_of_int [ 1; 2; 3; 4 ])
  "[1, 2, 3, 4]"
;;

eq
  ~printer:(fun s -> s)
  (to_string ~sep:" " string_of_int [ 1; 2; 3; 4 ])
  "1 2 3 4"
;;

q Q.(list int) (fun l -> of_iter (to_iter l) = l);;
q Q.(list int) (fun l -> of_gen (to_gen l) = l);;

eq
  ~printer:(fun s -> s)
  "[1, 2, 3]"
  (CCFormat.to_string
     (CCFormat.hbox
        (CCList.pp
           ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
           ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
           CCFormat.int))
     [ 1; 2; 3 ])
