
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCBV;;

let ppli = CCFormat.(Dump.list int);;

q (Q.pair Q.small_int Q.bool) (fun (size, b) -> create ~size b |> length = size);;

t @@ fun () -> create ~size:17 true |> cardinal = 17;;
t @@ fun () -> create ~size:32 true |> cardinal = 32;;
t @@ fun () -> create ~size:132 true |> cardinal = 132;;
t @@ fun () -> create ~size:200 false |> cardinal = 0;;
t @@ fun () -> create ~size:29 true |> to_sorted_list = CCList.range 0 28;;

q (Q.list Q.small_int) (fun l ->
  let bv = of_list l in to_list bv = to_list (copy bv));;

q Q.small_int (fun size -> create ~size true |> cardinal = size);;

t @@ fun () ->
  let bv1 = CCBV.create ~size:87 true in
  assert_equal ~printer:string_of_int 87 (CCBV.cardinal bv1);
  true;;

q Q.small_int (fun n -> CCBV.cardinal (CCBV.create ~size:n true) = n);;

t @@ fun () ->
  let bv = CCBV.create ~size:99 false in
  assert_bool "32 must be false" (not (CCBV.get bv 32));
  assert_bool "88 must be false" (not (CCBV.get bv 88));
  assert_bool "5 must be false" (not (CCBV.get bv 5));
  CCBV.set bv 32;
  CCBV.set bv 88;
  CCBV.set bv 5;
  assert_bool "32 must be true" (CCBV.get bv 32);
  assert_bool "88 must be true" (CCBV.get bv 88);
  assert_bool "5 must be true" (CCBV.get bv 5);
  assert_bool "33 must be false" (not (CCBV.get bv 33));
  assert_bool "44 must be false" (not (CCBV.get bv 44));
  assert_bool "1 must be false" (not (CCBV.get bv 1));
  true;;

t @@ fun () -> let bv = create ~size:3 false in set bv 0; get bv 0;;
t @@ fun () -> let bv = create ~size:3 false in set bv 1; not (get bv 0);;
t @@ fun () -> let bv = create ~size:3 false in set bv 0; reset bv 0; not (get bv 0);;

t @@ fun () ->
  let bv = of_list [1;10; 11; 30] in
  flip bv 10;
  assert_equal ~printer:Q.Print.(list int) [1;11;30] (to_sorted_list bv);
  assert_equal ~printer:Q.Print.bool false (get bv 10);
  flip bv 10;
  assert_equal ~printer:Q.Print.bool true (get bv 10);
  flip bv 5;
  assert_equal ~printer:Q.Print.(list int) [1;5;10;11;30] (to_sorted_list bv);
  assert_equal ~printer:Q.Print.bool true (get bv 5);
  flip bv 100;
  assert_equal ~printer:Q.Print.(list int) [1;5;10;11;30;100] (to_sorted_list bv);
  assert_equal ~printer:Q.Print.bool true (get bv 100);
  true;;

t @@ fun () -> let bv = create ~size:37 true in cardinal bv = 37 && (clear bv; cardinal bv= 0);;

t @@ fun () ->
  let bv = CCBV.of_list [1; 5; 200] in
  assert_equal ~printer:string_of_int 3 (CCBV.cardinal bv);
  CCBV.clear bv;
  assert_equal ~printer:string_of_int 0 (CCBV.cardinal bv);
  assert_bool "must be empty" (CCBV.is_empty bv);
  true;;

t @@ fun () -> equal (of_list [1; 3; 4]) (of_list [1; 3; 4]);;
t @@ fun () -> equal (empty()) (empty());;
t @@ fun () -> not (equal (empty ()) (of_list [1]));;
t @@ fun () -> not (equal (empty ()) (of_list [2; 5]));;
t @@ fun () -> not (equal (of_list [1;3]) (of_list [2; 3]));;

t @@ fun () ->
  List.iter
    (fun size ->
      let bv = create ~size false in
      set bv 5;
      let n = ref 0 in
      iter bv (fun i b -> incr n; assert_equal b (i=5));
      assert_bool "exactly size" (!n = size))
    [30; 100; 255; 256;10_000];
    true;;

let iter_zip s k = s (fun x y -> k(x,y));;

let eq' = eq ~printer:Q.Print.(list (pair int bool)) ;;
eq'  [] (iter (create ~size:0 false) |> iter_zip |> Iter.to_list);;
eq'  [0, false; 1, true; 2, false]
(iter (let bv = create ~size:3 false in set bv 1; bv) |> iter_zip |> Iter.to_list);;

q Q.(small_int) (fun n ->
    assert (n >= 0);
    let bv = create ~size:n true in
    let l = iter bv |> iter_zip |> Iter.to_list in
    List.length l = n && List.for_all (fun (_,b) -> b) l);;

t @@ fun () -> of_list [1;5;7] |> iter_true |> Iter.to_list |> List.sort CCOrd.poly = [1;5;7];;

let _gen = Q.Gen.(map of_list (list nat))
let _pp bv = Q.Print.(list string) (List.map string_of_int (to_list bv))
let _small bv = length bv

let gen_bv = Q.make ~small:_small ~print:_pp _gen;;

q gen_bv (fun bv ->
    let l' = Iter.to_rev_list (CCBV.iter_true bv) in
    let bv' = CCBV.of_list l' in
    CCBV.cardinal bv = CCBV.cardinal bv'
);;

t @@ fun () ->
  let bv = CCBV.of_list [1; 5; 156; 0; 222] in
  assert_equal ~printer:string_of_int 5 (CCBV.cardinal bv);
  CCBV.set bv 201;
  assert_equal ~printer:string_of_int 6 (CCBV.cardinal bv);
  let l = CCBV.to_list bv in
  let l = List.sort compare l in
  assert_equal [0;1;5;156;201;222] l;
  true;;

eq ~printer:(CCFormat.to_string ppli)
  [1;2;3;4;64;130] (of_list [1;2;3;4;64;130] |> to_sorted_list);;

q Q.(small_list small_nat) (fun l ->
    let l = List.sort_uniq CCOrd.poly l in
    let l2 = of_list l |> to_sorted_list in
    if l=l2 then true else Q.Test.fail_reportf "l1=%a, l2=%a" ppli l ppli l2);;
q Q.(small_list small_nat) (fun l ->
    let bv = of_list l in
    let l1 = bv |> to_sorted_list in
    let l2 =
      (CCList.init (length bv) (get bv) |> List.mapi (fun i b->i,b)
       |>CCList.filter_map (function (i,true) -> Some i| _ ->None)) in
    if l1=l2 then true else Q.Test.fail_reportf "l1=%a, l2=%a" ppli l1 ppli l2) ;;

eq ~cmp:equal ~printer:(CCFormat.to_string pp)
  (of_list [0]) (let bv=empty() in set bv 0; bv);;

t @@ fun () -> of_list [1;32;64] |> CCFun.flip get 64;;
t @@ fun () -> of_list [1;32;64] |> CCFun.flip get 32;;
t @@ fun () -> of_list [1;31;63] |> CCFun.flip get 63;;
t @@ fun () -> of_list [50; 10; 17; 22; 3; 12] |> first = Some 3;;
t @@ fun () -> let bv = of_list [1;2;3;4;5;6;7] in filter bv (fun x->x mod 2=0);
    to_sorted_list bv = [2;4;6];;

eq ~printer:(CCFormat.to_string ppli)
  [0;3;4;6] (let v = of_list [1;2;5;7;] in negate_self v; to_sorted_list v);;

q Q.small_int (fun size -> create ~size false |> negate |> cardinal = size);;

t @@ fun () ->
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.union bv1 bv2 in
  let l = List.sort compare (CCBV.to_list bv) in
  assert_equal ~printer:(CCFormat.(to_string (Dump.list int)))
    [1;2;3;4;200] l;
  true;;

t @@ fun () ->
  let bv1 = CCBV.of_list [1;2;3;4;64;130] in
  let bv2 = CCBV.of_list [4;64;3;120] in
  let bv = CCBV.union bv1 bv2 in
  assert_equal ~cmp:equal ~printer:(CCFormat.to_string pp)
    (of_list [1;2;3;4;64;120;130]) bv;
  true;;

t @@ fun () ->
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.union bv1 bv2 in
  assert_equal ~cmp:equal ~printer:(CCFormat.to_string pp)
    (of_list [1;2;3;4;200]) bv;
  true;;

t @@ fun () ->
  let v1 = CCBV.empty () in
  let () = CCBV.set v1 64 in
  let v2 = CCBV.diff (CCBV.empty ()) (CCBV.empty ()) in
  let v3 = CCBV.union v1 v2 in
  assert_equal ~printer:(CCFormat.to_string pp) ~cmp:CCBV.equal v1 v3;
  true;;

t @@ fun () -> union (of_list [1;2;3;4;5]) (of_list [7;3;5;6]) |> to_sorted_list = CCList.range 1 7;;
t @@ fun () -> inter (of_list [1;2;3;4]) (of_list [2;4;6;1]) |> to_sorted_list = [1;2;4];;

t @@ fun () ->
  let bv1 = CCBV.of_list [1;2;3;4;200;201] in
  let bv2 = CCBV.of_list [4;200;3] in
  let bv = CCBV.inter bv1 bv2 in
  let l = List.sort compare (CCBV.to_list bv) in
  assert_equal ~printer:(CCFormat.(to_string (Dump.list int)))
    [3;4;200] l;
  true;;

t @@ fun () ->
  let bv1 = CCBV.of_list [1;2;3;4] in
  let bv2 = CCBV.of_list [4;200;3] in
  CCBV.inter_into ~into:bv1 bv2;
  let l = List.sort compare (CCBV.to_list bv1) in
  assert_equal [3;4] l;
  true;;

t @@ fun () -> diff (of_list [1;2;3])    (of_list [1;2;3])   |> to_list = [];;
t @@ fun () -> diff (of_list [1;2;3])    (of_list [1;2;3;4]) |> to_list = [];;
t @@ fun () -> diff (of_list [1;2;3;4])  (of_list [1;2;3])   |> to_list = [4];;
t @@ fun () -> diff (of_list [1;2;3])      (of_list [1;2;3;400]) |> to_list = [];;
t @@ fun () -> diff (of_list [1;2;3;400])  (of_list [1;2;3])     |> to_list = [400];;

t @@ fun () ->
  let v1 = CCBV.empty () in
  set v1 65;
  let v2 = CCBV.diff v1 v1 in
  CCBV.is_empty v2 ;;

t @@ fun () ->
  let bv = CCBV.of_list [1;2;5;400] in
  let arr = [|"a"; "b"; "c"; "d"; "e"; "f"|] in
  let l = List.sort compare (CCBV.select bv arr) in
  assert_equal ["b"; "c"; "f"] l;
  true;;

t @@ fun () ->
  let bv = CCBV.of_list [1;2;5;400] in
  let arr = [|"a"; "b"; "c"; "d"; "e"; "f"|] in
  let l = List.sort compare (CCBV.selecti bv arr) in
  assert_equal [("b",1); ("c",2); ("f",5)] l;
  true;;

eq ~printer:Q.Print.(list (pair int int))
  [1,1; 3,3; 4,4] (selecti (of_list [1;4;3]) [| 0;1;2;3;4;5;6;7;8 |]
       |> List.sort CCOrd.poly);;

q Q.(small_int) (fun i ->
      let i = max 1 i in
      let bv = create ~size:i true in
      i = (to_iter bv |> Iter.length));;

t @@ fun () -> CCList.range 0 10 |> CCList.to_iter |> of_iter |> to_iter
       |> CCList.of_iter |> List.sort CCOrd.poly = CCList.range 0 10;;

eq ~printer:CCFun.id
  "bv {00001}" (CCFormat.to_string pp (of_list [4]));;
