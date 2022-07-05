module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCBV
open Internal_

let spf = Printf.sprintf
let ppli = CCFormat.(Dump.list int)

module Intset = CCSet.Make (CCInt);;

q (Q.pair Q.small_int Q.bool) (fun (size, b) -> create ~size b |> length = size)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
create ~size:17 true |> cardinal = 17
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
create ~size:32 true |> cardinal = 32
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
create ~size:132 true |> cardinal = 132
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
create ~size:200 false |> cardinal = 0
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
create ~size:29 true |> to_sorted_list = CCList.range 0 28
;;

q (Q.list Q.small_int) (fun l ->
    let bv = of_list l in
    to_list bv = to_list (copy bv))
;;

q Q.small_int (fun size -> create ~size true |> cardinal = size);;

q Q.small_int (fun size ->
    create ~size true |> to_sorted_list = List.init size CCFun.id)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv1 = CCBV.create ~size:87 true in
assert_equal ~printer:string_of_int 87 (CCBV.cardinal bv1);
true
;;

q Q.small_int (fun n -> CCBV.cardinal (CCBV.create ~size:n true) = n);;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
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
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = create ~size:3 false in
set bv 0;
get bv 0
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = create ~size:3 false in
set bv 1;
not (get bv 0)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = create ~size:3 false in
set bv 0;
reset bv 0;
not (get bv 0)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = of_list [ 1; 10; 11; 30 ] in
flip bv 10;
assert_equal ~printer:Q.Print.(list int) [ 1; 11; 30 ] (to_sorted_list bv);
assert_equal ~printer:Q.Print.bool false (get bv 10);
flip bv 10;
assert_equal ~printer:Q.Print.bool true (get bv 10);
flip bv 5;
assert_equal
  ~printer:Q.Print.(list int)
  [ 1; 5; 10; 11; 30 ] (to_sorted_list bv);
assert_equal ~printer:Q.Print.bool true (get bv 5);
flip bv 100;
assert_equal
  ~printer:Q.Print.(list int)
  [ 1; 5; 10; 11; 30; 100 ] (to_sorted_list bv);
assert_equal ~printer:Q.Print.bool true (get bv 100);
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = create ~size:37 true in
cardinal bv = 37
&&
(clear bv;
 cardinal bv = 0)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = CCBV.of_list [ 1; 5; 200 ] in
assert_equal ~printer:string_of_int 3 (CCBV.cardinal bv);
CCBV.clear bv;
assert_equal ~printer:string_of_int 0 (CCBV.cardinal bv);
assert_bool "must be empty" (CCBV.is_empty bv);
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
equal (of_list [ 1; 3; 4 ]) (of_list [ 1; 3; 4 ])
;;

t ~name:(spf "line %d" __LINE__) @@ fun () -> equal (empty ()) (empty ());;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
not (equal (empty ()) (of_list [ 1 ]))
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
not (equal (empty ()) (of_list [ 2; 5 ]))
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
not (equal (of_list [ 1; 3 ]) (of_list [ 2; 3 ]))
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
List.iter
  (fun size ->
    let bv = create ~size false in
    set bv 5;
    let n = ref 0 in
    iter bv (fun i b ->
        incr n;
        assert_equal b (i = 5));
    assert_bool "exactly size" (!n = size))
  [ 30; 100; 255; 256; 10_000 ];
true

let iter_zip s k = s (fun x y -> k (x, y))
let eq' = eq ~printer:Q.Print.(list (pair int bool));;

eq' [] (iter (create ~size:0 false) |> iter_zip |> Iter.to_list);;

eq'
  [ 0, false; 1, true; 2, false ]
  (iter
     (let bv = create ~size:3 false in
      set bv 1;
      bv)
  |> iter_zip |> Iter.to_list)
;;

q
  Q.(small_int)
  (fun n ->
    assert (n >= 0);
    let bv = create ~size:n true in
    let l = iter bv |> iter_zip |> Iter.to_list in
    List.length l = n && List.for_all (fun (_, b) -> b) l)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
of_list [ 1; 5; 7 ]
|> iter_true |> Iter.to_list |> List.sort CCOrd.poly = [ 1; 5; 7 ]

let _gen = Q.Gen.(map of_list (list nat))
let _pp bv = Q.Print.(list string) (List.map string_of_int (to_list bv))
let _small bv = length bv
let gen_bv = Q.make ~small:_small ~print:_pp _gen;;

q gen_bv (fun bv ->
    let l' = Iter.to_rev_list (CCBV.iter_true bv) in
    let bv' = CCBV.of_list l' in
    CCBV.cardinal bv = CCBV.cardinal bv')
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = CCBV.of_list [ 1; 5; 156; 0; 222 ] in
assert_equal ~printer:string_of_int 5 (CCBV.cardinal bv);
CCBV.set bv 201;
assert_equal ~printer:string_of_int 6 (CCBV.cardinal bv);
let l = CCBV.to_list bv in
let l = List.sort compare l in
assert_equal [ 0; 1; 5; 156; 201; 222 ] l;
true
;;

eq ~printer:(CCFormat.to_string ppli) [ 1; 2; 3; 4; 64; 130 ]
  (of_list [ 1; 2; 3; 4; 64; 130 ] |> to_sorted_list)
;;

q
  Q.(small_list small_nat)
  (fun l ->
    let l = List.sort_uniq CCOrd.poly l in
    let l2 = of_list l |> to_sorted_list in
    if l = l2 then
      true
    else
      Q.Test.fail_reportf "l1=%a, l2=%a" ppli l ppli l2)
;;

q
  Q.(small_list small_nat)
  (fun l ->
    let bv = of_list l in
    let l1 = bv |> to_sorted_list in
    let l2 =
      CCList.init (length bv) (get bv)
      |> List.mapi (fun i b -> i, b)
      |> CCList.filter_map (function
           | i, true -> Some i
           | _ -> None)
    in
    if l1 = l2 then
      true
    else
      Q.Test.fail_reportf "l1=%a, l2=%a" ppli l1 ppli l2)
;;

eq ~cmp:equal ~printer:(CCFormat.to_string pp)
  (of_list [ 0 ])
  (let bv = empty () in
   set bv 0;
   bv)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
of_list [ 1; 32; 64 ] |> CCFun.flip get 64
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
of_list [ 1; 32; 64 ] |> CCFun.flip get 32
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
of_list [ 1; 31; 63 ] |> CCFun.flip get 63
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
of_list [ 50; 10; 17; 22; 3; 12 ] |> first = Some 3
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
filter bv (fun x -> x mod 2 = 0);
to_sorted_list bv = [ 2; 4; 6 ]
;;

eq ~printer:(CCFormat.to_string ppli) [ 0; 3; 4; 6 ]
  (let v = of_list [ 1; 2; 5; 7 ] in
   negate_self v;
   to_sorted_list v)
;;

q Q.small_int (fun size -> create ~size false |> negate |> cardinal = size);;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv1 = CCBV.of_list [ 1; 2; 3; 4 ] in
let bv2 = CCBV.of_list [ 4; 200; 3 ] in
let bv = CCBV.union bv1 bv2 in
let l = List.sort compare (CCBV.to_list bv) in
assert_equal ~printer:CCFormat.(to_string (Dump.list int)) [ 1; 2; 3; 4; 200 ] l;
true
;;

q ~name:"union"
  Q.(pair (small_list small_nat) (small_list small_nat))
  (fun (l1, l2) ->
    let bv1 = of_list l1 in
    let bv2 = of_list l2 in
    let l' = CCList.sort_uniq ~cmp:CCInt.compare (l1 @ l2) in
    let bv = union bv1 bv2 in
    let bv' = of_list l' in
    if not (equal bv bv') then
      Q.Test.fail_reportf "union (%a, %a) <> %a" ppli l1 ppli l2 ppli l';
    true)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv1 = CCBV.of_list [ 1; 2; 3; 4; 64; 130 ] in
let bv2 = CCBV.of_list [ 4; 64; 3; 120 ] in
let bv = CCBV.union bv1 bv2 in
assert_equal ~cmp:equal ~printer:(CCFormat.to_string pp)
  (of_list [ 1; 2; 3; 4; 64; 120; 130 ])
  bv;
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv1 = CCBV.of_list [ 1; 2; 3; 4 ] in
let bv2 = CCBV.of_list [ 4; 200; 3 ] in
let bv = CCBV.union bv1 bv2 in
assert_equal ~cmp:equal ~printer:(CCFormat.to_string pp)
  (of_list [ 1; 2; 3; 4; 200 ])
  bv;
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let v1 = CCBV.empty () in
let () = CCBV.set v1 64 in
let v2 = CCBV.diff (CCBV.empty ()) (CCBV.empty ()) in
let v3 = CCBV.union v1 v2 in
assert_equal ~printer:(CCFormat.to_string pp) ~cmp:CCBV.equal v1 v3;
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
union (of_list [ 1; 2; 3; 4; 5 ]) (of_list [ 7; 3; 5; 6 ])
|> to_sorted_list = CCList.range 1 7
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
inter (of_list [ 1; 2; 3; 4 ]) (of_list [ 2; 4; 6; 1 ])
|> to_sorted_list = [ 1; 2; 4 ]
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv1 = CCBV.of_list [ 1; 2; 3; 4; 200; 201 ] in
let bv2 = CCBV.of_list [ 4; 200; 3 ] in
let bv = CCBV.inter bv1 bv2 in
let l = List.sort compare (CCBV.to_list bv) in
assert_equal ~printer:CCFormat.(to_string (Dump.list int)) [ 3; 4; 200 ] l;
true
;;

q ~name:"inter" ~count:10_000
  Q.(pair (small_list small_nat) (small_list small_nat))
  (fun (l1, l2) ->
    let bv1 = of_list l1 in
    let bv2 = of_list l2 in
    let l' = CCList.inter ~eq:CCInt.equal l1 l2 in
    let bv = inter bv1 bv2 in
    let bv' = of_list l' in
    (* make sure both are of the same length before comparing *)
    let len = max (length bv) (length bv') in
    resize bv len;
    resize bv' len;
    if not (equal bv bv') then
      Q.Test.fail_reportf "inter (%a, %a) <> %a@ (@[<hv>bv= %a,@ bv'=%a@])" ppli
        l1 ppli l2 ppli l' pp bv pp bv';
    true)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv1 = CCBV.of_list [ 1; 2; 3; 4 ] in
let bv2 = CCBV.of_list [ 4; 200; 3 ] in
CCBV.inter_into ~into:bv1 bv2;
let l = List.sort compare (CCBV.to_list bv1) in
assert_equal [ 3; 4 ] l;
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
diff (of_list [ 1; 2; 3 ]) (of_list [ 1; 2; 3 ]) |> to_list = []
;;

q ~name:"diff" ~count:10_000
  Q.(pair (small_list small_nat) (small_list small_nat))
  (fun (l1, l2) ->
    let bv1 = of_list l1 in
    let bv2 = of_list l2 in
    let bv = diff bv1 bv2 in
    let l' = Intset.(diff (of_list l1) (of_list l2) |> to_list) in
    let bv' = of_list l' in
    (* make sure both are of the same length before comparing *)
    let len = max (length bv) (length bv') in
    resize bv len;
    resize bv' len;
    if not (equal bv bv') then
      Q.Test.fail_reportf "idff (%a, %a) <> %a@ (@[<hv>bv= %a,@ bv'=%a@])" ppli
        l1 ppli l2 ppli l' pp bv pp bv';
    true)
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
diff (of_list [ 1; 2; 3 ]) (of_list [ 1; 2; 3; 4 ]) |> to_list = []
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
diff (of_list [ 1; 2; 3; 4 ]) (of_list [ 1; 2; 3 ]) |> to_list = [ 4 ]
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
diff (of_list [ 1; 2; 3 ]) (of_list [ 1; 2; 3; 400 ]) |> to_list = []
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
diff (of_list [ 1; 2; 3; 400 ]) (of_list [ 1; 2; 3 ]) |> to_list = [ 400 ]
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let v1 = CCBV.empty () in
set v1 65;
let v2 = CCBV.diff v1 v1 in
CCBV.is_empty v2
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = CCBV.of_list [ 1; 2; 5; 400 ] in
let arr = [| "a"; "b"; "c"; "d"; "e"; "f" |] in
let l = List.sort compare (CCBV.select bv arr) in
assert_equal [ "b"; "c"; "f" ] l;
true
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
let bv = CCBV.of_list [ 1; 2; 5; 400 ] in
let arr = [| "a"; "b"; "c"; "d"; "e"; "f" |] in
let l = List.sort compare (CCBV.selecti bv arr) in
assert_equal [ "b", 1; "c", 2; "f", 5 ] l;
true
;;

eq
  ~printer:Q.Print.(list (pair int int))
  [ 1, 1; 3, 3; 4, 4 ]
  (selecti (of_list [ 1; 4; 3 ]) [| 0; 1; 2; 3; 4; 5; 6; 7; 8 |]
  |> List.sort CCOrd.poly)
;;

q
  Q.(small_int)
  (fun i ->
    let i = max 1 i in
    let bv = create ~size:i true in
    i = (to_iter bv |> Iter.length))
;;

t ~name:(spf "line %d" __LINE__) @@ fun () ->
CCList.range 0 10 |> CCList.to_iter |> of_iter |> to_iter |> CCList.of_iter
|> List.sort CCOrd.poly = CCList.range 0 10
;;

eq ~printer:CCFun.id "bv {00001}" (CCFormat.to_string pp (of_list [ 4 ]))

let eq' = eq ~printer:CCInt.to_string;;

eq' 0b0 (__lsb_mask 0);;
eq' 0b1 (__lsb_mask 1);;
eq' 0b11 (__lsb_mask 2);;
eq' 0b111 (__lsb_mask 3);;
eq' 0b1111 (__lsb_mask 4);;
eq' 0b1_1111 (__lsb_mask 5);;
eq' 0b11_1111 (__lsb_mask 6);;
eq' 0b111_1111 (__lsb_mask 7);;
eq' 0b1111_1111 (__lsb_mask 8)

let popcount8_ref n =
  let rec loop n =
    if n = 0 then
      0
    else if n land 1 = 0 then
      loop (n lsr 1)
    else
      1 + loop (n lsr 1)
  in
  loop n
;;

(* test __popcount8 just to be sure. *)
t ~name:(spf "line %d" __LINE__) (fun () ->
    for i = 0 to 255 do
      let n = __popcount8 i in
      let n2 = popcount8_ref i in
      if n <> n2 then (
        Printf.printf "bad: i=%d => %d,%d\n" i n n2;
        assert false
      )
    done;
    true)
;;

t ~name:(spf "line %d" __LINE__) (fun () ->
    let b = create ~size:10 false in
    assert_equal 10 (length b);
    set b 9;
    for i = 0 to 9 do
      assert (i = 9 || not (get b i))
    done;

    resize b 42;
    assert_equal 42 (length b);
    for i = 0 to 41 do
      assert (i = 9 || not (get b i))
    done;
    resize b 11;
    assert_equal 11 (length b);
    for i = 0 to 11 do
      assert (i = 9 || not (get b i))
    done;

    true)
;;

t ~name:(spf "line %d" __LINE__) (fun () ->
    let v = empty () in
    resize v 9;
    inter_into ~into:v (of_list []);
    true)
;;

t ~name:(spf "line %d" __LINE__) (fun () ->
    let bv = empty () in
    flip bv 0;
    resize bv 0;
    negate_self bv;
    union_into ~into:bv (of_list [ 2 ]);
    assert_equal ~printer:(CCFormat.to_string ppli) [ 2 ] (to_list bv);
    true)
;;

t ~name:(spf "line %d" __LINE__) (fun () ->
    let bv = empty () in
    flip bv 0;
    inter_into ~into:bv (of_list []);
    negate_self bv;
    assert_equal ~printer:(CCFormat.to_string ppli) [] (to_list bv);
    true)
;;

t ~name:(spf "line %d" __LINE__) (fun () ->
    let v = empty () in
    union_into ~into:v (of_list [ 9; 16 ]);
    resize_minimize_memory v 9;
    Internal_.__check_invariant v;
    is_empty v)

module Op = struct
  type t =
    | Resize of int
    | Resize_min_mem of int
    | Set of int
    | Reset of int
    | Set_bool of int * bool
    | Flip of int
    | Clear
    | Clear_and_shrink
    | Filter_is_odd
    | Negate
    | Inter of int list
    | Union of int list
    | Diff of int list

  let apply (self : CCBV.t) (op : t) : unit =
    match op with
    | Resize n -> resize self n
    | Resize_min_mem n -> resize_minimize_memory self n
    | Set i -> set self i
    | Reset i -> reset self i
    | Set_bool (i, b) -> set_bool self i b
    | Flip i -> flip self i
    | Clear -> clear self
    | Clear_and_shrink -> clear_and_shrink self
    | Filter_is_odd -> filter self (fun i -> i mod 2 = 1)
    | Negate -> negate_self self
    | Inter l ->
      let bv' = of_list l in
      inter_into ~into:self bv'
    | Union l ->
      let bv' = of_list l in
      union_into ~into:self bv'
    | Diff l ->
      let bv' = of_list l in
      diff_into ~into:self bv'

  let post_size sz (self : t) =
    match self with
    | Resize i -> i
    | Resize_min_mem i -> i
    | Set j | Reset j | Set_bool (j, _) | Flip j -> max sz (j + 1)
    | Clear -> sz
    | Clear_and_shrink -> 0
    | Filter_is_odd | Negate -> sz
    | Diff _ -> sz
    | Inter [] | Union [] -> sz
    | Union l -> max sz (succ (List.fold_left max 0 l))
    | Inter l -> min sz (succ (List.fold_left max 0 l))

  let gen_ size : t Q.Gen.t =
    let open Q.Gen in
    let nonzero =
      [
        (3, 0 -- size >|= fun x -> Set x);
        (3, 0 -- size >|= fun x -> Reset x);
        ( 3,
          0 -- size >>= fun x ->
          bool >|= fun y -> Set_bool (x, y) );
        (3, 0 -- size >|= fun x -> Flip x);
      ]
    in

    (* random list of integers *)
    let rand_list =
      0 -- 200 >>= fun n st ->
      List.init n (fun i ->
          if bool st then
            Some i
          else
            None)
      |> CCList.keep_some
    in

    frequency
    @@ List.flatten
         [
           (if size > 0 then
             nonzero
           else
             []);
           [
             1, return Clear;
             1, return Clear_and_shrink;
             1, return Negate;
             1, return Filter_is_odd;
             (1, rand_list >|= fun l -> Inter l);
             (1, rand_list >|= fun l -> Union l);
             (1, rand_list >|= fun l -> Diff l);
             (1, 0 -- 100 >|= fun x -> Resize x);
             (1, 0 -- 100 >|= fun x -> Resize_min_mem x);
           ];
         ]

  let shrink =
    let open Q.Iter in
    let module S = Q.Shrink in
    function
    | Resize i -> S.int i >|= fun i -> Resize i
    | Resize_min_mem i -> S.int i >|= fun i -> Resize_min_mem i
    | Set i -> S.int i >|= fun i -> Resize i
    | Reset i -> S.int i >|= fun i -> Resize i
    | Set_bool (i, b) ->
      S.int i
      >|= (fun i -> Set_bool (i, b))
      <+>
      if b then
        return @@ Set_bool (i, b)
      else
        empty
    | Flip i -> S.int i >|= fun i -> Flip i
    | Clear | Clear_and_shrink | Filter_is_odd | Negate -> empty
    | Inter l -> S.list ~shrink:S.int l >|= fun l -> Inter l
    | Union l -> S.list ~shrink:S.int l >|= fun l -> Union l
    | Diff l -> S.list ~shrink:S.int l >|= fun l -> Diff l

  let pp out =
    let fpf = Format.fprintf in
    function
    | Resize i -> fpf out "resize %d" i
    | Resize_min_mem i -> fpf out "resize_minimize_memory %d" i
    | Set i -> fpf out "set %d" i
    | Reset i -> fpf out "reset %d" i
    | Set_bool (i, b) -> fpf out "set_bool(%d,%b)" i b
    | Flip i -> fpf out "flip %d" i
    | Clear -> fpf out "clear"
    | Clear_and_shrink -> fpf out "clear_and_shrink"
    | Filter_is_odd -> fpf out "filter_is_odd"
    | Negate -> fpf out "negate"
    | Inter l -> fpf out "inter %a" ppli l
    | Union l -> fpf out "union %a" ppli l
    | Diff l -> fpf out "diff %a" ppli l

  let arb_l =
    let rec gen_l sz n =
      let open Q.Gen in
      if n = 0 then
        return []
      else
        gen_ sz >>= fun op ->
        let sz' = post_size sz op in
        gen_l sz' (n - 1) >|= fun tl -> op :: tl
    in

    Q.make
      ~print:CCFormat.(to_string @@ Dump.list pp)
      ~shrink:(Q.Shrink.list ~shrink)
      Q.Gen.(0 -- 30 >>= fun len -> gen_l 0 len)
end

module Ref_ = struct
  type t = { mutable set: Intset.t; mutable size: int }

  let empty () = { size = 0; set = Intset.empty }

  let to_list self =
    Intset.to_list self.set |> List.filter (fun x -> x < self.size)

  let pp out (self : t) = ppli out (to_list self)

  let equal_to_bv (self : t) (bv : CCBV.t) : bool =
    to_list self = CCBV.to_sorted_list bv

  let cardinal self : int =
    Intset.filter (fun x -> x < self.size) self.set |> Intset.cardinal

  let get (self : t) i = Intset.mem i self.set

  let rec apply_op (self : t) (op : Op.t) =
    match op with
    | Resize n | Resize_min_mem n ->
      self.set <- Intset.filter (fun x -> x < n) self.set;
      self.size <- n
    | Set i ->
      self.size <- max self.size (i + 1);
      self.set <- Intset.add i self.set
    | Reset i ->
      self.size <- max self.size (i + 1);
      self.set <- Intset.remove i self.set
    | Set_bool (i, b) ->
      apply_op self
        (if b then
          Set i
        else
          Reset i)
    | Flip i ->
      self.size <- max self.size (i + 1);
      apply_op self
        (if Intset.mem i self.set then
          Reset i
        else
          Set i)
    | Clear -> self.set <- Intset.empty
    | Clear_and_shrink ->
      self.set <- Intset.empty;
      self.size <- 0
    | Filter_is_odd -> self.set <- Intset.filter (fun x -> x mod 2 = 1) self.set
    | Negate ->
      let l' =
        List.init self.size (fun x -> x)
        |> List.filter (fun x -> not (Intset.mem x self.set))
      in
      self.set <- Intset.of_list l'
    | Inter l ->
      let s' = Intset.of_list l in
      let sz' = List.fold_left (fun s x -> max s (x + 1)) 0 l in
      self.size <- min self.size sz';
      self.set <- Intset.inter self.set s'
    | Union l ->
      let s' = Intset.of_list l in
      self.size <- List.fold_left (fun s x -> max s (x + 1)) self.size l;
      self.set <- Intset.union self.set s'
    | Diff l ->
      let s' = Intset.of_list l in
      self.set <- Intset.diff self.set s'
end
;;

q ~name:"list ops: invariant" ~max_fail:1 ~count:20_000 Op.arb_l (fun ops ->
    let bv = empty () in

    Internal_.__check_invariant bv;
    List.iter
      (fun op ->
        Op.apply bv op;
        Internal_.__check_invariant bv)
      ops;
    true)
;;

q ~name:"list ops: compare to ref" ~max_fail:1 ~count:2_000 Op.arb_l (fun ops ->
    let bv = empty () in
    let bv' = Ref_.empty () in

    List.iter
      (fun op ->
        Op.apply bv op;
        Ref_.apply_op bv' op;

        if cardinal bv <> Ref_.cardinal bv' then
          Q.Test.fail_reportf
            "@[<v2>different cardinal:@ actual=%a@ ref=%a@ @[<v2>actual.card \
             %d@]@ @[<v2>ref.cardinal %d@]@]"
            pp bv Ref_.pp bv' (cardinal bv) (Ref_.cardinal bv');

        let bad_idx =
          Iter.(0 -- CCBV.length bv)
          |> Iter.find_pred (fun i -> get bv i <> Ref_.get bv' i)
        in
        (match bad_idx with
        | None -> ()
        | Some idx ->
          Q.Test.fail_reportf
            "at idx %d, not same `get`@ actual.get=%b,@ ref.get=%b" idx
            (get bv idx) (Ref_.get bv' idx));

        if not (Ref_.equal_to_bv bv' bv) then
          Q.Test.fail_reportf
            "@[<v2>not equal:@ actual=%a@ ref=%a@ @[<v2>actual.to_list@ %a@]@ \
             @[<v2>ref.to_list@ %a@]@]"
            pp bv Ref_.pp bv' ppli (to_sorted_list bv) ppli (Ref_.to_list bv'))
      ops;
    true)
