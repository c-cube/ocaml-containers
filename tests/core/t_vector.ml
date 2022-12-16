module T = (val Containers_testlib.make ~__FILE__ ())
include T
open CCVector;;

t @@ fun () -> create_with ~capacity:200 1 |> capacity >= 200;;
t @@ fun () -> return 42 |> to_list = [ 42 ];;
t @@ fun () -> return 42 |> length = 1;;

t @@ fun () ->
let v = create_with ~capacity:10 1 in
ensure v 200;
capacity v >= 200
;;

t @@ fun () ->
let v = create () in
push v 0.;
push v 1.;
push v 2.;
3 = length v
;;

t @@ fun () ->
let v = create () in
push v 1.;
push v 2.;
push v 3.;
6. = get v 0 +. get v 1 +. get v 2
;;

t @@ fun () ->
let v = create () in
push v 0;
push v 1;
push v 2;
3 = length v
;;

t @@ fun () ->
let v = create () in
push v 1;
push v 2;
push v 3;
6 = get v 0 + get v 1 + get v 2
;;

t @@ fun () ->
let v = create () in
push v "a";
push v "b";
push v "c";
3 = length v
;;

t @@ fun () ->
let v = create () in
push v "a";
push v "b";
push v "c";
"abc" = String.concat "" (to_list v)
;;

t @@ fun () ->
let v = create () in
push v 0.;
push v 1.;
clear v;
push v 0.;
push v 1.;
push v 7.;
push v 10.;
push v 12.;
truncate v 2;
assert_equal 1. (fold ( +. ) 0. v);
clear v;
assert_equal 0 (size v);
push v 0.;
push v 1.;
push v 7.;
push v 10.;
push v 12.;
assert_equal (1. +. 7. +. 10. +. 12.) (fold ( +. ) 0. v);
true
;;

t @@ fun () ->
let v = of_iter Iter.(1 -- 10) in
assert_equal 10 (size v);
clear v;
assert_equal 0 (size v);
assert (Iter.is_empty (to_iter v));
true
;;

t @@ fun () ->
let v = create () in
push v 1;
to_list v = [ 1 ]
;;

t @@ fun () ->
let v = of_list [ 1; 2; 3 ] in
push v 4;
to_list v = [ 1; 2; 3; 4 ]
;;

t @@ fun () ->
let v = make 1 0 in
resize_with v (fun i -> i) 5;
to_list v = [ 0; 1; 2; 3; 4 ]
;;

t @@ fun () ->
let v = make 1 0 in
resize_with v (fun i -> i) 5;
CCList.length (to_list v) = 5
;;

t @@ fun () ->
let v = create_with ~capacity:2 0 in
resize_with v (fun i -> i) 5;
to_list v = [ 0; 1; 2; 3; 4 ]
;;

t @@ fun () ->
let v = make 5 0 in
resize_with v (fun i -> i) 5;
to_list v = [ 0; 0; 0; 0; 0 ]
;;

t @@ fun () ->
let v = make 5 0 in
resize_with v (fun i -> i) 6;
to_list v = [ 0; 0; 0; 0; 0; 5 ]
;;

t @@ fun () ->
let v = make 5 0 in
try
  resize_with v (fun i -> i) (-1);
  false
with Invalid_argument _ -> true
;;

t @@ fun () ->
let v = make 5 0 in
resize_with v (fun i -> i) 5;
List.length (to_list v) = 5
;;

t @@ fun () ->
let v = make 1 0 in
resize_with_init v ~init:1 5;
to_list v = [ 0; 1; 1; 1; 1 ]
;;

t @@ fun () ->
let v = make 1 0 in
resize_with_init v ~init:1 5;
List.length (to_list v) = 5
;;

t @@ fun () ->
let v = create_with ~capacity:2 0 in
resize_with_init v ~init:1 5;
to_list v = [ 1; 1; 1; 1; 1 ]
;;

t @@ fun () ->
let v = make 5 0 in
resize_with_init v ~init:1 5;
to_list v = [ 0; 0; 0; 0; 0 ]
;;

t @@ fun () ->
let v = make 3 0 in
resize_with_init v ~init:1 5;
to_list v = [ 0; 0; 0; 1; 1 ]
;;

t @@ fun () ->
let v = make 5 0 in
try
  resize_with_init v ~init:1 (-1);
  false
with Invalid_argument _ -> true
;;

t @@ fun () ->
let v = make 5 0 in
resize_with_init v ~init:1 5;
List.length (to_list v) = 5
;;

(* test for asymptotic behavior *)
t @@ fun () ->
let v = make 1 0 in
for i = 0 to 100_000 do
  resize_with_init v ~init:10 i
done;
true
;;

t @@ fun () ->
let v1 = init 5 (fun i -> i) and v2 = init 5 (fun i -> i + 5) in
append v1 v2;
to_list v1 = CCList.(0 -- 9)
;;

t @@ fun () ->
let empty = create () and v2 = init 5 (fun i -> i) in
append empty v2;
to_list empty = CCList.(0 -- 4)
;;

t @@ fun () ->
let v1 = init 5 (fun i -> i) and empty = create () in
append v1 empty;
to_list v1 = CCList.(0 -- 4)
;;

t @@ fun () ->
let v = init 3 (fun i -> i) in
append v v;
to_list v = [ 0; 1; 2; 0; 1; 2 ]
;;

t @@ fun () ->
let empty = create () in
append empty empty;
to_list empty = []
;;

t @@ fun () ->
let a = of_iter Iter.(1 -- 5) in
let b = of_iter Iter.(6 -- 10) in
append a b;
assert_equal 10 (size a);
assert_equal (Iter.to_array Iter.(1 -- 10)) (to_array a);
assert_equal (Iter.to_array Iter.(6 -- 10)) (to_array b);
true
;;

q
  Q.(list_of_size (Gen.int_range 10 10) small_int)
  (fun l ->
    let v1 = of_list l and v2 = of_list l in
    remove_and_shift v1 9;
    remove_unordered v2 9;
    to_list v1 = to_list v2)
;;

q
  Q.(list_of_size (Gen.int_range 10 10) small_int)
  (fun l ->
    let l = List.sort CCInt.compare l in
    let v = of_list l in
    remove_and_shift v 3;
    to_list v = List.sort CCInt.compare (to_list v))
;;

q
  Q.(list_of_size (Gen.int_range 10 10) small_int)
  (fun l ->
    let l = List.sort CCInt.compare l in
    let v1 = of_list l and v2 = of_list l in
    remove_and_shift v1 3;
    remove_unordered v2 3;
    to_list v1 = List.sort CCInt.compare (to_list v2))
;;

t @@ fun () ->
let v = 1 -- 5 in
insert v 3 9;
to_list v = [ 1; 2; 3; 9; 4; 5 ]
;;

t @@ fun () ->
let v = create () in
insert v 0 2;
to_list v = [ 2 ]
;;

t @@ fun () ->
let v = 1 -- 3 in
remove_and_shift v 1;
insert v 1 5;
to_list v = [ 1; 5; 3 ]
;;

t @@ fun () ->
let v = 1 -- 3 in
remove_and_shift v 0;
insert v 2 5;
to_list v = [ 2; 3; 5 ]
;;

t @@ fun () ->
let v = 1 -- 3 in
insert v 3 5;
to_list v = [ 1; 2; 3; 5 ]
;;

t @@ fun () ->
let v1 = init 5 (fun i -> i) and v2 = Array.init 5 (fun i -> i + 5) in
append_array v1 v2;
to_list v1 = CCList.(0 -- 9)
;;

t @@ fun () ->
let empty = create () in
append_array empty CCArray.(0 -- 5);
to_list empty = CCList.(0 -- 5)
;;

t @@ fun () ->
let v1 = init 5 (fun i -> i) in
append_array v1 [||];
to_list v1 = CCList.(0 -- 4)
;;

t @@ fun () ->
let empty = create () in
append_array empty [||];
to_list empty = []
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    let v = of_list l1 in
    append_list v l2;
    to_list v = l1 @ l2)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    let v = of_list l1 in
    append_list v l2;
    length v = List.length l1 + List.length l2)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    let v = of_list l1 in
    append_gen v (Gen.of_list l2);
    to_list v = l1 @ l2)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    let v = of_list l1 in
    append_gen v (Gen.of_list l2);
    length v = List.length l1 + List.length l2)

let gen x =
  let small = length in
  let print =
    CCOption.map (fun p x -> Q.Print.list p (CCVector.to_list x)) x.Q.print
  in
  Q.make ?print ~small Q.Gen.(list x.Q.gen >|= of_list)
;;

q
  (Q.pair (gen Q.int) (gen Q.int))
  (fun (v1, v2) ->
    let l1 = to_list v1 in
    append v1 v2;
    Iter.to_list (to_iter v1)
    = Iter.(to_list (append (of_list l1) (to_iter v2))))
;;

t @@ fun () -> equal ( = ) (create ()) (create ());;
t @@ fun () -> equal ( = ) (return 42) (return 42);;
t @@ fun () -> not (equal ( = ) (create ()) (return 42));;
t @@ fun () -> not (equal ( = ) (return 42) (create ()));;

q
  Q.(
    let g = list_of_size Gen.(0 -- 10) small_int in
    pair g g)
  (fun (l1, l2) -> equal ( = ) (of_list l1) (of_list l2) = (l1 = l2))
;;

q
  Q.(pair (small_list small_int) (small_list small_int))
  (fun (l1, l2) ->
    let v1 = of_list l1 in
    let v2 = of_list l2 in
    equal ( = ) v1 v2 = (l1 = l2))
;;

q
  Q.(pair (small_list small_int) (small_list small_int))
  (fun (l1, l2) ->
    let v1 = of_list l1 in
    let v2 = of_list l2 in
    compare Stdlib.compare v1 v2 = CCList.compare Stdlib.compare l1 l2)
;;

t @@ fun () -> 1 -- 10 |> top = Some 10;;
t @@ fun () -> create () |> top = None;;
t @@ fun () -> 1 -- 10 |> top_exn = 10;;

t @@ fun () ->
let v = of_list [ 1; 2; 3 ] in
let v' = copy v in
to_list v' = [ 1; 2; 3 ]
;;

t @@ fun () -> create () |> copy |> is_empty;;

t @@ fun () ->
let v = of_iter Iter.(1 -- 100) in
assert_equal 100 (size v);
let v' = copy v in
assert_equal 100 (size v');
clear v';
assert (is_empty v');
assert (not (is_empty v));
true
;;

q
  Q.(small_list small_int)
  (fun l ->
    let v = of_list l in
    let v' = copy v in
    equal ( = ) v v')
;;

t @@ fun () ->
let v = of_iter Iter.(1 -- 10) in
truncate v 5;
assert_equal [ 1; 2; 3; 4; 5 ] (to_list v);
true
;;

q (gen Q.small_int) (fun v ->
    let n = size v / 2 in
    let l = to_list v in
    let h = Iter.(to_list (take n (of_list l))) in
    let v' = copy v in
    truncate v' n;
    h = to_list v')
;;

q (gen Q.small_int) (fun v ->
    let v' = copy v in
    shrink_to_fit v;
    to_list v = to_list v')
;;

q (gen Q.small_int) (fun v ->
    let v' = copy v in
    sort' Stdlib.compare v';
    let l = to_list v' in
    List.sort Stdlib.compare l = l)
;;

t @@ fun () ->
let v = of_list [ 1; 4; 5; 3; 2; 4; 1 ] in
uniq_sort Stdlib.compare v;
to_list v = [ 1; 2; 3; 4; 5 ]
;;

q ~long_factor:10
  Q.(small_list small_int)
  (fun l ->
    let v = of_list l in
    uniq_sort Stdlib.compare v;
    to_list v = CCList.sort_uniq ~cmp:Stdlib.compare l)
;;

t @@ fun () ->
let v = 0 -- 6 in
iteri (fun i _ -> if i = 3 then remove_unordered v i) v;
length v = 6
;;

t @@ fun () ->
let v = create () in
push v 1;
push v 2;
push v 3;
to_list (map string_of_int v) = [ "1"; "2"; "3" ]
;;

q
  Q.(pair (fun1 Observable.int small_int) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    to_list (map f v) = List.map f l)
;;

t @@ fun () ->
let v = create () in
push v 1;
push v 2;
push v 3;
to_list (mapi (fun i e -> Printf.sprintf "%i %i" i e) v)
= [ "0 1"; "1 2"; "2 3" ]
;;

q
  Q.(pair (fun2 Observable.int Observable.int small_int) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    to_list (mapi f v) = List.mapi f l)
;;

q
  Q.(pair (fun1 Observable.int small_int) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    map_in_place f v;
    to_list v = List.map f l)
;;

t @@ fun () ->
let v = 1 -- 10 in
filter_in_place (fun x -> x < 4) v;
to_list v = [ 1; 2; 3 ]
;;

q
  Q.(pair (fun1 Observable.int bool) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    filter_in_place f v;
    to_list v = List.filter f l)
;;

t @@ fun () ->
filter (fun x -> x mod 2 = 0) (of_list [ 1; 2; 3; 4; 5 ]) |> to_list = [ 2; 4 ]
;;

t @@ fun () ->
filter (fun x -> x mod 2 = 0) (1 -- 1_000_000) |> length = 500_000
;;

q
  Q.(pair (fun1 Observable.int bool) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    to_list (filter f v) = List.filter f l)
;;

t @@ fun () -> fold ( + ) 0 (of_list [ 1; 2; 3; 4; 5 ]) = 15;;
t @@ fun () -> fold ( + ) 0 (create ()) = 0;;

q
  Q.(pair (fun2 Observable.int Observable.int small_int) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    fold f 0 v = List.fold_left f 0 l)
;;

q
  Q.(pair (fun1 Observable.int bool) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    exists f v = List.exists f l)
;;

q
  Q.(pair (fun1 Observable.int bool) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    for_all f v = List.for_all f l)
;;

q
  Q.(pair (fun1 Observable.int bool) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    find f v = CCList.find_pred f l)
;;

q
  Q.(list small_int)
  (fun l ->
    let v = of_list l in
    let f x = x > 30 && x < 35 in
    find_map
      (fun x ->
        if f x then
          Some x
        else
          None)
      v
    = find f v)
;;

q
  Q.(pair (fun1 Observable.int (option bool)) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    to_list (filter_map f v) = CCList.filter_map f l)
;;

q
  Q.(pair (fun1 Observable.int (option small_int)) (small_list small_int))
  (fun (Q.Fun (_, f), l) ->
    let v = of_list l in
    filter_map_in_place f v;
    to_list v = CCList.filter_map f l)
;;

(* check it frees memory properly *)
t @@ fun () ->
let w = Weak.create 1 in
let v =
  let s = "coucou" ^ "lol" in
  Weak.set w 0 (Some s) ;
  of_list [ "a"; s ] in
filter_in_place (fun s -> String.length s <= 1) v;
assert_equal 1 (length v);
assert_equal "a" (get v 0);
Gc.full_major ();
assert_equal None (Weak.get w 0);
true
;;

eq ~cmp:( = )
  ~printer:Q.Print.(list int)
  [ 11; 12; 21; 22 ]
  (List.sort CCInt.compare @@ to_list
  @@ monoid_product ( + ) (of_list [ 10; 20 ]) (of_list [ 1; 2 ]))
;;

eq ~cmp:( = )
  ~printer:Q.Print.(list int)
  [ 11; 12; 13; 14 ]
  (List.sort CCInt.compare @@ to_list
  @@ monoid_product ( + ) (of_list [ 10 ]) (of_list [ 1; 2; 3; 4 ]))
;;

q
  Q.(small_list small_int)
  (fun l ->
    let v = of_list l in
    rev_in_place v;
    to_list v = List.rev l)
;;

t @@ fun () -> rev (of_list [ 1; 2; 3; 4 ]) |> to_list = [ 4; 3; 2; 1 ];;
t @@ fun () -> rev (of_list [ 1; 2; 3; 4; 5 ]) |> to_list = [ 5; 4; 3; 2; 1 ];;
t @@ fun () -> rev (create ()) |> to_list = [];;

q
  Q.(small_list small_int)
  (fun l ->
    let v = of_list l in
    to_list (rev v) = List.rev l)
;;

t @@ fun () ->
let v = of_list [ 1; 2; 3 ] in
(fun f -> rev_iter f v) |> Iter.to_list = [ 3; 2; 1 ]
;;

q
  Q.(list int)
  (fun l ->
    let v = of_list l in
    (fun f -> rev_iter f v) |> Iter.to_list = List.rev l)
;;

t @@ fun () -> of_iter Iter.(1 -- 10) |> to_list = CCList.(1 -- 10);;

q
  Q.(list int)
  (fun l ->
    let v = of_list l in
    v |> to_iter_rev |> Iter.to_rev_list = l)
;;

t @@ fun () ->
slice_iter (of_list [ 0; 1; 2; 3; 4 ]) 1 3 |> CCList.of_iter = [ 1; 2; 3 ]
;;

t @@ fun () ->
slice_iter (of_list [ 0; 1; 2; 3; 4 ]) 1 4 |> CCList.of_iter = [ 1; 2; 3; 4 ]
;;

t @@ fun () ->
slice_iter (of_list [ 0; 1; 2; 3; 4 ]) 0 5 |> CCList.of_iter = [ 0; 1; 2; 3; 4 ]
;;

t @@ fun () -> 1 -- 4 |> to_list = [ 1; 2; 3; 4 ];;
t @@ fun () -> 4 -- 1 |> to_list = [ 4; 3; 2; 1 ];;
t @@ fun () -> 0 -- 0 |> to_list = [ 0 ];;

q
  Q.(pair small_int small_int)
  (fun (a, b) -> a -- b |> to_list = CCList.(a -- b))
;;

q
  Q.(pair small_int small_int)
  (fun (a, b) -> a --^ b |> to_list = CCList.(a --^ b))
;;

t @@ fun () -> of_list CCList.(1 -- 300_000) |> to_list = CCList.(1 -- 300_000)
;;

t @@ fun () ->
let v = 1 -- 10 in
to_list v = Gen.to_list (to_gen v)
