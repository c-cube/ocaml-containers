module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCDeque

let plist l = CCFormat.(to_string (list int)) l
let pint i = string_of_int i;;

t @@ fun () ->
let q = create () in
add_iter_back q Iter.(3 -- 5);
assert_equal [ 3; 4; 5 ] (to_list q);
add_iter_front q Iter.(of_list [ 2; 1 ]);
assert_equal [ 1; 2; 3; 4; 5 ] (to_list q);
push_front q 0;
assert_equal [ 0; 1; 2; 3; 4; 5 ] (to_list q);
assert_equal 5 (take_back q);
assert_equal 0 (take_front q);
assert_equal 4 (length q);
true
;;

t @@ fun () ->
let q = of_iter Iter.(1 -- 100) in
assert_equal 100 (length q);
clear q;
assert_equal 0 (length q);
assert_raises (( = ) Empty) (fun () -> peek_front q);
assert_raises (( = ) Empty) (fun () -> peek_back q);
true
;;

t @@ fun () -> of_list [ 1; 2; 3 ] |> peek_front = 1;;

t @@ fun () ->
try
  ignore (of_list [] |> peek_front);
  false
with Empty -> true
;;

t @@ fun () ->
let d = of_iter Iter.(1 -- 10) in
let printer = pint in
assert_equal ~printer 1 (peek_front d);
push_front d 42;
assert_equal ~printer 42 (peek_front d);
assert_equal ~printer 42 (take_front d);
assert_equal ~printer 1 (take_front d);
assert_equal ~printer 2 (take_front d);
assert_equal ~printer 3 (take_front d);
assert_equal ~printer 10 (peek_back d);
true
;;

t @@ fun () -> of_list [ 1; 2; 3 ] |> peek_back = 3;;

t @@ fun () ->
try
  ignore (of_list [] |> peek_back);
  false
with Empty -> true
;;

t @@ fun () ->
let d = of_iter Iter.(1 -- 10) in
let printer = pint in
assert_equal ~printer 1 (peek_front d);
push_back d 42;
assert_equal ~printer 42 (peek_back d);
assert_equal ~printer 42 (take_back d);
assert_equal ~printer 10 (take_back d);
assert_equal ~printer 9 (take_back d);
assert_equal ~printer 8 (take_back d);
assert_equal ~printer 1 (peek_front d);
true
;;

t @@ fun () ->
let q = of_list [ 1 ] in
take_back q = 1 && to_list q = []
;;

t @@ fun () ->
let q = of_list [ 1; 2 ] in
take_back q = 2 && to_list q = [ 1 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3 ] in
take_back q = 3 && to_list q = [ 1; 2 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
take_back q = 7 && to_list q = [ 1; 2; 3; 4; 5; 6 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3 ] in
take_front q = 1 && to_list q = [ 2; 3 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
remove_back q;
to_list q = [ 1; 2; 3; 4; 5; 6 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
remove_front q;
to_list q = [ 2; 3; 4; 5; 6; 7 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
update_front q (fun _ -> None);
to_list q = [ 2; 3; 4; 5; 6; 7 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
update_front q (fun _ -> Some 9);
to_list q = [ 9; 2; 3; 4; 5; 6; 7 ]
;;

q
  Q.(list int)
  (fun l ->
    let q = of_list l in
    update_front q (fun _ -> None);
    let output_list =
      if l = [] then
        []
      else
        List.tl l
    in
    to_list q = output_list)
;;

q
  Q.(list int)
  (fun l ->
    let q = of_list l in
    update_front q (fun x -> Some (x + 42));
    let output_list =
      if l = [] then
        []
      else
        List.((hd l + 42) :: tl l)
    in
    to_list q = output_list)
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
update_back q (fun _ -> None);
to_list q = [ 1; 2; 3; 4; 5; 6 ]
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6; 7 ] in
update_back q (fun _ -> Some 9);
to_list q = [ 1; 2; 3; 4; 5; 6; 9 ]
;;

q
  Q.(list int)
  (fun l ->
    let q = of_list l in
    update_back q (fun _ -> None);
    let output_list =
      if l = [] then
        []
      else
        List.(rev l |> tl)
    in
    to_list q |> List.rev = output_list)
;;

q
  Q.(list int)
  (fun l ->
    let q = of_list l in
    update_back q (fun x -> Some (x + 42));
    let output_list =
      if l = [] then
        []
      else
        List.(rev l |> fun l -> (hd l + 42) :: tl l)
    in
    to_list q |> List.rev = output_list)
;;

t @@ fun () ->
let n = ref 0 in
iter (fun _ -> incr n) (of_list [ 1; 2; 3 ]);
!n = 3
;;

t @@ fun () ->
let d = of_iter Iter.(1 -- 5) in
let s = Iter.from_iter (fun k -> iter k d) in
let l = Iter.to_list s in
assert_equal ~printer:plist [ 1; 2; 3; 4; 5 ] l;
true
;;

t @@ fun () ->
let q = of_list [ 3; 4 ] in
append_front ~into:q (of_list [ 2; 1 ]);
assert_equal [ 1; 2; 3; 4 ] (to_list q);
append_back ~into:q (of_list [ 5; 6 ]);
assert_equal [ 1; 2; 3; 4; 5; 6 ] (to_list q);
true
;;

t @@ fun () -> fold ( + ) 0 (of_list [ 1; 2; 3 ]) = 6;;

t @@ fun () ->
fold (fun acc x -> x :: acc) [] (of_list [ 1; 2; 3 ]) = [ 3; 2; 1 ]
;;

q
  Q.(list int)
  (fun l ->
    let q = of_list l in
    append_front ~into:q (of_list l);
    append_back ~into:q (of_list l);
    length q = 3 * List.length l)
;;

t @@ fun () ->
let d = of_iter Iter.(1 -- 10) in
assert_equal ~printer:pint 10 (length d);
true
;;

t @@ fun () ->
let q = of_list [ 4; 5 ] in
add_iter_front q Iter.(of_list [ 3; 2; 1 ]);
assert_equal [ 1; 2; 3; 4; 5 ] (to_list q);
add_iter_back q Iter.(of_list [ 6; 7 ]);
assert_equal [ 1; 2; 3; 4; 5; 6; 7 ] (to_list q);
true
;;

q
  Q.(list int)
  (fun l -> Iter.of_list l |> of_iter |> to_iter |> Iter.to_list = l)
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3 ] in
assert_equal 1 (take_front q);
assert_equal 3 (take_back q);
assert_equal 2 (take_front q);
assert_equal true (is_empty q);
true
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4; 5; 6 ] in
filter_in_place q (fun x -> x mod 2 = 0);
assert_equal [ 2; 4; 6 ] (to_list q);
true
;;

t @@ fun () ->
let q = of_list [ 2; 1; 4; 6; 10; 20 ] in
filter_in_place q (fun x -> x mod 2 = 0);
assert_equal [ 2; 4; 6; 10; 20 ] (to_list q);
true
;;

q
  Q.(list nat_small)
  (fun l ->
    let f x = x mod 2 = 0 in
    let q = of_list l in
    (filter_in_place q f;
     to_list q)
    = List.filter f l)
;;

q
  Q.(list nat_small)
  (fun l ->
    let f x = x mod 2 = 0 in
    let q = filter f (of_list l) in
    to_list q = List.filter f l)
;;

t @@ fun () -> of_list [ 1; 2; 3 ] |> to_gen |> of_gen |> to_list = [ 1; 2; 3 ]
;;
q Q.(list int) (fun l -> of_list l |> to_gen |> of_gen |> to_list = l);;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4 ] in
assert_equal 4 (length q);
let q' = copy q in
let cmp = equal ~eq:CCInt.equal in
assert_equal 4 (length q');
assert_equal ~cmp q q';
push_front q 0;
assert_bool "not equal" (not (cmp q q'));
assert_equal 5 (length q);
push_front q' 0;
assert_equal ~cmp q q';
true
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    CCOrd.equiv
      (compare ~cmp:Stdlib.compare (of_list l1) (of_list l2))
      (CCList.compare Stdlib.compare l1 l2))
