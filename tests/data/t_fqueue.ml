module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCFQueue

let pp_ilist = CCFormat.(to_string (list int));;

t @@ fun () ->
let q = empty in
assert_bool "is_empty" (is_empty q);
true
;;

q
  (Q.pair Q.int (Q.list Q.int))
  (fun (x, l) -> cons x (of_list l) |> to_list = x :: l)
;;

q
  (Q.pair Q.int (Q.list Q.int))
  (fun (x, l) -> snoc (of_list l) x |> to_list = l @ [ x ])
;;

t @@ fun () ->
let q = List.fold_left snoc empty [ 1; 2; 3; 4; 5 ] in
let q = tail q in
let q = List.fold_left snoc q [ 6; 7; 8 ] in
let l = Iter.to_list (to_iter q) in
assert_equal ~printer:pp_ilist [ 2; 3; 4; 5; 6; 7; 8 ] l;
true
;;

q
  (Q.pair Q.int (Q.list Q.int))
  (fun (x, l) ->
    let x', q = cons x (of_list l) |> take_front_exn in
    x' = x && to_list q = l)
;;

t @@ fun () ->
let q = of_list [ 1; 2; 3; 4 ] in
let x, q = take_front_exn q in
assert_equal 1 x;
let q = List.fold_left snoc q [ 5; 6; 7 ] in
assert_equal 2 (first_exn q);
let x, _q = take_front_exn q in
assert_equal 2 x;
true
;;

t @@ fun () -> take_front empty = None;;

t @@ fun () ->
let l, q = take_front_l 5 (1 -- 10) in
l = [ 1; 2; 3; 4; 5 ] && to_list q = [ 6; 7; 8; 9; 10 ]
;;

t @@ fun () ->
take_front_while (fun x -> x < 5) (1 -- 10) |> fst = [ 1; 2; 3; 4 ]
;;

q
  (Q.pair Q.int (Q.list Q.int))
  (fun (x, l) ->
    let q, x' = snoc (of_list l) x |> take_back_exn in
    x' = x && to_list q = l)
;;

t @@ fun () -> take_back empty = None;;
q (Q.list Q.int) (fun l -> size (of_list l) = List.length l);;

t @@ fun () ->
let l = CCList.(0 -- 100) in
let q = of_list l in
List.map (fun i -> nth_exn i q) l = l
;;

q ~count:30 (Q.list Q.int) (fun l ->
    let len = List.length l in
    let idx = CCList.(0 -- (len - 1)) in
    let q = of_list l in
    l = [] || List.for_all (fun i -> nth i q = Some (List.nth l i)) idx)
;;

q (Q.list Q.int) (fun l ->
    l = [] || of_list l |> init |> to_list = List.rev (List.tl (List.rev l)))
;;

q (Q.list Q.int) (fun l -> l = [] || of_list l |> tail |> to_list = List.tl l);;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    add_iter_front (Iter.of_list l1) (of_list l2) |> to_list = l1 @ l2)
;;

q (Q.list Q.int) (fun l -> of_list l |> to_iter |> Iter.to_list = l);;

q
  (Q.pair (Q.list Q.int) (Q.list Q.int))
  (fun (l1, l2) -> append (of_list l1) (of_list l2) |> to_list = l1 @ l2)
;;

t @@ fun () ->
let q1 = of_iter (Iter.of_list [ 1; 2; 3; 4 ]) in
let q2 = of_iter (Iter.of_list [ 5; 6; 7; 8 ]) in
let q = append q1 q2 in
let l = Iter.to_list (to_iter q) in
assert_equal ~printer:pp_ilist [ 1; 2; 3; 4; 5; 6; 7; 8 ] l;
true
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    add_seq_front (CCList.to_seq l1) (of_list l2) |> to_list = l1 @ l2)
;;

q (Q.list Q.int) (fun l -> of_list l |> to_seq |> CCList.of_seq = l);;

q (Q.list Q.int) (fun l ->
    of_list l |> map string_of_int |> to_list = List.map string_of_int l)
;;

q (Q.list Q.int) (fun l ->
    of_list l |> fold (fun acc x -> x :: acc) [] = List.rev l)
;;

t @@ fun () ->
let q = of_iter (Iter.of_list [ 1; 2; 3; 4 ]) in
let n = fold ( + ) 0 q in
assert_equal 10 n;
true
;;

q (Q.list Q.int) (fun l -> Iter.of_list l |> of_iter |> to_list = l);;
q (Q.list Q.int) (fun l -> of_list l |> rev |> to_list = List.rev l);;

t @@ fun () ->
let q1 = 1 -- 10 and q2 = append (1 -- 5) (6 -- 10) in
equal ( = ) q1 q2
;;

t @@ fun () -> 1 -- 5 |> to_list = [ 1; 2; 3; 4; 5 ];;
t @@ fun () -> 5 -- 1 |> to_list = [ 5; 4; 3; 2; 1 ];;
t @@ fun () -> 0 -- 0 |> to_list = [ 0 ];;
t @@ fun () -> 1 --^ 5 |> to_list = [ 1; 2; 3; 4 ];;
t @@ fun () -> 5 --^ 1 |> to_list = [ 5; 4; 3; 2 ];;
t @@ fun () -> 1 --^ 2 |> to_list = [ 1 ];;
t @@ fun () -> 0 --^ 0 |> to_list = []
