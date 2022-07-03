module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCRAL;;

q
  Q.(pair (pair small_int int) (list int))
  (fun ((i, v), l) ->
    l = []
    ||
    let i = abs i mod List.length l in
    let ral = of_list l in
    let ral = set ral i v in
    get_exn ral i = v)
;;

q
  Q.(list small_int)
  (fun l ->
    let l1 = of_list l in
    CCList.mapi (fun i x -> i, x) l
    |> List.for_all (fun (i, x) -> get_exn l1 i = x))
;;

t @@ fun () ->
let l = of_list [ 1; 2; 3 ] in
hd l = 1
;;

t @@ fun () ->
let l = of_list [ 1; 2; 3 ] in
tl l |> to_list = [ 2; 3 ]
;;

q
  Q.(list_of_size Gen.(1 -- 100) int)
  (fun l ->
    let open Q in
    let l' = of_list l in
    (not (is_empty l')) ==> equal ~eq:CCInt.equal l' (cons (hd l') (tl l')))
;;

eq
  ~printer:Q.Print.(list int)
  [ 1; 2; 4 ]
  (to_list @@ remove (of_list [ 1; 2; 3; 4 ]) 2)
;;

eq
  ~printer:Q.Print.(pair int (list int))
  (3, [ 1; 2; 4 ])
  (CCPair.map_snd to_list @@ get_and_remove_exn (of_list [ 1; 2; 3; 4 ]) 2)
;;

q Q.small_int (fun n ->
    let l = CCList.(0 -- n) in
    let l' = of_list l |> mapi ~f:(fun i x -> i, x) in
    List.mapi (fun i x -> i, x) l = to_list l')
;;

q
  Q.(pair (list small_int) (fun2 Observable.int Observable.int bool))
  (fun (l, f) ->
    let f = Q.Fn.apply f in
    mapi ~f (of_list l) |> to_list = List.mapi f l)
;;

q
  Q.(list int)
  (fun l ->
    let f x = x + 1 in
    of_list l |> rev_map ~f |> to_list = List.rev_map f l)
;;

q
  Q.(list small_int)
  (fun l ->
    let l = of_list l in
    rev (rev l) = l)
;;

q
  Q.(list small_int)
  (fun l ->
    let l1 = of_list l in
    length l1 = List.length l)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) -> append (of_list l1) (of_list l2) = of_list (l1 @ l2))
;;

t @@ fun () ->
of_list [ 1; 2; 3; 4; 5; 6 ]
|> filter ~f:(fun x -> x mod 2 = 0)
|> to_list = [ 2; 4; 6 ]
;;

q
  Q.(pair (fun1 Observable.int (small_list int)) (small_list int))
  (fun (f, l) ->
    let f x = Q.Fn.apply f x in
    let f' x = f x |> of_list in
    of_list l |> flat_map f' |> to_list = CCList.(flat_map f l))
;;

t @@ fun () ->
flatten (of_list [ of_list [ 1 ]; of_list []; of_list [ 2; 3 ] ])
= of_list [ 1; 2; 3 ]
;;

q
  Q.(small_list (small_list int))
  (fun l ->
    of_list l |> map ~f:of_list |> flatten |> to_list = CCList.flatten l)
;;

t @@ fun () ->
app (of_list [ ( + ) 2; ( * ) 10 ]) (of_list [ 1; 10 ])
|> to_list = [ 3; 12; 10; 100 ]
;;

t @@ fun () -> take 3 (of_list CCList.(1 -- 10)) |> to_list = [ 1; 2; 3 ];;
t @@ fun () -> take 5 (of_list CCList.(1 -- 10)) |> to_list = [ 1; 2; 3; 4; 5 ]
;;
t @@ fun () -> take 0 (of_list CCList.(1 -- 10)) |> to_list = [];;

q
  Q.(pair small_int (list int))
  (fun (n, l) -> of_list l |> take n |> to_list = CCList.take n l)
;;

q
  Q.(list int)
  (fun l ->
    let f x = x mod 7 <> 0 in
    of_list l |> take_while ~f |> to_list = CCList.take_while f l)
;;

q
  Q.(pair (fun1 Observable.int bool) (list int))
  (fun (f, l) ->
    let f x = Q.Fn.apply f x in
    of_list l |> take_while ~f |> to_list = CCList.take_while f l)
;;

t @@ fun () -> of_list [ 1; 2; 3 ] |> drop 2 |> length = 1;;

q
  Q.(pair small_int (list int))
  (fun (n, l) -> of_list l |> drop n |> to_list = CCList.drop n l)
;;

t @@ fun () -> drop 3 (of_list CCList.(1 -- 10)) |> to_list = CCList.(4 -- 10);;
t @@ fun () -> drop 5 (of_list CCList.(1 -- 10)) |> to_list = [ 6; 7; 8; 9; 10 ]
;;
t @@ fun () -> drop 0 (of_list CCList.(1 -- 10)) |> to_list = CCList.(1 -- 10);;
t @@ fun () -> drop 15 (of_list CCList.(1 -- 10)) |> to_list = [];;

q
  Q.(list_of_size Gen.(0 -- 200) int)
  (fun l ->
    let f x = x mod 10 <> 0 in
    of_list l |> drop_while ~f |> to_list = CCList.drop_while f l)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) -> equal ~eq:CCInt.equal (of_list l1) (of_list l2) = (l1 = l2))
;;

q
  Q.(pair small_int (small_list int))
  (fun (n, l) -> of_list l |> repeat n |> to_list = CCList.(repeat n l))
;;

t @@ fun () -> range 0 3 |> to_list = [ 0; 1; 2; 3 ];;
t @@ fun () -> range 3 0 |> to_list = [ 3; 2; 1; 0 ];;
t @@ fun () -> range 17 17 |> to_list = [ 17 ];;

q
  Q.(pair small_int small_int)
  (fun (i, j) -> range i j |> to_list = CCList.(i -- j))

let eq' = eq ~printer:CCFormat.(to_string (hbox (list int)));;

eq' [ 1; 2; 3; 4 ] (1 --^ 5 |> to_list);;
eq' [ 5; 4; 3; 2 ] (5 --^ 1 |> to_list);;
eq' [ 1 ] (1 --^ 2 |> to_list);;
eq' [] (0 --^ 0 |> to_list);;

q
  Q.(pair (list small_int) (list small_int))
  (fun (l1, l2) -> add_list (of_list l2) l1 |> to_list = l1 @ l2)
;;

q Q.(list int) (fun l -> to_list (of_list l) = l);;
q Q.(array int) (fun a -> of_array a |> to_array = a);;
q Q.(list small_int) (fun l -> of_list l |> to_iter |> Iter.to_list = l);;
q Q.(list small_int) (fun l -> Iter.of_list l |> of_iter |> to_list = l);;

t @@ fun () ->
add_iter (of_list [ 3; 4 ]) (Iter.of_list [ 1; 2 ]) |> to_list = [ 1; 2; 3; 4 ]
;;

q Q.(list small_int) (fun l -> of_list l |> to_gen |> Gen.to_list = l);;
q Q.(list small_int) (fun l -> Gen.of_list l |> of_gen |> to_list = l);;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    compare ~cmp:CCInt.compare (of_list l1) (of_list l2) = Stdlib.compare l1 l2)
