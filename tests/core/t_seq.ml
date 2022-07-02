
open CCSeq

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> repeat ~n:4 0 |> to_list = [0;0;0;0];;
t @@ fun () -> repeat ~n:0 1 |> to_list = [];;
t @@ fun () -> repeat 1 |> take 20 |> to_list = (repeat ~n:20 1 |> to_list);;
t @@ fun () -> of_list [1;2;3;4] |> take_while (fun x->x < 4) |> to_list = [1;2;3];;

q (Q.pair (Q.list Q.small_int) Q.small_int) (fun (l,n) ->
    let s = of_list l in let s1, s2 = take n s, drop n s in
    append s1 s2 |> to_list = l  );;

t @@ fun () -> (map ((+) 1) (1 -- 5) |> to_list) = (2 -- 6 |> to_list);;
t @@ fun () -> mapi (fun i x -> i,x) (1 -- 3) |> to_list = [0, 1; 1, 2; 2, 3];;
t @@ fun () -> fmap (fun x -> if x mod 2=0 then Some (x*3) else None) (1--10) |> to_list
    = [6;12;18;24;30];;

t @@ fun () -> cycle (of_list [1;2]) |> take 5 |> to_list = [1;2;1;2;1];;
t @@ fun () -> cycle (of_list [1; ~-1]) |> take 100_000 |> fold (+) 0 = 0;;
t @@ fun () -> let f = function  10 -> None | x -> Some (x, x+1) in
  unfold f 0 |> to_list = [0;1;2;3;4;5;6;7;8;9];;
t @@ fun () -> for_all ((=) 1) (of_list []) = true;;
t @@ fun () -> for_all ((=) 1) (of_list [0]) = false;;
t @@ fun () -> for_all ((=) 1) (of_list [1]) = true;;
t @@ fun () -> for_all ((=) 1) (of_list [1; 0]) = false;;
t @@ fun () -> for_all ((=) 1) (of_list [0; 1]) = false;;
t @@ fun () -> for_all ((=) 1) (of_list [1; 1]) = true;;
t @@ fun () -> let l () = Cons (0, fun () -> failwith "no second element") in
  try ignore (for_all ((=) 1) l); true with Failure _ -> false;;
t @@ fun () -> exists ((=) 1) (of_list []) = false;;
t @@ fun () -> exists ((=) 1) (of_list [0]) = false;;
t @@ fun () -> exists ((=) 1) (of_list [1]) = true;;
t @@ fun () -> exists ((=) 1) (of_list [1; 0]) = true;;
t @@ fun () -> exists ((=) 1) (of_list [0; 1]) = true;;
t @@ fun () -> exists ((=) 1) (of_list [0; 0]) = false;;
t @@ fun () -> let l () = Cons (1, fun () -> failwith "no second element") in
  try ignore (exists ((=) 1) l); true with Failure _ -> false;;
t @@ fun () -> of_list [1;1;1;2;2;3;3;1] |> group (=)
  |> map to_list |> to_list = [[1;1;1]; [2;2]; [3;3]; [1]];;
t @@ fun () -> range 0 5 |> to_list = [0;1;2;3;4;5];;
t @@ fun () -> range 0 0 |> to_list = [0];;
t @@ fun () -> range 5 2 |> to_list = [5;4;3;2];;
t @@ fun () -> 1 --^ 5 |> to_list = [1;2;3;4];;
t @@ fun () -> 5 --^ 1 |> to_list = [5;4;3;2];;
t @@ fun () -> 1 --^ 2 |> to_list = [1];;
t @@ fun () -> 0 --^ 0 |> to_list = [];;

q Q.(list (pair int int)) (fun l ->
  let l = of_list l in let a, b = unzip l in equal (=) l (zip a b));;

eq [0,'a'; 1, 'b'; 2, 'c'] (of_string "abcde" |> zip_i |> take 3 |> to_list);;

q Q.(array int) (fun a -> of_array a |> to_array = a);;

t @@ fun () -> of_array [| 1; 2; 3 |] |> to_list = [1;2;3];;
t @@ fun () -> of_list [1;2;3] |> to_array = [| 1; 2; 3; |];;
t @@ fun () ->
  let r = ref 1 in
  let s = unfold (fun i -> if i < 3 then let x = !r in incr r; Some (x, succ i) else None) 0 in
  to_array s = [| 1; 2; 3; |];;

t @@ fun () ->
  let g = let n = ref 0 in fun () -> Some (incr n; !n) in
  let l = of_gen g in
  assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
  assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
  assert_equal [11;12] (drop 10 l |> take 2 |> to_list);
  true;;

t @@ fun () ->
  let printer = Q.Print.(list int) in
  let gen () =
    let rec l = let r = ref 0 in fun () -> incr r; Cons (!r, l) in l
  in
  let l1 = gen () in
  assert_equal ~printer [1;2;3;4] (take 4 l1 |> to_list);
  assert_equal ~printer [5;6;7;8] (take 4 l1 |> to_list);
  let l2 = gen () |> memoize in
  assert_equal ~printer [1;2;3;4] (take 4 l2 |> to_list);
  assert_equal ~printer [1;2;3;4] (take 4 l2 |> to_list);
  true;;

t @@ fun () -> interleave (of_list [1;3;5]) (of_list [2;4;6]) |> to_list = [1;2;3;4;5;6];;
t @@ fun () -> fair_app (of_list [(+)1; ( * ) 3]) (of_list [1; 10])
    |> to_list |> List.sort Stdlib.compare = [2; 3; 11; 30];;
