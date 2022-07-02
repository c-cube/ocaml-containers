
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCPool;;

module P = Make(struct let max_size = 30 end)
module P2 = Make(struct let max_size = 15 end)
module Fut = P.Fut
module Fut2 = P2.Fut;;

t @@ fun () ->
  List.iter
    (fun n ->
      let l = Iter.(1 -- n) |> Iter.to_list in
      let l = List.rev_map (fun _i ->
        Fut.make
          (fun () ->
            Thread.delay 0.01;
            1
        )) l in
      let l' = List.map Fut.get l in
      assert_equal n (List.fold_left (+) 0 l');
    )
    [ 10; 300; ];
  true;;

t @@ fun () ->
  List.iter
    (fun n ->
      let l = Iter.(1 -- n) |> Iter.to_list in
      let l = List.rev_map (fun _i ->
        Fut2.make
          (fun () ->
            Thread.delay 0.01;
            1
        )) l in
      let l' = List.map Fut2.get l in
      assert_equal n (List.fold_left (+) 0 l');
    )
    [ 10; 300; ];
  true;;

t @@ fun () ->
  let a = Fut.make (fun () -> 1) in
  let b = Fut.return 42 in
  let c = Fut.monoid_product CCPair.make a b in
  assert_equal (1,42) (Fut.get c);
  true;;

t @@ fun () ->
  let a = Fut.make (fun () -> 1) in
  let b = Fut.make (fun () -> 42) in
  let c = Fut.monoid_product CCPair.make a b in
  assert_equal (1,42) (Fut.get c);
  true;;

t @@ fun () ->
  let a = Fut.make (fun () -> 1) in
  let b = Fut.map succ @@ Fut.make (fun () -> 41) in
  let c = Fut.monoid_product CCPair.make a b in
  assert_equal (1,42) (Fut.get c);
  true;;

eq [2;3] (Fut.get @@ Fut.map_l (fun x -> Fut.return (x+1)) [1;2]);;
eq [] (Fut.get @@ Fut.map_l (fun x -> Fut.return (x+1)) []);;

t @@ fun () ->
  let l = CCList.(1 -- 50) in
  let l' = l
    |> List.map
      (fun x -> Fut.make (fun () -> Thread.delay 0.1; x*10))
    |> Fut.sequence_l
    |> Fut.map (List.fold_left (+) 0)
  in
  let expected = List.fold_left (fun acc x -> acc + 10 * x) 0 l in
  assert_equal expected (Fut.get l');
  true;;

t @@ fun () ->
  let l = CCList.(1 -- 100_000) in
  let l' = l
    |> CCList.map
      (fun _x -> Fut.make (fun () -> 1))
    |> Fut.sequence_l
    |> Fut.map (List.fold_left (+) 0)
  in
  let expected = 100_000 in
  assert_equal expected (Fut.get l');
  true;;

t @@ fun () ->
  let l = CCList.(1 -- 50) in
  let l' = l
    |> List.map
      (fun x -> Fut.make (fun () -> Thread.delay 0.1; if x = 5 then raise Exit; x))
    |> Fut.sequence_l
    |> Fut.map (List.fold_left (+) 0)
  in
  assert_raises ((=)Exit) (fun () -> Fut.get l');
  true;;

t @@ fun () ->
  let rec fib x = if x<2 then 1 else fib (x-1)+fib(x-2) in
  let l =
    CCList.(1--10_000)
    |> List.rev_map
      (fun x-> Fut.make (fun () -> Thread.yield(); fib (x mod 20)))
    |> Fut.(map_l (fun x->x>|= fun x->x+1))
  in
  assert (Fut.state l = Waiting);
  let l' = Fut.get l in
  assert_equal 10_000 (List.length l');
  true;;

t @@ fun () ->
  let l = CCList.(1 -- 50) in
  let l' = l
    |> List.map
      (fun x -> Fut2.make (fun () -> Thread.delay 0.1; x*10))
    |> Fut2.sequence_l
    |> Fut2.map (List.fold_left (+) 0)
  in
  let expected = List.fold_left (fun acc x -> acc + 10 * x) 0 l in
  assert_equal expected (Fut2.get l');
  true;;

t @@ fun () ->
  let l = CCList.(1 -- 50) in
  let l' = l
    |> List.map
      (fun x -> Fut2.make (fun () -> Thread.delay 0.1; if x = 5 then raise Exit; x))
    |> Fut2.sequence_l
    |> Fut2.map (List.fold_left (+) 0)
  in
  assert_raises ((=)Exit) (fun () -> Fut2.get l');
  true;;

t @@ fun () ->
  let rec fib x = if x<2 then 1 else fib (x-1)+fib(x-2) in
  let l =
    CCList.(1--10_000)
    |> List.rev_map
      (fun x-> Fut2.make (fun () -> Thread.yield(); fib (x mod 20)))
    |> Fut2.(map_l (fun x->x>|= fun x->x+1))
  in
  assert (Fut2.state l = Waiting);
  let l' = Fut2.get l in
  assert_equal 10_000 (List.length l');
  true;;

t @@ fun () ->
  let start = Unix.gettimeofday () in
  let pause = 0.2 and n = 10 in
  let l = CCList.(1 -- n)
    |> List.map (fun _ -> Fut.make (fun () -> Thread.delay pause))
  in
  List.iter Fut.get l;
  let stop = Unix.gettimeofday () in
  assert (stop -. start < float_of_int n *. pause);
  true;;

t @@ fun () ->
  let start = Unix.gettimeofday () in
  let pause = 0.2 and n = 10 in
  let l = CCList.(1 -- n)
    |> List.map (fun _ -> Fut2.make (fun () -> Thread.delay pause))
  in
  List.iter Fut2.get l;
  let stop = Unix.gettimeofday () in
  assert (stop -. start < float_of_int n *. pause);
  true;;
