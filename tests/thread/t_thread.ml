module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCThread;;

t @@ fun () ->
let l = CCLock.create 0 in
let a = Arr.spawn 101 (fun i -> CCLock.update l (( + ) i)) in
Arr.join a;
let n = Iter.(1 -- 100 |> fold ( + ) 0) in
assert_equal ~printer:CCInt.to_string n (CCLock.get l);
true
;;

t @@ fun () ->
let b = Barrier.create () in
let res = CCLock.create 0 in
let t1 =
  spawn (fun _ ->
      Barrier.wait b;
      CCLock.incr res)
and t2 =
  spawn (fun _ ->
      Barrier.wait b;
      CCLock.incr res)
in
Thread.delay 0.2;
assert_equal 0 (CCLock.get res);
Barrier.activate b;
Thread.join t1;
Thread.join t2;
assert_equal 2 (CCLock.get res);
true
