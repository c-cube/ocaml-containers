
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCSemaphore;;

t @@ fun () ->
  let s = create 1 in
  let r = CCLock.create false in
  let _ = Thread.create (fun s -> acquire 5 s; CCLock.set r true) s in
  Thread.yield ();
  assert_equal false (CCLock.get r);
  release 4 s;
  Thread.delay 0.2;
  assert_equal true (CCLock.get r);
  assert_equal 0 (get s);
  true;;

t @@ fun () ->
  let s = create 5 in
  let n = CCLock.create 0 in
  let a = Array.init 100 (fun i ->
    Thread.create (fun _ ->
      for _i = 1 to 100 do
        with_acquire ~n:(1 + (i mod 5)) s
          ~f:(fun () -> Thread.yield(); CCLock.incr n)
      done)
    ())
  in
  Array.iter Thread.join a;
  assert_equal ~printer:CCInt.to_string 5 (get s);
  assert_equal ~printer:CCInt.to_string 10_000 (CCLock.get n);
  true;;

t @@ fun () ->
  let output _s = () in
  let s = create 2 in
  let res = CCLock.create false in
  let id = Thread.create
    (fun () ->
      output "start";
      wait_until_at_least ~n:5 s
        ~f:(fun () ->
          assert (get s >= 5);
          output "modify now";
          CCLock.set res true)
    ) ()
  in
  output "launched thread";
  Thread.yield();
  assert (not (CCLock.get res));
  output "release 2";
  release 2 s;
  Thread.yield();
  assert (not (CCLock.get res));
  output "release 1";
  release 1 s;
  (* should work now *)
  Thread.delay 0.2;
  Thread.join id;
  output "check";
  assert (CCLock.get res);
  true;;
