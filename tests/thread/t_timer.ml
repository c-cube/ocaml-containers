
module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCTimer;;


(* NOTE: could be tighter bounds, but travis' mac OS seems to be dog slow. *)

t @@ fun () ->
  let start = Unix.gettimeofday() in
  let timer = create() in
  let res = CCLock.create 0 in
  let sem = CCSemaphore.create 1 in
  CCSemaphore.acquire 1 sem;
  let stop = ref 0. in
  every timer 0.1
    ~f:(fun () ->
      if CCLock.incr_then_get res > 5 then (
        stop := Unix.gettimeofday();
        CCSemaphore.release 1 sem;
        raise ExitEvery
      ));
  CCSemaphore.acquire 1 sem; (* wait *)
  assert_equal ~printer:CCInt.to_string 6 (CCLock.get res);
  assert (!stop -. start >= 0.49999);
  assert (!stop -. start < 2.);
  true;;

t @@ fun () ->
  (* scenario:  n := 1; n := n*4 ; n := n+2; res := n *)
  let timer = create () in
  let n = CCLock.create 1 in
  let res = CCLock.create 0 in
  after timer 0.3
    ~f:(fun () -> CCLock.update n (fun x -> x+2));
  ignore (Thread.create
    (fun _ -> Thread.delay 0.4; CCLock.set res (CCLock.get n)) ());
  after timer 0.1
    ~f:(fun () -> CCLock.update n (fun x -> x * 4));
  Thread.delay 0.6 ;
  assert_equal ~printer:Q.Print.int 6 (CCLock.get res);
  true;;
