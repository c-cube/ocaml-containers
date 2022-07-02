

module Test = (val Containers_testlib.make ~__FILE__())
open Test;;
open CCLock;;

t @@ fun () ->
  let l = create 0 in
  let try_incr l =
    update l (fun x -> Thread.yield(); x+1)
  in
  for _i = 1 to 10 do ignore (Thread.create try_incr l) done;
  Thread.delay 0.10 ;
  assert_equal 10 (get l);
  true;;

t @@ fun () ->
  let l = create 0 in
  let test_it l =
    with_lock_as_ref l
      ~f:(fun r ->
        (* increment and decrement *)
        for j = 0 to 100 do
          let x = LockRef.get r in
          LockRef.set r (x+10);
          if j mod 5=0 then Thread.yield ();
          let y = LockRef.get r in
          LockRef.set r (y - 10);
        done
      )
  in
  for _i = 1 to 100 do ignore (Thread.create test_it l) done;
  Thread.delay 0.10;
  0 =get l;;

t @@ fun () -> let l = create 5 in update l (fun x->x+1); get l = 6;;
t @@ fun () -> let l = create 5 in update_map l (fun x->x+1, string_of_int x) = "5" && get l = 6;;
t @@ fun () -> let l = create 0 in set l 4; get l = 4;;
t @@ fun () -> let l = create 0 in set l 4; set l 5; get l = 5;;

t @@ fun () ->
  let l = create 0 in
  let a = Array.init 100 (fun _ -> Thread.create (fun _ -> incr l) ()) in
  Array.iter Thread.join a;
  assert_equal ~printer:CCInt.to_string 100 (get l);
  true;;

t @@ fun () -> let l = create 0 in incr l ; get l = 1;;
t @@ fun () -> let l = create 0 in decr l ; get l = ~-1;;
t @@ fun () -> let l = create 0 in 1 = incr_then_get l && 1 = get l;;
t @@ fun () -> let l = create 0 in 0 = get_then_incr l && 1 = get l;;
t @@ fun () -> let l = create 10 in 9 = decr_then_get l && 9 = get l;;
t @@ fun () -> let l = create 10 in 10 = get_then_decr l && 9 = get l;;
