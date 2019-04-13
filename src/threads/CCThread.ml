(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Threads} *)

type t = Thread.t

let spawn f = Thread.create f ()

let spawn1 f x = Thread.create f x

let spawn2 f x y = Thread.create (fun () -> f x y) ()

let detach f = ignore (Thread.create f ())

let finally_ f x ~h =
  try
    let res = f x in
    ignore (h ());
    res
  with e ->
    ignore (h());
    raise e

module Arr = struct
  let spawn n f =
    Array.init n (fun i -> Thread.create f i)

  let join a = Array.iter Thread.join a
end

(*$R
  let l = CCLock.create 0 in
  let a = Arr.spawn 101 (fun i -> CCLock.update l ((+) i)) in
  Arr.join a;
  let n = Iter.(1 -- 100 |> fold (+) 0) in
  assert_equal ~printer:CCInt.to_string n (CCLock.get l)
*)

module Barrier = struct
  type t = {
    lock: Mutex.t;
    cond: Condition.t;
    mutable activated: bool;
  }

  let create () = {
    lock=Mutex.create();
    cond=Condition.create();
    activated=false;
  }

  let with_lock_ b f =
    Mutex.lock b.lock;
    finally_ f () ~h:(fun () -> Mutex.unlock b.lock)

  let reset b = with_lock_ b (fun () -> b.activated <- false)

  let wait b =
    with_lock_ b
      (fun () ->
         while not b.activated do
           Condition.wait b.cond b.lock
         done)

  let activate b =
    with_lock_ b
      (fun () ->
         if not b.activated then (
           b.activated <- true;
           Condition.broadcast b.cond))

  let activated b = with_lock_ b (fun () -> b.activated)
end

(*$R
  let b = Barrier.create () in
  let res = CCLock.create 0 in
  let t1 = spawn (fun _ -> Barrier.wait b; CCLock.incr res)
  and t2 = spawn (fun _ -> Barrier.wait b; CCLock.incr res) in
  Thread.delay 0.2;
  assert_equal 0 (CCLock.get res);
  Barrier.activate b;
  Thread.join t1; Thread.join t2;
  assert_equal 2 (CCLock.get res)
*)
