(** {1 Semaphores} *)

type t = {
  mutable n : int;
  mutex : Mutex.t;
  cond : Condition.t;
}

let create n =
  if n <= 0 then invalid_arg "Semaphore.create";
  { n;
    mutex=Mutex.create();
    cond=Condition.create();
  }

let get t = t.n

(* assume [t.mutex] locked, try to acquire [t] *)
let acquire_once_locked_ m t =
  while t.n < m do
    Condition.wait t.cond t.mutex;
  done;
  assert (t.n >= m);
  t.n <- t.n - m;
  Condition.broadcast t.cond;
  Mutex.unlock t.mutex

let acquire m t =
  Mutex.lock t.mutex;
  acquire_once_locked_ m t

(* assume [t.mutex] locked, try to release [t] *)
let release_once_locked_ m t =
  t.n <- t.n + m;
  Condition.broadcast t.cond;
  Mutex.unlock t.mutex

let release m t =
  Mutex.lock t.mutex;
  release_once_locked_ m t;
  ()

(*$R
  let s = create 1 in
  let r = CCLock.create false in
  let _ = Thread.create (fun s -> acquire 5 s; CCLock.set r true) s in
  Thread.yield ();
  assert_equal false (CCLock.get r);
  release 4 s;
  Thread.delay 0.2;
  assert_equal true (CCLock.get r);
  assert_equal 0 (get s)
*)

let with_acquire ~n t ~f =
  Mutex.lock t.mutex;
  acquire_once_locked_ n t;
  try
    let x = f() in
    release_once_locked_ n t;
    x
  with e ->
    release_once_locked_ n t;
    raise e

(*$R
  let s = create 5 in
  let n = CCLock.create 0 in
  let a = Array.init 100 (fun i ->
    Thread.create (fun _ ->
      with_acquire ~n:(1 + (i mod 5)) s
        ~f:(fun () -> CCLock.incr n)
    ) ())
  in
  Array.iter Thread.join a;
  assert_equal ~printer:CCInt.to_string 5 (get s);
  assert_equal ~printer:CCInt.to_string 100 (CCLock.get n)
*)

let wait_until_at_least ~n t ~f =
  Mutex.lock t.mutex;
  while t.n < n do
    Condition.wait t.cond t.mutex;
  done;
  assert (t.n >= n);
  Mutex.unlock t.mutex;
  f ()

(*$R
  let output s = () in
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
  assert_bool "start" (not (CCLock.get res));
  output "release 2";
  release 2 s;
  Thread.yield();
  assert_bool "after release 2" (not (CCLock.get res));
  output "release 1";
  release 1 s;
  (* should work now *)
  Thread.delay 0.2;
  Thread.join id;
  output "check";
  assert_bool "after release 1" (CCLock.get res)
*)
