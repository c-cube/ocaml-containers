(** {1 Semaphores} *)

type t = { mutable n: int; mutex: Mutex.t; cond: Condition.t }

let create n =
  if n <= 0 then invalid_arg "Semaphore.create";
  { n; mutex = Mutex.create (); cond = Condition.create () }

let get t = t.n

(* assume [t.mutex] locked, try to acquire [t] *)
let acquire_once_locked_ m t =
  while t.n < m do
    Condition.wait t.cond t.mutex
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

let with_acquire ~n t ~f =
  acquire n t;
  try
    let x = f () in
    release n t;
    x
  with e ->
    release n t;
    raise e

let wait_until_at_least ~n t ~f =
  Mutex.lock t.mutex;
  while t.n < n do
    Condition.wait t.cond t.mutex
  done;
  assert (t.n >= n);
  Mutex.unlock t.mutex;
  f ()
