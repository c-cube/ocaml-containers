
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around Mutex} *)

type 'a t = {
  mutex : Mutex.t;
  mutable content : 'a;
}

type 'a lock = 'a t

let create content = {
  mutex = Mutex.create();
  content;
}

let with_lock l f =
  Mutex.lock l.mutex;
  try
    let x = f l.content in
    Mutex.unlock l.mutex;
    x
  with e ->
    Mutex.unlock l.mutex;
    raise e

(*$R
  let l = create 0 in
  let try_incr l =
    update l (fun x -> Thread.yield(); x+1)
  in
  for i = 1 to 10 do ignore (Thread.create try_incr l) done;
  Thread.delay 0.10 ;
  assert_equal 10 (get l)
*)

let try_with_lock l f =
  if Mutex.try_lock l.mutex
  then
    try
      let x = f l.content in
      Mutex.unlock l.mutex;
      Some x
    with e ->
      Mutex.unlock l.mutex;
      raise e
  else None

module LockRef = struct
  type 'a t = 'a lock
  let get t = t.content
  let set t x = t.content <- x
  let update t f = t.content <- f t.content
end

let with_lock_as_ref l ~f =
  Mutex.lock l.mutex;
  try
    let x = f l in
    Mutex.unlock l.mutex;
    x
  with e ->
    Mutex.unlock l.mutex;
    raise e

(*$R
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
  for i = 1 to 100 do ignore (Thread.create test_it l) done;
  Thread.delay 0.10;
  assert_equal 0 (get l)
*)

let mutex l = l.mutex

let update l f =
  with_lock l (fun x -> l.content <- f x)

(*$T
  let l = create 5 in update l (fun x->x+1); get l = 6
*)

let update_map l f =
  with_lock l
    (fun x ->
       let x', y = f x in
       l.content <- x';
       y)

(*$T
  let l = create 5 in update_map l (fun x->x+1, string_of_int x) = "5" && get l = 6
*)

let get l =
  Mutex.lock l.mutex;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let set l x =
  Mutex.lock l.mutex;
  l.content <- x;
  Mutex.unlock l.mutex

(*$T
  let l = create 0 in set l 4; get l = 4
  let l = create 0 in set l 4; set l 5; get l = 5
*)

let incr l = update l Stdlib.succ

let decr l = update l Stdlib.pred


(*$R
  let l = create 0 in
  let a = Array.init 100 (fun _ -> Thread.create (fun _ -> incr l) ()) in
  Array.iter Thread.join a;
  assert_equal ~printer:CCInt.to_string 100 (get l)
*)

(*$T
  let l = create 0 in incr l ; get l = 1
  let l = create 0 in decr l ; get l = ~-1
*)

let incr_then_get l =
  Mutex.lock l.mutex;
  l.content <- l.content + 1;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let get_then_incr l =
  Mutex.lock l.mutex;
  let x = l.content in
  l.content <- l.content + 1;
  Mutex.unlock l.mutex;
  x

let decr_then_get l =
  Mutex.lock l.mutex;
  l.content <- l.content - 1;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let get_then_decr l =
  Mutex.lock l.mutex;
  let x = l.content in
  l.content <- l.content - 1;
  Mutex.unlock l.mutex;
  x

(*$T
  let l = create 0 in 1 = incr_then_get l && 1 = get l
  let l = create 0 in 0 = get_then_incr l && 1 = get l
  let l = create 10 in 9 = decr_then_get l && 9 = get l
  let l = create 10 in 10 = get_then_decr l && 9 = get l
*)

let get_then_set l =
  Mutex.lock l.mutex;
  let x = l.content in
  l.content <- true;
  Mutex.unlock l.mutex;
  x

let get_then_clear l =
  Mutex.lock l.mutex;
  let x = l.content in
  l.content <- false;
  Mutex.unlock l.mutex;
  x

