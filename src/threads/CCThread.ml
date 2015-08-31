
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Threads} *)

type t = Thread.t

let spawn f = Thread.create f ()

let detach f = ignore (Thread.create f ())

module Array = struct
  let spawn n f =
    Array.init n (fun i -> Thread.create f i)

  let join a = Array.iter Thread.join a
end

(*$R
  let l = CCLock.create 0 in
  let a = Array.spawn 101 (fun i -> CCLock.update l ((+) i)) in
  Array.join a;
  let n = Sequence.(1 -- 100 |> fold (+) 0) in
  assert_equal ~printer:CCInt.to_string n (CCLock.get l)
*)

module Queue = struct
  type 'a t = {
    q : 'a Queue.t;
    lock : Mutex.t;
    cond : Condition.t;
    capacity : int;
    mutable size : int;
  }

  let create n =
    if n < 1 then invalid_arg "CCThread.Queue.create";
    let q = {
      q=Queue.create();
      lock=Mutex.create();
      cond=Condition.create();
      capacity=n;
      size=0;
    } in
    q
      
  let incr_size_ q = assert(q.size < q.capacity); q.size <- q.size + 1
  let decr_size_ q = q.size <- q.size - 1

  let with_lock_ q f =
    Mutex.lock q.lock;
    try
      let x = f () in
      Mutex.unlock q.lock;
      x
    with e ->
      Mutex.unlock q.lock;
      raise e

  let push q x =
    with_lock_ q
      (fun () ->
        while q.size = q.capacity do
          Condition.wait q.cond q.lock
        done;
        assert (q.size < q.capacity);
        Queue.push x q.q;
        if q.size = 0 then Condition.signal q.cond;
        incr_size_ q;
      )

  let take q =
    with_lock_ q
      (fun () ->
        while q.size = 0 do
          Condition.wait q.cond q.lock
        done;
        let x = Queue.take q.q in
        if q.size = q.capacity then Condition.signal q.cond;
        decr_size_ q;
        x
      )

  (*$R
    let q = Queue.create 1 in
    let t1 = spawn (fun () -> Queue.push q 1; Queue.push q 2) in
    let t2 = spawn (fun () -> Queue.push q 3; Queue.push q 4) in
    let l = CCLock.create [] in
    let t3 = spawn (fun () -> for i = 1 to 4 do
        let x = Queue.take q in
        CCLock.update l (fun l -> x :: l)
      done)
    in
    Thread.join t1; Thread.join t2; Thread.join t3;
    assert_equal [1;2;3;4] (List.sort Pervasives.compare (CCLock.get l))
  *)

  (* TODO: more efficient versions (push or pop several items at once when possible) *)

  let push_list q l = List.iter (push q) l

  let rec take_list q n =
    if n=0 then []
    else
      let x = take q in
      x :: take_list q (n-1)

  let try_take q =
    with_lock_ q
      (fun () ->
        if q.size > 0
          then (
            decr_size_ q;
            Some (Queue.take q.q)
          ) else None
      )

  let try_push q x =
    with_lock_ q
      (fun () ->
        if q.size < q.capacity
          then (
            incr_size_ q;
            Queue.push x q.q;
            Condition.signal q.cond;
            true
          ) else false
      )

  let peek q =
    with_lock_ q
      (fun () ->
         try Some (Queue.peek q.q) with Queue.Empty -> None
      )

  let size q = with_lock_ q (fun () -> q.size)

  let capacity q = q.capacity
end



