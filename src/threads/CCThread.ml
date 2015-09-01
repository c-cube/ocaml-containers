
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Threads} *)

type t = Thread.t

let spawn f = Thread.create f ()

let detach f = ignore (Thread.create f ())

module Arr = struct
  let spawn n f =
    Array.init n (fun i -> Thread.create f i)

  let join a = Array.iter Thread.join a
end

(*$R
  let l = CCLock.create 0 in
  let a = Arr.spawn 101 (fun i -> CCLock.update l ((+) i)) in
  Arr.join a;
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
  let decr_size_ q = assert(q.size > 0); q.size <- q.size - 1

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
        (* if there are blocked receivers, awake one of them *)
        incr_size_ q;
        Condition.broadcast q.cond;
      )

  let take q =
    with_lock_ q
      (fun () ->
        while q.size = 0 do
          Condition.wait q.cond q.lock
        done;
        let x = Queue.take q.q in
        (* if there are blocked senders, awake one of them *)
        decr_size_ q;
        Condition.broadcast q.cond;
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

  let push_list q l =
    let is_empty_ = function [] -> true | _::_ -> false in
    (* push elements until it's not possible *)
    let rec push_ q l = match l with
      | [] -> l
      | _::_ when q.size = q.capacity -> l (* no room remaining *)
      | x :: tl ->
          Queue.push x q.q;
          incr_size_ q;
          push_ q tl
    in
    (* push chunks of [l] in [q] until [l] is empty *)
    let rec aux q l =
      if not (is_empty_ l)
      then
        let l = with_lock_ q
          (fun () ->
            while q.size = q.capacity do
              Condition.wait q.cond q.lock
            done;
            let l = push_ q l in
            Condition.broadcast q.cond;
            l
          )
        in
        aux q l
    in aux q l

  let take_list q n =
    (* take at most [n] elements of [q] and prepend them to [acc] *)
    let rec pop_ acc q n =
      if n=0 || Queue.is_empty q.q then acc, n
      else ( (* take next element *)
        let x = Queue.take q.q in
        decr_size_ q;
        pop_ (x::acc) q (n-1)
      )
    in
    (* call [pop_] until [n] elements have been gathered *)
    let rec aux acc q n =
      if n=0 then List.rev acc
      else
        let acc, n = with_lock_ q
          (fun () ->
            while q.size = 0 do
              Condition.wait q.cond q.lock
            done;
            let acc, n = pop_ acc q n in
            Condition.broadcast q.cond;
            acc, n
          )
        in
        aux acc q n
    in
    aux [] q n

  (*$R
    let n = 1000 in
    let lists = [| CCList.(1 -- n) ; CCList.(n+1 -- 2*n); CCList.(2*n+1 -- 3*n) |] in
    let q = Queue.create 2 in
    let senders = Arr.spawn 3
      (fun i ->
        if i=1
        then Queue.push_list q lists.(i)  (* test push_list *)
        else List.iter (Queue.push q) lists.(i)
      )
    in
    let res = CCLock.create [] in
    let receivers = Arr.spawn 3
      (fun i ->
        if i=1 then
          let l = Queue.take_list q n in
          CCLock.update res (fun acc -> l @ acc)
        else
          for _j = 1 to n do
            let x = Queue.take q in
            CCLock.update res (fun acc -> x::acc)
          done
      )
    in
    Arr.join senders; Arr.join receivers;
    let l = CCLock.get res |> List.sort Pervasives.compare in
    assert_equal CCList.(1 -- 3*n) l
  *)

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



