(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Blocking Queue} *)

type 'a t = {
  q: 'a Queue.t;
  lock: Mutex.t;
  cond: Condition.t;
  capacity: int;
  mutable size: int;
}

let create n =
  if n < 1 then invalid_arg "BloquingQueue.create";
  let q =
    {
      q = Queue.create ();
      lock = Mutex.create ();
      cond = Condition.create ();
      capacity = n;
      size = 0;
    }
  in
  q

let incr_size_ q =
  assert (q.size < q.capacity);
  q.size <- q.size + 1

let decr_size_ q =
  assert (q.size > 0);
  q.size <- q.size - 1

let finally_ f x ~h =
  try
    let res = f x in
    ignore (h ());
    res
  with e ->
    ignore (h ());
    raise e

let with_lock_ q f =
  Mutex.lock q.lock;
  finally_ f () ~h:(fun () -> Mutex.unlock q.lock)

let push q x =
  with_lock_ q (fun () ->
      while q.size = q.capacity do
        Condition.wait q.cond q.lock
      done;
      assert (q.size < q.capacity);
      Queue.push x q.q;
      (* if there are blocked receivers, awake one of them *)
      incr_size_ q;
      Condition.broadcast q.cond)

let take q =
  with_lock_ q (fun () ->
      while q.size = 0 do
        Condition.wait q.cond q.lock
      done;
      let x = Queue.take q.q in
      (* if there are blocked senders, awake one of them *)
      decr_size_ q;
      Condition.broadcast q.cond;
      x)

let push_list q l =
  (* push elements until it's not possible.
     Assumes the lock is acquired. *)
  let rec push_ q l =
    match l with
    | [] -> l
    | _ :: _ when q.size = q.capacity -> l (* no room remaining *)
    | x :: tl ->
      Queue.push x q.q;
      incr_size_ q;
      push_ q tl
  in
  (* push chunks of [l] in [q] until [l] is empty *)
  let rec aux q l =
    match l with
    | [] -> ()
    | _ :: _ ->
      let l =
        with_lock_ q (fun () ->
            while q.size = q.capacity do
              Condition.wait q.cond q.lock
            done;
            let l = push_ q l in
            Condition.broadcast q.cond;
            l)
      in
      aux q l
  in
  aux q l

let take_list q n =
  (* take at most [n] elements of [q] and prepend them to [acc] *)
  let rec pop_ acc q n =
    if n = 0 || Queue.is_empty q.q then
      acc, n
    else (
      (* take next element *)
      let x = Queue.take q.q in
      decr_size_ q;
      pop_ (x :: acc) q (n - 1)
    )
  in
  (* call [pop_] until [n] elements have been gathered *)
  let rec aux acc q n =
    if n = 0 then
      List.rev acc
    else (
      let acc, n =
        with_lock_ q (fun () ->
            while q.size = 0 do
              Condition.wait q.cond q.lock
            done;
            let acc, n = pop_ acc q n in
            Condition.broadcast q.cond;
            acc, n)
      in
      aux acc q n
    )
  in
  aux [] q n

let try_take q =
  with_lock_ q (fun () ->
      if q.size = 0 then
        None
      else (
        decr_size_ q;
        Some (Queue.take q.q)
      ))

let try_push q x =
  with_lock_ q (fun () ->
      if q.size = q.capacity then
        false
      else (
        incr_size_ q;
        Queue.push x q.q;
        Condition.signal q.cond;
        true
      ))

let peek q =
  with_lock_ q (fun () -> try Some (Queue.peek q.q) with Queue.Empty -> None)

let size q = with_lock_ q (fun () -> q.size)
let capacity q = q.capacity
