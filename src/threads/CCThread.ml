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
