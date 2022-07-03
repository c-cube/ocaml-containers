(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around Mutex} *)

type 'a t = { mutex: Mutex.t; mutable content: 'a }
type 'a lock = 'a t

let create content = { mutex = Mutex.create (); content }

let with_lock l f =
  Mutex.lock l.mutex;
  try
    let x = f l.content in
    Mutex.unlock l.mutex;
    x
  with e ->
    Mutex.unlock l.mutex;
    raise e

let try_with_lock l f =
  if Mutex.try_lock l.mutex then (
    try
      let x = f l.content in
      Mutex.unlock l.mutex;
      Some x
    with e ->
      Mutex.unlock l.mutex;
      raise e
  ) else
    None

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

let mutex l = l.mutex
let update l f = with_lock l (fun x -> l.content <- f x)

let update_map l f =
  with_lock l (fun x ->
      let x', y = f x in
      l.content <- x';
      y)

let get l =
  Mutex.lock l.mutex;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let set l x =
  Mutex.lock l.mutex;
  l.content <- x;
  Mutex.unlock l.mutex

let incr l = update l Stdlib.succ
let decr l = update l Stdlib.pred

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
