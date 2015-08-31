
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)


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

module LockRef = struct
  type 'a t = 'a lock
  let get t = t.content
  let set t x = t.content <- x
  let update t f = t.content <- f t.content
end

let with_lock_as_ref l f =
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
      (fun r ->
        let x = LockRef.get r in
        LockRef.set r (x+10);
        Thread.yield ();
        let y = LockRef.get r in
        LockRef.set r (y - 10);
      )
  in
  for i = 1 to 100 do ignore (Thread.create test_it l) done;
  Thread.delay 0.10;
  assert_equal 0 (get l)
*)

let mutex l = l.mutex

let update l f =
  with_lock l (fun x -> l.content <- f x)

let get l =
  Mutex.lock l.mutex;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let incr l = update l (fun x -> x+1)

let decr l = update l (fun x -> x-1)

