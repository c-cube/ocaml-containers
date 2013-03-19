(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Futures for concurrency} *)

type 'a t = {
  mutable content : 'a result;
  mutex : Mutex.t;
  condition : Condition.t;
} (** A future value of type 'a *)
and 'a result =
  | NotKnown
  | Success of 'a
  | Failure of exn
  (** Result of a computation *)

exception SendTwice
  (** Exception raised when a future is evaluated several time *)

(** {2 Thread pool} *)
module Pool = struct
  type t = {
    mutable threads : Thread.t list;
    size : int;
    jobs : (unit -> unit) Queue.t;
    mutex : Mutex.t;
    condition : Condition.t;
  } (** A pool of threads *)

  (* TODO option to allow the pool to grow on demand? *)

  (* Internal function, which is run by the threads of the pool *)
  let serve pool =
    (* loop, to get the next job *)
    let rec poll () =
      Mutex.lock pool.mutex;
      Condition.wait pool.condition pool.mutex;
      if Queue.is_empty pool.jobs
        then begin (* caramba! try again *)
          Mutex.unlock pool.mutex;
          poll () end
        else begin
          let job = Queue.pop pool.jobs in
          Mutex.unlock pool.mutex;
          (* run the job *)
          (try
            job ()
          with _ ->
            ());
          poll ()  (* recurse *)
        end
    in
    poll ()

  (** Create a pool with the given number of threads. *)
  let create ~size =
    let pool = {
      threads = [];
      size;
      jobs = Queue.create ();
      mutex = Mutex.create ();
      condition = Condition.create ();
    } in
    (* start threads *)
    for i = 0 to size - 1 do
      pool.threads <- (Thread.create serve pool) :: pool.threads;
    done;
    pool

  (** Schedule a function to run in the pool *)
  let schedule pool f =
    Mutex.lock pool.mutex;
    Queue.push f pool.jobs;
    Condition.signal pool.condition; (* wake up one thread *)
    Mutex.unlock pool.mutex;
    ()

  (** Kill threads in the pool *)
  let finish pool =
    List.iter (fun t -> Thread.kill t) pool.threads
end

let default_pool = Pool.create 3
  (** Default pool of threads *)

(** {2 Basic Future functions} *)

let make () =
  { content = NotKnown;
    mutex = Mutex.create ();
    condition = Condition.create ();
  }

let get future =
  (* check whether it's finished: precond: mutex is locked *)
  let rec check () =
    match future.content with
    | NotKnown ->
      poll ()  (* wait *)
    | Success x ->
      Mutex.unlock future.mutex;
      x (* return success *)
    | Failure e ->
      Mutex.unlock future.mutex;
      raise e (* raise exception *)
  (* poll, to wait for the result to arrive. Precond: mutex is acquired. *)
  and poll () =
    Condition.wait future.condition future.mutex;
    check ()  (* we have been signaled, check! *)
  in
  Mutex.lock future.mutex;
  check () 

let send future x =
  Mutex.lock future.mutex;
  match future.content with
  | NotKnown ->  (* set content and signal *)
    future.content <- Success x;
    Condition.broadcast future.condition;
    Mutex.unlock future.mutex
  | _ ->
    Mutex.unlock future.mutex;
    raise SendTwice  (* already set! *)

let fail future e =
  Mutex.lock future.mutex;
  match future.content with
  | NotKnown ->  (* set content and signal *)
    future.content <- Failure e;
    Condition.broadcast future.condition;
    Mutex.unlock future.mutex
  | _ ->
    Mutex.unlock future.mutex;
    raise SendTwice  (* already set! *)

let is_done future =
  Mutex.lock future.mutex;
  match future.content with
  | NotKnown ->
    Mutex.unlock future.mutex;
    false
  | _ ->
    Mutex.unlock future.mutex;
    true

(** {2 Combinators *)

let flatMap ?(pool=default_pool) f future =
  let future' = make () in
  (* schedule the task that waits for [future] to return [x], then
     computes [f x] and send the result to [future'] *)
  Pool.schedule pool
    (fun () ->
      try
        let x = get future in
        let future'' = f x in
        let y = get future'' in
        send future' y
      with e -> (* failure occurred *)
        fail future' e);
  future'

(** {2 Future constructors} *)

let return x =
  { content = Success x;
    mutex = Mutex.create ();
    condition = Condition.create ();
  }

let spawn ?(pool=default_pool) f =
  let future = make () in
  (* schedule computation *)
  Pool.schedule pool
    (fun () ->
      try
        let x = f () in
        send future x
      with e ->
        fail future e);
  future

(** slurp the entire content of the file_descr into a string *)
let slurp i_chan =
  let buf_size = 128 in
  let content = Buffer.create 120
  and buf = String.make 128 'a' in
  let rec next () =
    let num = input i_chan buf 0 buf_size in
    if num = 0
      then Buffer.contents content (* EOF *)
      else (Buffer.add_substring content buf 0 num; next ())
  in next ()

(** Spawn a sub-process with the given command [cmd] (and possibly input);
    returns a future containing (returncode, stdout, stderr) *)
let spawn_process ?(pool=default_pool) ?(stdin="") ~cmd =
  spawn ~pool
    (fun () ->
      (* spawn subprocess *)
      let out, inp, err = Unix.open_process_full cmd (Unix.environment ()) in
      output_string inp stdin;
      (* send stdin to command *)
      flush inp;
      (* read output of process *)
      let out' = slurp out in
      let err' = slurp err in
      (* wait for termination *)
      let status = Unix.close_process_full (out,inp,err) in
      (* get return code *)
      let returncode = match status with
        | Unix.WEXITED i -> i
        | Unix.WSIGNALED i -> i
        | Unix.WSTOPPED i -> i in
      (returncode, out', err'))

module Infix = struct
  let (>>=) x f = flatMap f x
end
