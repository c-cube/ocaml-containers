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
  mutable handlers : 'a handler list;   (* handlers *)
  mutex : Mutex.t;
  condition : Condition.t;
} (** A future value of type 'a *)
and 'a result =
  | NotKnown
  | Success of 'a
  | Failure of exn
  (** Result of a computation *)
and 'a handler =
  | OnSuccess of ('a -> unit)
  | OnFailure of (exn -> unit)
  | OnFinish of (unit -> unit)

exception SendTwice
  (** Exception raised when a future is evaluated several time *)

(** {2 Thread pool} *)
module Pool = struct
  type t = {
    mutable threads : Thread.t list;
    mutable stop : bool;
    size : int;
    max_load : int;
    jobs : (unit -> unit) Queue.t;
    mutex : Mutex.t;
    condition : Condition.t;
  } (** A pool of threads *)

  (* TODO option to allow the pool to grow on demand? *)

  let load pool =
    Mutex.lock pool.mutex;
    let n = Queue.length pool.jobs in
    Mutex.unlock pool.mutex;
    n

  (* Internal function, which is run by the threads of the pool *)
  let serve pool limit =
    (* loop, to get the next job *)
    let rec poll limit =
      Mutex.lock pool.mutex;
      Condition.wait pool.condition pool.mutex;
      if Queue.is_empty pool.jobs
        then begin (* caramba! try again *)
          Mutex.unlock pool.mutex;
          if not pool.stop then poll limit end
        else begin
          let job = Queue.pop pool.jobs in
          Mutex.unlock pool.mutex;
          (* run the job *)
          (try
            job ()
          with _ ->
            ());
          match limit with
          | None -> if not pool.stop then poll limit (* I am immortal! *)
          | Some 0 -> ()  (* stop, reached limit *)
          | Some n -> if not pool.stop then poll (Some (n-1)) (* continue serving *)
        end
    in
    poll limit

  (** Add a thread to the pool, that will serve at most [limit] jobs *)
  let add_thread ?limit pool =
    let t = Thread.create (serve pool) limit in
    (* transient threads are not stored *)
    if limit = None
      then pool.threads <- t :: pool.threads

  (** Create a pool with the given number of threads. *)
  let create ?(max_load=max_int) ~size =
    let pool = {
      threads = [];
      stop = false;
      size;
      max_load;
      jobs = Queue.create ();
      mutex = Mutex.create ();
      condition = Condition.create ();
    } in
    (* start persistent threads *)
    for i = 0 to size - 1 do
      add_thread pool
    done;
    pool

  let transient_thread_lifetime = 10

  (** Schedule a function to run in the pool *)
  let schedule pool f =
    Mutex.lock pool.mutex;
    Queue.push f pool.jobs;
    (* grow set of threads, if needed *)
    (if Queue.length pool.jobs > pool.max_load
      then begin
        add_thread ~limit:transient_thread_lifetime pool
      end);
    Condition.signal pool.condition; (* wake up one thread *)
    Mutex.unlock pool.mutex;
    ()

  (** Kill threads in the pool *)
  let finish pool =
    Mutex.lock pool.mutex;
    pool.stop <- true;
    Condition.broadcast pool.condition;
    Mutex.unlock pool.mutex;
    (* kill immortal threads *)
    List.iter (fun t -> Thread.kill t) pool.threads
end

let default_pool = Pool.create ~max_load:500 ~size:3
  (** Default pool of threads (growable) *)

(** {2 MVar: a zero-or-one element thread-safe box} *)

module MVar = struct
  type 'a t = {
    mutable content : 'a option;
    mutex : Mutex.t;
    on_take : Condition.t;  (* signal that a value was removed (empty) *)
    on_put : Condition.t;   (* signal that a value was added (full) *)
  }

  (** Create an empty box *)
  let empty () = {
    content = None;
    mutex = Mutex.create ();
    on_take = Condition.create ();
    on_put = Condition.create ();
  }

  (** Create a full box *)
  let full x = {
    content = Some x;
    mutex = Mutex.create ();
    on_take = Condition.create ();
    on_put = Condition.create ();
  }

  (** Is the box currently empty? *)
  let is_empty box =
    Mutex.lock box.mutex;
    let ans = box.content <> None in
    Mutex.unlock box.mutex;
    ans

  (* assuming we have a lock on given box, wait it gets a value and return it *)
  let rec wait_put box =
    match box.content with
    | None ->
      Condition.wait box.on_put box.mutex;
      wait_put box  (* try again *)
    | Some x -> x

  (* same, but waits for the box to become empty *)
  let rec wait_take box =
    match box.content with
    | None -> ()  (* empty! *)
    | Some _ ->
      Condition.wait box.on_take box.mutex;
      wait_take box  (* try again *)

  (** Take value out of the box. Wait if necessary *)
  let take box =
    Mutex.lock box.mutex;
    let x = wait_put box in
    box.content <- None;
    Condition.broadcast box.on_take;
    Mutex.unlock box.mutex;
    x

  (** Put a value in the box. Waits if the box is already full *)
  let put box x =
    Mutex.lock box.mutex;
    wait_take box;
    box.content <- Some x;
    Condition.broadcast box.on_put;
    Mutex.unlock box.mutex

  (** Use given function to atomically update content, and return
      the previous value and the new one *)
  let update box f =
    Mutex.lock box.mutex;
    let x = wait_put box in
    try
      let y = f x in
      box.content <- Some y;
      Condition.broadcast box.on_put;  (* signal write *)
      Mutex.unlock box.mutex;
      x, y
    with e ->
      Mutex.unlock box.mutex;
      raise e 

  (** Look at the value, without removing it *)
  let peek box =
    Mutex.lock box.mutex;
    let x = wait_put box in
    Mutex.unlock box.mutex;
    x
end

(** {2 Basic Future functions} *)

let make () =
  { content = NotKnown;
    handlers = [];
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
    List.iter
      (function
        | OnSuccess f -> f x
        | OnFinish f -> f ()
        | OnFailure _ -> ())
      future.handlers;
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
    List.iter
      (function
        | OnSuccess _ -> ()
        | OnFinish f -> f ()
        | OnFailure f -> f e)
      future.handlers;
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

let on_success future k =
  Mutex.lock future.mutex;
  (match future.content with
  | NotKnown ->
    future.handlers <- (OnSuccess k) :: future.handlers; (* wait *)
  | Success x -> k x
  | Failure _ -> ());
  Mutex.unlock future.mutex

let on_failure future k =
  Mutex.lock future.mutex;
  (match future.content with
  | NotKnown ->
    future.handlers <- (OnFailure k) :: future.handlers; (* wait *)
  | Success _ -> ()
  | Failure e -> k e);
  Mutex.unlock future.mutex

let on_finish future k =
  Mutex.lock future.mutex;
  (match future.content with
  | NotKnown ->
    future.handlers <- (OnFinish k) :: future.handlers; (* wait *)
  | Success _ | Failure _ -> k ());
  Mutex.unlock future.mutex

let flatMap f future =
  let future' = make () in
  (* if [future] succeeds with [x], we spawn a new job to compute [f x] *)
  on_success future
    (fun x ->
      try
        let future'' = f x in
        on_success future'' (fun x -> send future' x);
        on_failure future'' (fun e -> fail future' e);
      with e ->
        fail future' e);
  on_failure future
    (fun e -> fail future' e);
  future'

let andThen future f =
  flatMap (fun _ -> f ()) future

let sequence futures =
  let a = Array.of_list futures in
  let n = Array.length a in
  let results = Array.make n NotKnown in
  let future' = make () in
  (* state: how many remain to finish *)
  let count = MVar.full (Array.length a) in
  (* when all futures returned, collect results for future' *)
  let check_at_end () =
    let l = Array.to_list results in
    try
      let l = List.map
        (function
          | Success x -> x
          | Failure e -> raise e
          | NotKnown -> assert false)
        l in
      send future' l
    with e ->
      fail future' e
  in
  (* function called whenever a future succeeds *)
  let one_succeeded i x =
    results.(i) <- Success x;
    let _, n = MVar.update count (fun x -> x-1) in
    if n = 0 then check_at_end ()
  and one_failed i e =
    results.(i) <- Failure e;
    let _, n = MVar.update count (fun x -> x-1) in
    if n = 0 then check_at_end ()
  in
  (* wait for all to succeed or fail *)
  for i = 0 to Array.length a - 1 do
    on_success a.(i) (one_succeeded i);
    on_failure a.(i) (one_failed i);
  done;
  future'

let choose futures =
  failwith "not implemented"

let map f future =
  let future' = make () in
  on_success future (fun x -> let y = f x in send future' y);
  on_failure future (fun e -> fail future' e);
  future'

(** {2 Future constructors} *)

let return x =
  { content = Success x;
    handlers = [];
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
      close_out inp;
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

let sleep ?(pool=default_pool) time =
  spawn ~pool
    (fun () -> Unix.sleep time; ())

module Infix = struct
  let (>>=) x f = flatMap f x
  let (>>) a f = andThen a f
end
