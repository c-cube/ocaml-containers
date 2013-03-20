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

(** {2 Thread pool} *)
module Pool = struct
  type t = {
    mutable stop : bool;  (* indicate that threads should stop *)
    mutex : Mutex.t;
    jobs : job Queue.t;  (* waiting jobs *)
    mutable threads : waiting_thread list;  (* waiting threads *)
    mutable cur_size : int;
    max_size : int;
    timeout : float; (* idle time after which to discard threads *)
  } (** Dynamic, growable thread pool *)
  and job = unit -> unit
  and command =
    | Perform of job
    | Quit
    (** Command sent to a thread *)
  and waiting_thread = float * command MVar.t

  (** Cleanup waiting threads. precond: pool is locked *)
  let cleanup_waiting pool =
    let l = pool.threads in
    let now = Unix.gettimeofday () in
    (* filter threads that have been waiting for too long *)
    let l' = List.filter
      (fun (time, box) ->
        if time +. pool.timeout < now
          then (MVar.put box Quit; false)
          else true)
      l in
    pool.threads <- l'

  (** Function that the threads run. They also take a MVar to
      get commands *)
  let serve pool box =
    (* wait for a job to come *)
    let rec wait_job () =
      match MVar.take box with
      | Quit -> (Mutex.lock pool.mutex; quit ())  (* exit *)
      | Perform job ->
        run_job job
    (* run the given job *)
    and run_job job =
      (try job () with _ -> ());
      next () (* loop *)
    (* process next task *)
    and next () =
      Mutex.lock pool.mutex;
      if pool.stop then quit ()  (* stop the pool *)
      else if Queue.is_empty pool.jobs
        then begin
          let now = Unix.gettimeofday () in
          (* cleanup waiting threads *)
          cleanup_waiting pool;
          if pool.cur_size > 1 && List.length pool.threads + 1 = pool.cur_size
            then
              (* all other threads are waiting, we may need to kill them later *) 
              (Mutex.unlock pool.mutex; delay ()) 
            else begin
              (* add oneself to the list of waiting threads *)
              pool.threads <- (now, box) :: pool.threads;
              Mutex.unlock pool.mutex;
              wait_job ()
            end
        end else
          let job = Queue.pop pool.jobs in
          Mutex.unlock pool.mutex;
          run_job job
    (* delay [pool.timeout], so that in case no job is submitted we
       still kill old cached threads *)
    and delay () =
      Thread.delay pool.timeout;
      next ()
    (* stop the thread (assume we have pool.mutex) *)
    and quit () =
      pool.cur_size <- pool.cur_size - 1;
      Mutex.unlock pool.mutex
    in wait_job ()

  let size pool =
    Mutex.lock pool.mutex;
    let n = pool.cur_size in
    Mutex.unlock pool.mutex;
    n

  (** Add a thread to the pool, starting with the first job *)
  let add_thread pool job =
    let box = MVar.full job in
    ignore (Thread.create (serve pool) box)

  (** Create a pool with at most the given number of threads. [timeout]
      is the time after which idle threads are killed. *)
  let create ?(timeout=30.) ~size =
    let pool = {
      stop = false;
      cur_size = 0;
      max_size=size;
      timeout;
      threads = [];
      jobs = Queue.create ();
      mutex = Mutex.create ();
    } in
    pool

  (** Run the job in the given pool *)
  let run pool job =
    assert (not (pool.stop));
    Mutex.lock pool.mutex;
    begin match pool.threads with
    | [] when pool.cur_size = pool.max_size ->
      (* max capacity reached, push task in queue *)
      Queue.push job pool.jobs
    | [] ->
      (* spawn a thread for the given task *)
      add_thread pool (Perform job);
      pool.cur_size <- pool.cur_size + 1;
    | (_,box)::l' ->
      (* use the first thread *)
      MVar.put box (Perform job);
      pool.threads <- l';
    end;
    Mutex.unlock pool.mutex

  (** Kill threads in the pool *)
  let finish pool =
    Mutex.lock pool.mutex;
    pool.stop <- true;
    (* kill waiting threads *)
    List.iter (fun (_,box) -> MVar.put box Quit) pool.threads;
    pool.threads <- [];
    Mutex.unlock pool.mutex
end

let default_pool = Pool.create ?timeout:None ~size:100
  (** Default pool of threads, should be ok for most uses. *)

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
  let future' = make () in
  let one_finished = MVar.full false in
  (* handlers. The first handler to be called will update [one_finished]
     to true, see that it was false (hence know it is the first)
     and propagate its result to [future'] *)
  let one_succeeded x =
    let one_finished, _ = MVar.update one_finished (fun _ -> true) in
    if not one_finished then send future' x
  and one_failed e =
    let one_finished, _ = MVar.update one_finished (fun _ -> true) in
    if not one_finished then fail future' e
  in
  (* add handlers to all futures *)
  List.iter
    (fun future ->
      on_success future one_succeeded;
      on_failure future one_failed; )
    futures;
  future'

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
  Pool.run pool
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
    (fun () -> Thread.delay time; ())

module Infix = struct
  let (>>=) x f = flatMap f x
  let (>>) a f = andThen a f
end
