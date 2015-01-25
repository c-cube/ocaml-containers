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

type 'a state =
  | Done of 'a
  | Waiting
  | Failed of exn

(** {2 Thread pool} *)
module Pool = struct
  type job =
    | Job : ('a -> unit) * 'a -> job

  type t = {
    mutable stop : bool;  (* indicate that threads should stop *)
    mutex : Mutex.t;
    jobs : job Queue.t;  (* waiting jobs *)
    mutable cur_size : int;  (* total number of threads *)
    max_size : int;
  } (** Dynamic, growable thread pool *)

  let with_lock_ t f =
    Mutex.lock t.mutex;
    try
      let x = f t in
      Mutex.unlock t.mutex;
      x
    with e ->
      Mutex.unlock t.mutex;
      raise e

  type command =
    | Process of job
    | Die (* thread has no work to do *)

  let die pool =
    assert (pool.cur_size > 0);
    pool.cur_size <- pool.cur_size - 1;
    Die

  (** thread: entry point. They seek jobs in the queue *)
  let rec serve pool =
    match with_lock_ pool get_next with
    | Die -> ()
    | Process (Job (f, x)) ->
        f x;
        serve pool

  (* thread: seek what to do next (including dying) *)
  and get_next pool =
    if pool.stop then die pool
    else if Queue.is_empty pool.jobs then die pool
    else (
      let job = Queue.pop pool.jobs in
      Process job
    )

  (** Create a pool with at most the given number of threads. [timeout]
      is the time after which idle threads are killed. *)
  let create ~max_size () =
    let pool = {
      stop = false;
      cur_size = 0;
      max_size;
      jobs = Queue.create ();
      mutex = Mutex.create ();
    } in
    pool

  exception PoolStopped

  let run_job pool job =
    (* heuristic criterion for starting a new thread. We try to assess
        whether there are many busy threads and many waiting tasks.
        If there are many threads, it's less likely to start a new one *)
    let should_start_thread p =
      let num_q = Queue.length p.jobs in
      let num_busy = p.cur_size in
      let reached_max = p.cur_size = p.max_size in
      num_q > 0 && not reached_max && (num_q > 2 * num_busy)
    in
    (* acquire lock and push job in queue *)
    with_lock_ pool
      (fun pool ->
        if pool.stop then raise PoolStopped;
        Queue.push job pool.jobs;
        (* maybe start a thread *)
        if should_start_thread pool then (
          pool.cur_size <- pool.cur_size + 1;
          ignore (Thread.create serve pool)
        )
      )

  (* Run the function on the argument in the given pool *)
  let run pool f x = run_job pool (Job (f, x))

  (* Kill threads in the pool *)
  let stop pool =
    with_lock_ pool
      (fun p ->
        p.stop <- true;
        Queue.clear p.jobs
      )
end

let pool = Pool.create ~max_size:50 ()
(** Default pool of threads, should be ok for most uses. *)

(** {2 Futures} *)

type 'a handler = 'a state -> unit

(** A proper future, with a delayed computation *)
type 'a cell = {
  mutable state : 'a state;
  mutable handlers : 'a handler list;   (* handlers *)
  mutex : Mutex.t;
  condition : Condition.t;
}

(** A future value of type 'a *)
type 'a t =
  | Return of 'a
  | FailNow of exn
  | Run of 'a cell

type 'a future = 'a t

(** {2 Basic Future functions} *)

let return x = Return x

let fail e = FailNow e

let create_cell () = {
  state = Waiting;
  handlers = [];
  mutex = Mutex.create ();
  condition = Condition.create ();
}

let with_lock_ cell f =
  Mutex.lock cell.mutex;
  try
    let x = f cell in
    Mutex.unlock cell.mutex;
    x
  with e ->
    Mutex.unlock cell.mutex;
    raise e

let set_done_ cell x =
  with_lock_ cell
    (fun cell -> match cell.state with
    | Waiting ->  (* set state and signal *)
        cell.state <- Done x;
        Condition.broadcast cell.condition;
        List.iter (fun f -> f cell.state) cell.handlers
    | _ -> assert false
    )

let set_fail_ cell e =
  with_lock_ cell
    (fun cell -> match cell.state with
    | Waiting ->
        cell.state <- Failed e;
        Condition.broadcast cell.condition;
        List.iter (fun f -> f cell.state) cell.handlers
    | _ -> assert false
    )

let run_and_set1 cell f x =
  try
    let y = f x in
    set_done_ cell y
  with e ->
    set_fail_ cell e

let run_and_set2 cell f x y =
  try
    let z = f x y in
    set_done_ cell z
  with e ->
    set_fail_ cell e

let make1 f x =
  let cell = create_cell() in
  Pool.run pool (run_and_set1 cell f) x;
  Run cell

let make f = make1 f ()

let make2 f x y =
  let cell = create_cell() in
  Pool.run pool (run_and_set2 cell f x) y;
  Run cell

let get = function
  | Return x -> x
  | FailNow e -> raise e
  | Run cell ->
      let rec get_cell cell = match cell.state with
        | Waiting ->
            Condition.wait cell.condition cell.mutex; (* wait *)
            get_cell cell
        | Done x -> Mutex.unlock cell.mutex; x
        | Failed e -> Mutex.unlock cell.mutex; raise e
      in
      Mutex.lock cell.mutex;
      get_cell cell

let state = function
  | Return x -> Done x
  | FailNow e -> Failed e
  | Run cell ->
      with_lock_ cell (fun cell -> cell.state)

let is_done = function
  | Return _
  | FailNow _ -> true
  | Run cell ->
      with_lock_ cell (fun c -> c.state <> Waiting)

(** {2 Combinators *)

let add_handler_ cell f =
  with_lock_ cell
    (fun cell -> match cell.state with
      | Waiting -> cell.handlers <- f :: cell.handlers
      | Done _ | Failed _ -> f cell.state
    )

let on_finish fut k = match fut with
  | Return x -> k (Done x)
  | FailNow e -> k (Failed e)
  | Run cell -> add_handler_ cell k

let on_success fut k =
  on_finish fut
    (function
      | Done x -> k x
      | _ -> ()
    )

let on_failure fut k =
  on_finish fut
    (function
      | Failed e -> k e
      | _ -> ()
    )

let map f fut = match fut with
  | Return x -> make1 f x
  | FailNow e -> FailNow e
  | Run cell ->
      let cell' = create_cell() in
      add_handler_ cell
        (function
          | Done x -> run_and_set1 cell' f x
          | Failed e -> set_fail_ cell' e
          | Waiting -> assert false
        );
      Run cell'

let flat_map f fut = match fut with
  | Return x -> f x
  | FailNow e -> FailNow e
  | Run cell ->
      let cell' = create_cell() in
      add_handler_ cell
        (function
          | Done x ->
              let fut' = f x in
              on_finish fut'
                (function
                  | Done y -> set_done_ cell' y
                  | Failed e -> set_fail_ cell' e
                  | Waiting -> assert false
                )
          | Failed e -> set_fail_ cell' e
          | Waiting -> assert false
        );
      Run cell'

let and_then fut f = flat_map (fun _ -> f ()) fut

let sequence futures =
  let n = List.length futures in
  let state = CCLock.create (`WaitFor n) in
  let results = Array.make n None in
  let cell = create_cell() in
  (* when all futures returned, collect results for future' *)
  let send_result () =
    let l = Array.map
      (function
        | None -> assert false
        | Some x -> x
      ) results
    in
    set_done_ cell (Array.to_list l)
  in
  (* wait for all to succeed or fail *)
  List.iteri
    (fun i fut ->
      on_finish fut
        (fun res ->
          CCLock.update state
            (fun st -> match res, st with
            | Done _, `Failed -> st
            | Done x, `WaitFor 1 -> results.(i) <- Some x; send_result (); `Done
            | Done x, `WaitFor n -> results.(i) <- Some x; `WaitFor (n-1)
            | Failed _, `Failed -> st
            | Failed e, `WaitFor _ -> set_fail_ cell e; `Failed
            | _, `Done -> assert false
            | Waiting, _ -> assert false
            )
        )
    ) futures;
  Run cell

let choose futures =
  let cell = create_cell() in
  let state = ref `Waiting in
  (* add handlers to all futures *)
  List.iter
    (fun fut ->
      on_finish fut 
        (fun res -> match res, !state with
          | Done x, `Waiting -> state := `Done; set_done_ cell x
          | Failed e, `Waiting -> state := `Done; set_fail_ cell e
          | Waiting, _ -> assert false
          | _, `Done -> ()
        )
    ) futures;
  Run cell

(** slurp the entire state of the file_descr into a string *)
let slurp i_chan =
  let buf_size = 128 in
  let state = Buffer.create 120
  and buf = String.make 128 'a' in
  let rec next () =
    let num = input i_chan buf 0 buf_size in
    if num = 0
      then Buffer.contents state (* EOF *)
      else (
        Buffer.add_substring state buf 0 num;
        next ()
      )
  in next ()

(** Spawn a sub-process with the given command [cmd] (and possibly input);
    returns a future containing (returncode, stdout, stderr) *)
let spawn_process ?(stdin="") ~cmd () =
   make
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
      (returncode, out', err')
    )

let sleep time = make (fun () -> Thread.delay time)

(** {2 Event timer} *)

module Timer = struct
  module TaskHeap = CCHeap.Make(struct
    type t = (float * unit cell)
    let leq (f1,_)(f2,_) = f1 <= f2
  end)

  type t = {
    mutable stop : bool;
    mutable thread : Thread.t option;  (* thread dedicated to the timer *)
    mutable tasks : TaskHeap.t;
    t_mutex : Mutex.t;
    fifo_in : Unix.file_descr;
    fifo_out : Unix.file_descr;
  } (** A timer for events *)

  let standby_wait = 10.    (* when no task is scheduled *)
  let epsilon = 0.0001      (* accepted time diff for actions *)

  let with_lock_ t f =
    Mutex.lock t.t_mutex;
    try
      let x = f t in
      Mutex.unlock t.t_mutex;
      x
    with e ->
      Mutex.unlock t.t_mutex;
      raise e

  type command =
    | Loop
    | Wait of float

  let pop_task_ t =
    let tasks, _ = TaskHeap.take_exn t.tasks in
    t.tasks <- tasks

  (** Wait for next event, run it, and loop *)
  let serve timer =
    let buf = String.make 1 '_' in
    (* acquire lock, call [process_task] and do as it commands *)
    let rec next () = match with_lock_ timer process_task with
      | Loop -> next ()
      | Wait delay -> wait delay
    (* check next task *)
    and process_task timer = match TaskHeap.find_min timer.tasks with
      | None -> Wait standby_wait
      | Some (time, cell) ->
          let now = Unix.gettimeofday () in
          if now +. epsilon > time then (
            (* now! *)
            pop_task_ timer;
            set_done_ cell ();
            Loop
          ) else Wait (time -. now)
    (* wait for [delay] seconds, or until something happens on fifo_in *)
    and wait delay =
      let read = Thread.wait_timed_read timer.fifo_in delay in
      if read
        then ignore (Unix.read timer.fifo_in buf 0 1);  (* remove char *)
      next ()
    in
    next ()

  (** A timer that runs in the given thread pool *)
  let create () =
    let fifo_in, fifo_out = Unix.pipe () in
    let timer = {
      stop = false;
      thread = None;
      tasks = TaskHeap.empty;
      t_mutex = Mutex.create ();
      fifo_in;
      fifo_out;
    } in
    (* start a thread to process tasks *)
    let t = Thread.create serve timer in
    timer.thread <- Some t;
    timer

  (** [timerule_at s t act] will run [act] at the Unix echo [t] *)
  let at timer time =
    let now = Unix.gettimeofday () in
    if now >= time
    then return ()
    else (
      let cell = create_cell() in
      with_lock_ timer
        (fun timer ->
          (* time of the next scheduled event *)
          let next_time = match TaskHeap.find_min timer.tasks with
            | None -> max_float
            | Some (f, _) -> f
          in
          (* insert task *)
          timer.tasks <- TaskHeap.insert (time, cell) timer.tasks;
          (* see if the timer thread needs to be awaken earlier *)
          if time < next_time
            then ignore (Unix.single_write timer.fifo_out "_" 0 1)
        );
      Run cell
    )

  let after timer delay =
    assert (delay >= 0.);
    let now = Unix.gettimeofday () in
    at timer (now +. delay)

  (** Stop the given timer, cancelling pending tasks *)
  let stop timer =
    with_lock_ timer
      (fun timer ->
        if not timer.stop then (
          timer.stop <- true;
          (* empty heap of tasks *)
          timer.tasks <- TaskHeap.empty;
          (* kill the thread *)
          match timer.thread with
          | None -> ()
          | Some t ->
            Thread.kill t;
            timer.thread <- None
        )
      )
end

module Infix = struct
  let (>>=) x f = flat_map f x
  let (>>) a f = and_then a f
  let (>|=) a f = map f a
end

include Infix

(** {2 Low Level } *)

let stop_pool () = Pool.stop pool
