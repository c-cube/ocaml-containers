
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Event timer} *)

type job =
  | Job : float * (unit -> 'a) -> job

let (<=) (a : float) b = Stdlib.(<=) a b
let (>=) (a : float) b = Stdlib.(>=) a b
let (<) (a : float) b = Stdlib.(<) a b
let (>) (a : float) b = Stdlib.(>) a b

module TaskHeap = CCHeap.Make(struct
    type t = job
    let leq (Job(f1,_)) (Job (f2,_)) = f1 <= f2
  end)

exception Stopped

type t = {
  mutable stop : bool;
  mutable tasks : TaskHeap.t;
  mutable exn_handler : (exn -> unit);
  t_mutex : Mutex.t;
  fifo_in : Unix.file_descr;
  fifo_out : Unix.file_descr;
}

let set_exn_handler timer f = timer.exn_handler <- f

let standby_wait = 10.
(* when no task is scheduled, this is the amount of time that is waited
   in a row for something to happen. This is also the maximal delay
   between the call to {!stop} and the actual termination of the
   thread. *)

let epsilon = 0.0001
(* accepted time diff for actions. *)

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
  | Quit
  | Run : (unit -> _) -> command
  | Wait of float

let pop_task_ t =
  let tasks, _ = TaskHeap.take_exn t.tasks in
  t.tasks <- tasks

let call_ timer f =
  try ignore (f ())
  with e -> timer.exn_handler e

(* check next task *)
let next_task_ timer = match TaskHeap.find_min timer.tasks with
  | _ when timer.stop -> Quit
  | None -> Wait standby_wait
  | Some Job (time, f) ->
    let now = Unix.gettimeofday () in
    if now +. epsilon > time then (
      (* now! *)
      pop_task_ timer;
      Run f
    ) else Wait (time -. now)

(* The main thread function: wait for next event, run it, and loop *)
let serve timer =
  let buf = Bytes.make 1 '_' in
  (* acquire lock, call [process_task] and do as it commands *)
  let rec next () = match with_lock_ timer next_task_ with
    | Quit -> ()
    | Run f ->
      call_ timer f; (* call outside of any lock *)
      next ()
    | Wait delay -> wait delay
  (* wait for [delay] seconds, or until something happens on [fifo_in] *)
  and wait delay =
    let read = Thread.wait_timed_read timer.fifo_in delay in
    (* remove char from fifo, so that next write can happen *)
    if read then ignore (Unix.read timer.fifo_in buf 0 1);
    next ()
  in
  next ()

let nop_handler_ _ = ()

let create () =
  let fifo_in, fifo_out = Unix.pipe () in
  let timer = {
    stop = false;
    tasks = TaskHeap.empty;
    exn_handler = nop_handler_;
    t_mutex = Mutex.create ();
    fifo_in;
    fifo_out;
  } in
  (* start a thread to process tasks *)
  let _t = Thread.create serve timer in
  timer

let underscore_ = Bytes.make 1 '_'

(* awake the thread *)
let awaken_ timer =
  ignore (Unix.single_write timer.fifo_out underscore_ 0 1)

(** [at s t ~f] will run [f ()] at the Unix echo [t] *)
let at timer time ~f =
  if timer.stop then raise Stopped;
  let now = Unix.gettimeofday () in
  if now >= time
  then call_ timer f
  else
    with_lock_ timer
      (fun timer ->
         if timer.stop then raise Stopped;
         (* time of the next scheduled event *)
         let next_time = match TaskHeap.find_min timer.tasks with
           | None -> max_float
           | Some Job (d, _) -> d
         in
         (* insert task *)
         timer.tasks <- TaskHeap.insert (Job (time, f)) timer.tasks;
         (* see if the timer thread needs to be awaken earlier *)
         if time < next_time then awaken_ timer
      )

let after timer delay ~f =
  assert (delay >= 0.);
  let now = Unix.gettimeofday () in
  at timer (now +. delay) ~f

exception ExitEvery

let every ?delay timer d ~f =
  let rec run () =
    try
      ignore (f ());
      schedule()
    with ExitEvery -> () (* stop *)
  and schedule () = after timer d ~f:run in
  match delay with
    | None -> run()
    | Some d -> after timer d ~f:run

(*$R
  let start = Unix.gettimeofday() in
  let timer = create() in
  let res = CCLock.create 0 in
  let sem = CCSemaphore.create 1 in
  CCSemaphore.acquire 1 sem;
  let stop = ref 0. in
  every timer 0.1
    ~f:(fun () ->
      if CCLock.incr_then_get res > 5 then (
        stop := Unix.gettimeofday();
        CCSemaphore.release 1 sem;
        raise ExitEvery
      ));
  CCSemaphore.acquire 1 sem; (* wait *)
  OUnit.assert_equal ~printer:CCInt.to_string 6 (CCLock.get res);
  OUnit.assert_bool "estimate delay" (abs_float (!stop -. start -. 0.5) < 0.2);
*)

let active timer = not timer.stop

(** Stop the given timer, cancelling pending tasks *)
let stop timer =
  with_lock_ timer
    (fun timer ->
       if not timer.stop then (
         timer.stop <- true;
         (* empty heap of tasks *)
         timer.tasks <- TaskHeap.empty;
         (* tell the thread to stop *)
         awaken_ timer;
       )
    )

(*$R
  (* scenario:  n := 1; n := n*4 ; n := n+2; res := n *)
  let timer = create () in
  let n = CCLock.create 1 in
  let res = CCLock.create 0 in
  after timer 0.3
    ~f:(fun () -> CCLock.update n (fun x -> x+2));
  ignore (Thread.create
    (fun _ -> Thread.delay 0.4; CCLock.set res (CCLock.get n)) ());
  after timer 0.2
    ~f:(fun () -> CCLock.update n (fun x -> x * 4));
  Thread.delay 0.6 ;
  OUnit.assert_equal 6 (CCLock.get res);
*)
