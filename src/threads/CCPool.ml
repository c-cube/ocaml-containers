(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Thread Pool, and Futures} *)

type +'a state = Done of 'a | Waiting | Failed of exn

module type PARAM = sig
  val max_size : int
  (** Maximum number of threads in the pool *)
end

exception Stopped

(** {2 Thread pool} *)
module Make (P : PARAM) = struct
  type job =
    | Job1 : ('a -> _) * 'a -> job
    | Job2 : ('a -> 'b -> _) * 'a * 'b -> job
    | Job3 : ('a -> 'b -> 'c -> _) * 'a * 'b * 'c -> job
    | Job4 : ('a -> 'b -> 'c -> 'd -> _) * 'a * 'b * 'c * 'd -> job

  type t = {
    mutable stop: bool; (* indicate that threads should stop *)
    mutable exn_handler: exn -> unit;
    mutex: Mutex.t;
    cond: Condition.t;
    jobs: job Queue.t; (* waiting jobs *)
    mutable cur_size: int; (* total number of threads *)
    mutable cur_idle: int; (* number of idle threads *)
  }
  (** Dynamic, growable thread pool *)

  let nop_ _ = ()

  (* singleton pool *)
  let pool =
    {
      stop = false;
      exn_handler = nop_;
      cond = Condition.create ();
      cur_size = 0;
      cur_idle = 0;
      (* invariant: cur_idle <= cur_size *)
      jobs = Queue.create ();
      mutex = Mutex.create ();
    }

  let set_exn_handler f = pool.exn_handler <- f

  let[@inline] with_lock_ t f =
    Mutex.lock t.mutex;
    try
      let x = f t in
      Mutex.unlock t.mutex;
      x
    with e ->
      Mutex.unlock t.mutex;
      raise e

  let incr_size_ p = p.cur_size <- p.cur_size + 1
  let decr_size_ p = p.cur_size <- p.cur_size - 1
  let incr_idle_ p = p.cur_idle <- p.cur_idle + 1
  let decr_idle_ p = p.cur_idle <- p.cur_idle - 1

  (* next thing a thread should do *)
  type command =
    | Process of job
    | Wait
    (* wait on condition *)
    | Die
  (* thread has no work to do *)

  (* thread: seek what to do next (including dying).
     Assumes the pool is locked. *)
  let get_next_ pool =
    (*Printf.printf "get_next (cur=%d, idle=%d, stop=%B)\n%!" pool.cur_size pool.cur_idle pool.stop;*)
    if pool.stop then
      Die
    else (
      match Queue.take pool.jobs with
      | exception Queue.Empty ->
        if pool.cur_idle > 0 then
          (* die: there's already at least one idle thread *)
          (*Printf.printf "DIE (idle>0)\n%!";*)
          Die
        else
          (*Printf.printf "WAIT\n%!";*)
          Wait
      | job -> Process job
    )

  (* heuristic criterion for starting a new thread. *)
  let[@inline] can_start_thread_ p = p.cur_size < P.max_size

  (* Thread: entry point. They seek jobs in the queue *)
  let rec serve pool =
    assert (pool.cur_size <= P.max_size);
    assert (pool.cur_size > 0);
    Mutex.lock pool.mutex;
    let cmd = get_next_ pool in
    maybe_start_runner_ pool;
    run_cmd pool cmd

  (* run a command *)
  and run_cmd pool = function
    | Die ->
      decr_size_ pool;
      Mutex.unlock pool.mutex;
      ()
    | Wait ->
      (*Printf.printf "WAIT\n%!";*)
      incr_idle_ pool;
      Condition.wait pool.cond pool.mutex;
      decr_idle_ pool;
      Mutex.unlock pool.mutex;
      serve pool
    | Process job ->
      Mutex.unlock pool.mutex;
      run_job pool job

  (* execute the job *)
  and run_job pool job =
    match job with
    | Job1 (f, x) ->
      (try ignore (f x) with e -> pool.exn_handler e);
      serve pool
    | Job2 (f, x, y) ->
      (try ignore (f x y) with e -> pool.exn_handler e);
      serve pool
    | Job3 (f, x, y, z) ->
      (try ignore (f x y z) with e -> pool.exn_handler e);
      serve pool
    | Job4 (f, x, y, z, w) ->
      (try ignore (f x y z w) with e -> pool.exn_handler e);
      serve pool

  and maybe_start_runner_ pool : unit =
    if (not (Queue.is_empty pool.jobs)) && can_start_thread_ pool then (
      (* there's room for another thread to start processing jobs,
         starting with [Queue.pop pool.jobs] *)
      let job' = Queue.pop pool.jobs in
      launch_worker_on_ pool job'
    )

  and[@inline] launch_worker_on_ pool job =
    incr_size_ pool;
    ignore (Thread.create (run_job pool) job)

  let run_job job =
    (* acquire lock and push job in queue, or start thread directly
       if the queue is empty *)
    with_lock_ pool (fun pool ->
        if pool.stop then raise Stopped;
        if
          Queue.is_empty pool.jobs && can_start_thread_ pool
          && pool.cur_idle = 0
        then
          (* create the thread now, on [job], since no other job in
             the queue takes precedence.
             We do not want to wait for the busy threads to do our task
             if we are allowed to spawn a new thread, and no thread is
             just idle waiting for new jobs. *)
          launch_worker_on_ pool job
        else if pool.cur_idle > 0 then (
          (* at least one idle thread, wake it up *)
          Queue.push job pool.jobs;
          Condition.broadcast pool.cond (* wake up some worker *)
        ) else (
          Queue.push job pool.jobs;

          (* we might still be able to start another thread to help the
             active ones (none is idle). This thread is not necessarily
             going to process [job], but rather the first job in the queue *)
          if can_start_thread_ pool then (
            let job' = Queue.pop pool.jobs in
            launch_worker_on_ pool job'
          )
        ))

  (* run the function on the argument in the given pool *)
  let run1 f x = run_job (Job1 (f, x))
  let run f = run1 f ()
  let run2 f x y = run_job (Job2 (f, x, y))
  let run3 f x y z = run_job (Job3 (f, x, y, z))
  let run4 f x y z w = run_job (Job4 (f, x, y, z, w))
  let active () = not pool.stop

  (* kill threads in the pool *)
  let stop () =
    with_lock_ pool (fun p ->
        p.stop <- true;
        Queue.clear p.jobs;
        Condition.broadcast p.cond (* wait up idlers *))

  (* stop threads if pool is GC'd *)
  let () = Gc.finalise (fun _ -> stop ()) pool

  (** {6 Futures} *)
  module Fut = struct
    type 'a handler = 'a state -> unit

    type 'a cell = {
      mutable state: 'a state;
      mutable handlers: 'a handler list; (* handlers *)
      f_mutex: Mutex.t;
      condition: Condition.t;
    }
    (** A proper future, with a delayed computation *)

    (** A future value of type 'a *)
    type 'a t = Return of 'a | FailNow of exn | Run of 'a cell

    type 'a future = 'a t

    (** {2 Basic Future functions} *)

    let return x = Return x
    let fail e = FailNow e

    let create_cell () =
      {
        state = Waiting;
        handlers = [];
        f_mutex = Mutex.create ();
        condition = Condition.create ();
      }

    let with_lock_ cell f =
      Mutex.lock cell.f_mutex;
      try
        let x = f cell in
        Mutex.unlock cell.f_mutex;
        x
      with e ->
        Mutex.unlock cell.f_mutex;
        raise e

    (* TODO: exception handler for handler errors *)

    let set_done_ cell x =
      with_lock_ cell (fun cell ->
          match cell.state with
          | Waiting ->
            (* set state and signal *)
            cell.state <- Done x;
            Condition.broadcast cell.condition;
            List.iter
              (fun f -> try f cell.state with e -> pool.exn_handler e)
              cell.handlers
          | _ -> assert false)

    let set_fail_ cell e =
      with_lock_ cell (fun cell ->
          match cell.state with
          | Waiting ->
            cell.state <- Failed e;
            Condition.broadcast cell.condition;
            List.iter
              (fun f -> try f cell.state with e -> pool.exn_handler e)
              cell.handlers
          | _ -> assert false)

    (* calls [f x], and put result or exception in [cell] *)
    let run_and_set1 cell f x =
      try
        let y = f x in
        set_done_ cell y
      with e -> set_fail_ cell e

    let run_and_set2 cell f x y =
      try
        let z = f x y in
        set_done_ cell z
      with e -> set_fail_ cell e

    let make1 f x =
      let cell = create_cell () in
      run3 run_and_set1 cell f x;
      Run cell

    let make f = make1 f ()

    let make2 f x y =
      let cell = create_cell () in
      run4 run_and_set2 cell f x y;
      Run cell

    let get = function
      | Return x -> x
      | FailNow e -> raise e
      | Run cell ->
        let rec get_ cell =
          match cell.state with
          | Waiting ->
            Condition.wait cell.condition cell.f_mutex;
            (* wait *)
            get_ cell
          | Done x -> x
          | Failed e -> raise e
        in
        with_lock_ cell get_

    (* access the result without locking *)
    let get_nolock_ = function
      | Return x | Run { state = Done x; _ } -> x
      | FailNow _ | Run { state = Failed _ | Waiting; _ } -> assert false

    let state = function
      | Return x -> Done x
      | FailNow e -> Failed e
      | Run cell -> with_lock_ cell (fun cell -> cell.state)

    let is_not_waiting = function
      | Waiting -> false
      | Failed _ | Done _ -> true

    let is_done = function
      | Return _ | FailNow _ -> true
      | Run cell -> with_lock_ cell (fun c -> is_not_waiting c.state)

    (** {2 Combinators *)

    let add_handler_ cell f =
      with_lock_ cell (fun cell ->
          match cell.state with
          | Waiting -> cell.handlers <- f :: cell.handlers
          | Done _ | Failed _ -> f cell.state)

    let on_finish fut k =
      match fut with
      | Return x -> k (Done x)
      | FailNow e -> k (Failed e)
      | Run cell -> add_handler_ cell k

    let on_success fut k =
      on_finish fut (function
        | Done x -> k x
        | _ -> ())

    let on_failure fut k =
      on_finish fut (function
        | Failed e -> k e
        | _ -> ())

    let map_cell_ ~async f cell ~into:cell' =
      add_handler_ cell (function
        | Done x ->
          if async then
            run3 run_and_set1 cell' f x
          else
            run_and_set1 cell' f x
        | Failed e -> set_fail_ cell' e
        | Waiting -> assert false);
      Run cell'

    let map_ ~async f fut =
      match fut with
      | Return x ->
        if async then
          make1 f x
        else
          Return (f x)
      | FailNow e -> FailNow e
      | Run cell -> map_cell_ ~async f cell ~into:(create_cell ())

    let map f fut = map_ ~async:false f fut
    let map_async f fut = map_ ~async:true f fut

    let app_ ~async f x =
      match f, x with
      | Return f, Return x ->
        if async then
          make1 f x
        else
          Return (f x)
      | FailNow e, _ | _, FailNow e -> FailNow e
      | Return f, Run x ->
        map_cell_ ~async (fun x -> f x) x ~into:(create_cell ())
      | Run f, Return x ->
        map_cell_ ~async (fun f -> f x) f ~into:(create_cell ())
      | Run f, Run x ->
        let cell' = create_cell () in
        add_handler_ f (function
          | Done f -> ignore (map_cell_ ~async f x ~into:cell')
          | Failed e -> set_fail_ cell' e
          | Waiting -> assert false);
        Run cell'

    let app f x = app_ ~async:false f x
    let app_async f x = app_ ~async:true f x

    let monoid_product f x y =
      match x, y with
      | Return x, Return y -> Return (f x y)
      | FailNow e, _ | _, FailNow e -> FailNow e
      | Return x, Run y ->
        map_cell_ ~async:false (fun y -> f x y) y ~into:(create_cell ())
      | Run x, Return y ->
        map_cell_ ~async:false (fun x -> f x y) x ~into:(create_cell ())
      | Run x, Run y ->
        let cell' = create_cell () in
        add_handler_ x (function
          | Done x ->
            ignore (map_cell_ ~async:false (fun y -> f x y) y ~into:cell')
          | Failed e -> set_fail_ cell' e
          | Waiting -> assert false);
        Run cell'

    let flat_map f fut =
      match fut with
      | Return x -> f x
      | FailNow e -> FailNow e
      | Run cell ->
        let cell' = create_cell () in
        add_handler_ cell (function
          | Done x ->
            let fut' = f x in
            on_finish fut' (function
              | Done y -> set_done_ cell' y
              | Failed e -> set_fail_ cell' e
              | Waiting -> assert false)
          | Failed e -> set_fail_ cell' e
          | Waiting -> assert false);
        Run cell'

    let and_then fut f = flat_map (fun _ -> f ()) fut

    type _ array_or_list =
      | A_ : 'a array -> 'a array_or_list
      | L_ : 'a list -> 'a array_or_list

    let iter_aol : type a. a array_or_list -> (a -> unit) -> unit =
     fun aol f ->
      match aol with
      | A_ a -> Array.iter f a
      | L_ l -> List.iter f l

    (* [sequence_ l f] returns a future that waits for every element of [l]
       to return of fail, and call [f ()] to obtain the result (as a closure)
       in case every element succeeded (otherwise a failure is
       returned automatically) *)
    let sequence_ : type a res. a t array_or_list -> (unit -> res) -> res t =
     fun aol f ->
      let n =
        match aol with
        | A_ a -> Array.length a
        | L_ l -> List.length l
      in
      assert (n > 0);
      let cell = create_cell () in
      let n_err = CCLock.create 0 in
      (* number of failed threads *)
      let n_ok = CCLock.create 0 in
      (* number of succeeding threads *)
      iter_aol aol (fun fut ->
          on_finish fut (function
            | Failed e ->
              let x = CCLock.incr_then_get n_err in
              (* if first failure, then seal [cell]'s fate now *)
              if x = 1 then set_fail_ cell e
            | Done _ ->
              let x = CCLock.incr_then_get n_ok in
              (* if [n] successes, then [cell] succeeds. Otherwise, some
                 job has not finished or some job has failed. *)
              if x = n then (
                let res = f () in
                set_done_ cell res
              )
            | Waiting -> assert false));
      Run cell

    (* map an array of futures to a future array *)
    let sequence_a a =
      match a with
      | [||] -> return [||]
      | [| x |] -> map (fun x -> [| x |]) x
      | _ -> sequence_ (A_ a) (fun () -> Array.map get_nolock_ a)

    let map_a f a = sequence_a (Array.map f a)

    let sequence_l l =
      match l with
      | [] -> return []
      | _ :: _ ->
        let l = List.rev l in
        sequence_ (L_ l) (fun () -> List.rev_map get_nolock_ l)

    (* reverse twice *)
    let map_l f l =
      match l with
      | [] -> return []
      | _ ->
        let l = List.rev_map f l in
        sequence_ (L_ l) (fun () -> List.rev_map get_nolock_ l)

    let choose_ : type a. a t array_or_list -> a t =
     fun aol ->
      let cell = create_cell () in
      let is_done = CCLock.create false in
      iter_aol aol (fun fut ->
          on_finish fut (fun res ->
              match res with
              | Waiting -> assert false
              | Done x ->
                let was_done = CCLock.get_then_clear is_done in
                if not was_done then set_done_ cell x
              | Failed e ->
                let was_done = CCLock.get_then_clear is_done in
                if not was_done then set_fail_ cell e));
      Run cell

    let choose_a a = choose_ (A_ a)
    let choose_l l = choose_ (L_ l)
    let sleep time = make1 Thread.delay time

    module Infix = struct
      let ( >>= ) x f = flat_map f x
      let ( >> ) a f = and_then a f
      let ( >|= ) a f = map f a
      let ( <*> ) = app

      [@@@ifge 4.8]

      let ( let+ ) = ( >|= )
      let ( let* ) = ( >>= )
      let[@inline] ( and+ ) a1 a2 = monoid_product (fun x y -> x, y) a1 a2
      let ( and* ) = ( and+ )

      [@@@endif]
    end

    include Infix
  end
end
