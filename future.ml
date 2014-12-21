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

exception SendTwice

(** {2 MVar: a zero-or-one element thread-safe box} *)

module MVar = struct
  type 'a t = {
    mutable content : 'a option;
    mutex : Mutex.t;
    on_take : Condition.t;  (* signal that a value was removed (empty) *)
    on_put : Condition.t;   (* signal that a value was added (full) *)
  }

  (* Create an empty box *)
  let empty () = {
    content = None;
    mutex = Mutex.create ();
    on_take = Condition.create ();
    on_put = Condition.create ();
  }

  (* Create a full box *)
  let full x = {
    content = Some x;
    mutex = Mutex.create ();
    on_take = Condition.create ();
    on_put = Condition.create ();
  }

  (* Is the box currently empty? *)
  let is_empty box = match box.content with
      | Some _ -> true
      | None -> false

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

  (* Take value out of the box. Wait if necessary *)
  let take box =
    Mutex.lock box.mutex;
    let x = wait_put box in
    box.content <- None;
    Condition.broadcast box.on_take;
    Mutex.unlock box.mutex;
    x

  (* Put a value in the box. Waits if the box is already full *)
  let put box x =
    Mutex.lock box.mutex;
    wait_take box;
    box.content <- Some x;
    Condition.broadcast box.on_put;
    Mutex.unlock box.mutex

  (* Use given function to atomically update content, and return
      the previous value and the new one *)
  let update box f =
    Mutex.lock box.mutex;
    let x = wait_put box in
    try
      let x', res = f x in
      box.content <- Some x';
      Condition.broadcast box.on_put;  (* signal write *)
      Mutex.unlock box.mutex;
      res
    with e ->
      Mutex.unlock box.mutex;
      raise e 

  (* Look at the value, without removing it *)
  let peek box =
    Mutex.lock box.mutex;
    let x = wait_put box in
    Mutex.unlock box.mutex;
    x
end

module type S = sig
  type 'a t
    (** A future value of type 'a *)

  val run : (unit -> unit) -> unit
    (** Use the underlying thread pool to run this job *)

  val finish : unit -> unit
    (** Kill threads in the pool. The pool won't be usable any more. *)

  (** {2 Basic low-level Future functions} *)

  type 'a state =
    | NotKnown
    | Success of 'a
    | Failure of exn

  val state : 'a t -> 'a state
    (** Current state of the future *)

  val is_done : 'a t -> bool
    (** Is the future evaluated (success/failure)? *)

  (** {2 Combinators} *)

  val on_success : 'a t -> ('a -> unit) -> unit
    (** Attach a handler to be called upon success *)

  val on_failure : _ t -> (exn -> unit) -> unit
    (** Attach a handler to be called upon failure *)

  val on_finish : _ t -> (unit -> unit) -> unit
    (** Attach a handler to be called when the future is evaluated *)

  val flatMap : ('a -> 'b t) -> 'a t -> 'b t
    (** Monadic combination of futures *)

  val andThen : 'a t -> (unit -> 'b t) -> 'b t
    (** Wait for the first future to succeed, then launch the second *)

  val sequence : 'a t list -> 'a list t
    (** Future that waits for all previous sequences to terminate *)

  val choose : 'a t list -> 'a t
    (** Choose among those futures (the first to terminate) *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** Maps the value inside the future *)

  (** {2 Future constructors} *)

  val return : 'a -> 'a t
    (** Future that is already computed *)

  val spawn : (unit -> 'a) -> 'a t
    (** Spawn a thread that wraps the given computation *)

  val spawn_process : ?stdin:string -> cmd:string ->
                      (int * string * string) t
    (** Spawn a sub-process with the given command [cmd] (and possibly input);
        returns a future containing (returncode, stdout, stderr) *)

  val sleep : float -> unit t
    (** Future that returns with success in the given amount of seconds *)

  (** {2 Event timer} *)

  module Timer : sig
    val at : float -> (unit -> unit) -> unit
      (** [schedule_at ~at act] will run [act] at the Unix echo [at] *)

    val after : float -> (unit -> unit) -> unit
      (** [schedule_after ~after act] will run [act] in [after] seconds *)
  end

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>) : 'a t -> (unit -> 'b t) -> 'b t
  end

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> (unit -> 'b t) -> 'b t
end

module type CONFIG = sig
  val min_size : int
  val max_size : int
end

module DefaultConfig = struct
  let min_size = 0
  let max_size = 15
end

(** {2 Mutable heap}
inlined here for avoiding dependencies *)
module Heap = struct
  (** Implementation from http://en.wikipedia.org/wiki/Skew_heap *)

  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A pairing tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  let rec union ~cmp t1 t2 = match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (x1, l1, r1), Node (x2, l2, r2) ->
    if cmp x1 x2 <= 0
      then Node (x1, union ~cmp t2 r1, l1)
      else Node (x2, union ~cmp t1 r2, l2)

  let insert h x =
    h.tree <- union ~cmp:h.cmp (Node (x, Empty, Empty)) h.tree

  let min h = match h.tree with
    | Empty -> raise Not_found
    | Node (x, _, _) -> x

  let pop h = match h.tree with
    | Empty -> raise Not_found
    | Node (x, l, r) ->
      h.tree <- union ~cmp:h.cmp l r;
      x
end

module Make(C : CONFIG) = struct
  type command =
    | Perform of (unit -> unit)
    | Quit
    (** Command sent to a thread *)

  type waiting_thread = float * command MVar.t

  let stop = ref false
  let mutex = Mutex.create ()
  let jobs = Queue.create ()
  let new_task = Condition.create () (* signal when new task *)
  let cur_size = ref 0

  (* Function that the threads run *)
  let rec serve () =
    Mutex.lock mutex;
    next_task ()
  (* process next task *)
  and next_task () =
    if !stop then Condition.broadcast new_task  (* and stop *)
    else match poll () with
    | Some job ->
        Mutex.unlock mutex;
        begin try job()
        with _ -> ()
        end;
        serve ()
    | None ->
        if !cur_size > C.min_size
        then () (* stop, too many threads *)
        else next_task()
  (* poll for next task *)
  and poll () =
    if Queue.is_empty jobs
    then begin
      Condition.wait new_task mutex;
      if !stop || Queue.is_empty jobs
      then None
      else begin
        let job = Queue.pop jobs in
        Condition.signal new_task;
        Some job
      end
    end else
      Some (Queue.pop jobs)

      (* TODO: start thread iff new task and not max_size reached *)

  (* Add a thread to the pool, starting with the first job *)
  let add_thread job =
    let box = MVar.full job in
    ignore (Thread.create serve box)

  (* Run the job in the pool *)
  let run job =
    assert (not (!stop));
    Mutex.lock mutex;
    begin match !threads with
    | [] when !cur_size = C.max_size ->
      (* max capacity reached, push task in queue *)
      Queue.push job jobs
    | [] ->
      assert (!cur_size < C.max_size);
      (* spawn a thread for the given task *)
      add_thread (Perform job);
      cur_size := !cur_size + 1;
    | (_,box)::l' ->
      (* use the first thread *)
      MVar.put box (Perform job);
      threads := l';
    end;
    Mutex.unlock mutex

  (** Kill threads in the pool *)
  let finish () =
    Mutex.lock mutex;
    stop := true;
    (* kill waiting threads *)
    List.iter (fun (_,box) -> MVar.put box Quit) !threads;
    threads := [];
    Mutex.unlock mutex

  (** {3 Futures} *)

  type 'a state =
    | NotKnown
    | Success of 'a
    | Failure of exn

  type 'a handler =
    | OnSuccess of ('a -> unit)
    | OnFailure of (exn -> unit)
    | OnFinish of (unit -> unit)

  type 'a t = {
    mutable state : 'a state;
    mutable handlers : 'a handler list;   (* handlers *)
  } (** A future value of type 'a *)

  let make_empty pool = {
    state = NotKnown;
    handlers = [];
  }

  let state f = f.state

  let send future x =
    match future.state with
    | NotKnown ->  (* set content and signal *)
      future.state <- Success x;
      List.iter
        (function
          | OnSuccess f -> run (fun () -> f x)
          | OnFinish f -> run (fun () -> f ())
          | OnFailure _ -> ())
        future.handlers;
    | _ ->
      raise SendTwice  (* already set! *)

  let fail future e =
    match future.state with
    | NotKnown ->  (* set content and signal *)
      future.state <- Failure e;
      List.iter
        (function
          | OnSuccess _ -> ()
          | OnFinish f -> f ()
          | OnFailure f -> f e)
        future.handlers
    | _ ->
      raise SendTwice  (* already set! *)

  let is_done future =
    match future.state with
    | NotKnown -> false
    | _ -> true

  (** {2 Combinators *)

  let on_success future k =
    match future.state with
    | NotKnown ->
      future.handlers <- (OnSuccess k) :: future.handlers; (* wait *)
    | Success x -> run (fun () -> k x)
    | Failure _ -> ()

  let on_failure future k =
    match future.state with
    | NotKnown ->
      future.handlers <- (OnFailure k) :: future.handlers; (* wait *)
    | Success _ -> ()
    | Failure e -> run (fun () -> k e)

  let on_finish future k =
    match future.state with
    | NotKnown ->
      future.handlers <- (OnFinish k) :: future.handlers; (* wait *)
    | Success _ | Failure _ -> run (fun () -> k ())

  let flatMap f future =
    let future' = make_empty () in
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
    let future' = make_empty () in
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
    let future' = make_empty () in
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
    let future' = make_empty () in
    on_success future (fun x -> let y = f x in send future' y);
    on_failure future (fun e -> fail future' e);
    future'

  (** {2 Future constructors} *)

  let return x = {
    state = Success x;
    handlers = [];
  }

  let spawn f =
    let future = make_empty () in
    (* schedule computation *)
    run (fun () ->
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
  let spawn_process ?(stdin="") ~cmd =
    spawn (fun () ->
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

  let sleep time =
    spawn (fun () -> Thread.delay time; ())

  (** {2 Event timer} *)
  module Timer = struct
    let cmp_tasks (f1,_) (f2,_) =
      compare f1 f2

    let stop = ref false
    let tasks : (float * (unit -> unit)) Heap.t = Heap.empty ~cmp:cmp_tasks
    let fifo_in, fifo_out = Unix.pipe ()
    let thread = ref None
    let standby_wait = 30.    (* when no task is scheduled *)
    let epsilon = 0.0001      (* accepted time diff for actions *)

    (** Wait for next event, run it, and loop *)
    let serve () =
      let buf = String.make 1 '_' in
      (* process next task *)
      let rec next () =
        Mutex.lock mutex;
        (* what is the next task? *)
        let next_task =
          try Some (Heap.min tasks)
          with Not_found -> None in
        match next_task with
        | _ when !stop -> Mutex.unlock mutex  (* stop *)
        | None ->
          Mutex.unlock mutex;
          wait standby_wait  (* wait for a task *)
        | Some (time, task) ->
          let now = Unix.gettimeofday () in
          if now +. epsilon > time
            then begin (* run task in the pool *)
              run task;
              ignore (Heap.pop tasks); 
              Mutex.unlock mutex;
              (* process next task, if any *)
              next ()
            end else  (* too early, wait *)
              (Mutex.unlock mutex;
              wait (time -. now))
      (* wait for [delay] seconds, or until something happens on fifo_in *)
      and wait delay =
        let read = Thread.wait_timed_read fifo_in delay in
        if read
          then ignore (Unix.read fifo_in buf 0 1);  (* remove char *)
        next ()
      in
      next ()

    let () =
      let t = Thread.create serve () in
      thread := Some t;
      ()

    let at time task =
      Mutex.lock mutex;
      (* time of the next scheduled event *)
      let next_time =
        try let time, _ = Heap.min tasks in time
        with Not_found -> max_float
      in
      (* insert task *)
      Heap.insert tasks (time, task);
      (* see if the timer thread needs to be awaken earlier *)
      if time < next_time
        then ignore (Unix.single_write fifo_out "_" 0 1);
      Mutex.unlock mutex;
      ()

    let after after task =
      assert (after>= 0.);
      at (Unix.gettimeofday () +. after) task
  end

  module Infix = struct
    let (>>=) x f = flatMap f x
    let (>>) a f = andThen a f
  end

  include Infix
end

module Std = Make(DefaultConfig)
