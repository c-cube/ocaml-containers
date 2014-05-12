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

(** {1 Behavior Trees for React} *)

(** {2 Behavior tree} *)

type tree =
  | Test of (unit -> bool)                  (* call and test value *)
  | Wait of (unit -> bool Lwt.t)            (* wait for the future to complete *)
  | Do of (unit -> bool)                    (* perform an action *)
  | If of (unit -> bool) * tree * tree      (* switch *)
  | Sequence of bool * tree list            (* yield to subtrees sequentially. bool: loop? *)
  | Select of select_strategy * tree list   (* select one subtree *)
  | Parallel of parallel_strategy * tree list   (* run all subtrees in parallel *)
  | Closure of (unit -> tree)               (* build a tree dynamically *)
  | Succeed                                 (* always succeed *)
  | Fail                                    (* always fail *)
  (** A behavior tree *)
and select_strategy = tree list -> (unit -> tree option)
  (** How to select a subtree to run. It yields a subtree until it
      decides to fail *)
and parallel_strategy =
  | PSForall  (** succeeds when all subtrees succeed *)
  | PSExists  (** succeeds when some subtree succeeds *)

let strategy_inorder l =
  let cur = ref l in
  fun () -> match !cur with
    | [] -> None
    | t::l' ->
      cur := l';
      Some t

let strategy_random ?(proba_fail=0.05) l =
  let a = Array.of_list l in
  fun () ->
    if Random.float 1. < proba_fail
      then None
      else (* choose in array *)
        let t = a.(Random.int (Array.length a)) in
        Some t

let succeed = Succeed

let fail = Fail

let test f = Test f

let wait fut = Wait (fun () -> fut)

let wait_ fut = Wait (fun () -> Lwt.bind fut (fun () -> Lwt.return_true))

let wait_closure f = Wait f

let timeout f = Wait (fun () -> Lwt.bind (Lwt_unix.sleep f) (fun () -> Lwt.return_false))

let delay f = Wait (fun () -> Lwt.bind (Lwt_unix.sleep f) (fun () -> Lwt.return_true))

let do_ act = Do act

let do_succeed act = Do (fun () -> act (); true)

let if_ s then_ else_ = If (s, then_, else_)

let when_ s t = if_ s t succeed

let while_ f l = Sequence (true, (test f) :: l)

let sequence ?(loop=false) l =
  assert (l <> []);
  Sequence (loop, l)

let repeat t = sequence ~loop:true [t]

let select ?(strat=strategy_inorder) l =
  assert (l <> []);
  Select (strat, l)

let or_else t1 t2 =
  select ~strat:strategy_inorder [t1; t2]

let parallel ?(strat=PSForall) l =
  assert (l <> []);
  Parallel (strat, l)

let closure f =
  Closure f

(** {2 Run a tree} *)

type result = bool Lwt.t

let run tree =
  let (>>=) = Lwt.(>>=) in
  (* run given tree *)
  let rec run tree = 
    match tree with
    | Test f -> Lwt.return (f ())
    | Wait f -> f ()
    | Do act -> if act () then Lwt.return_true else Lwt.return_false
    | If (s, then_, else_) -> (* depends on value returned by [s] *)
      if s () then run then_ else run else_
    | Sequence (loop, l) -> run_sequence ~loop l
    | Select (strat, l) -> run_select ~strat l
    | Parallel (strat, l) -> run_parallel ~strat l
    | Closure f -> let tree' = f () in run tree'
    | Succeed -> Lwt.return_true
    | Fail -> Lwt.return_false
  and run_sequence ~loop start =
    let rec process l = match l with
    | [] when loop -> run_sequence ~loop start
    | [] -> Lwt.return_true  (* success *)
    | t::l' ->
      let res_t = run t in
      res_t >>= fun t_succeeded ->
        if t_succeeded
          then process l'
          else Lwt.return_false
    in
    process start
  and run_select ~strat l =
    (* choice function *)
    let choose = strat l in
    (* try a subtree *)
    let rec try_one () =
      match choose () with
      | None -> Lwt.return_false  (* failure *)
      | Some t ->
        run t >>= fun t_succeeded ->
          if t_succeeded
            then Lwt.return_true
            else try_one ()
    in
    try_one ()
  and run_parallel ~strat l =
    let results = List.map run l in
    match strat with
    | PSExists -> Lwt_list.exists_p (fun x -> x) results
    | PSForall -> Lwt_list.for_all_p (fun x -> x) results
  in
  run tree
