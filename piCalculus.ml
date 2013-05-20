(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 Pi-calculus model of concurrency} *)

module DList = struct
  type 'a t = {
    value : 'a wrapper;
    mutable prev : 'a t;
    mutable next : 'a t;
  }
  and 'a wrapper =
    | First  (* first element of the list *)
    | Element of 'a

  (** New empty list *)
  let create () =
    let rec node = {
      value = First;
      prev = node;
      next = node;
    } in
    node

  let is_empty l =
    let ans = l.prev == l in
    (if ans then (assert (l.next == l && l.value == First)));
    ans

  (** Add element at the end *)
  let append l x =
    let node = {
      value = Element x;
      prev = l.prev;
      next = l;
    } in
    l.prev.next <- node;
    l.prev <- node;
    node

  (** Add element at the beginning *)
  let prepend l x =
    let node = {
      value = Element x;
      prev = l;
      next = l.next;
    } in
    l.next.prev <- node;
    l.next <- node;
    node

  (* remove the given element *)
  let remove x =
    assert (not (x.prev == x || x.next == x));
    x.prev.next <- x.next;
    x.next.prev <- x.prev;
    ()

  (** Pop the first element *)
  let pop l =
    match l.next.value with
    | First -> failwith "DList.pop: empty list"
    | Element x ->
      remove l.next;
      x

  let rec remove_list l = match l with
    | [] -> ()
    | x::l' -> remove x; remove_list l'

  (** Iterate on all elements *)
  let iter l f =
    let rec iter l = match l.value with
    | First -> ()
    | Element x ->
      f x;
      iter l.next
    in
    iter l.next
end

type 'a chan = {
  receivers : 'a transition_node DList.t;
  senders : 'a transition_node DList.t;
} (** Channel conveying values of type 'a. Invariant: receivers = None || senders = None *)
and 'a transition_node = {
  tn_transition : 'a __transition;
  mutable tn_hook : unit -> unit;       (* hook to call after transition *)
  tn_to_replicate : to_replicate ref;   (* do we have to replicate a process *)
} (** List of transitions for a channel *)
and to_replicate =
  | ReplicateNothing
  | ReplicateThis of process
  (** Do we have to replicate a process? *)
and process =
  | Parallel : process list -> process      (** Spawn several processes *)
  | Sum : transition list -> process        (** Choice point *)
  | Replicate : process -> process          (** Replication of a process *)
  | New : ('a chan -> process) -> process   (** New local name *)
  | Escape : (unit -> process) -> process   (** Run a user function *)
  | Stop : process                          (** Stop this process *)
  (** A process of the Pi-calculus *)
and _ __transition =
  | Receive : 'a chan * ('a -> process) -> 'a __transition
  | Send : 'a chan * 'a * process -> 'a __transition
  (** Transition: send or receive a message *)
and transition =
  | Transition : 'a __transition -> transition

let parallel l = (assert (l <> []); Parallel l)
let sum l = (assert (l <> []); Sum l)
let replicate p = Replicate p
let new_ f = New f
let escape f = Escape f
let stop = Stop

let send ch x p = Transition (Send (ch, x, p))
let receive ch f = Transition (Receive (ch, f))

let send_one ch x p = sum [send ch x p]
let receive_one ch f = sum [receive ch f]

let (>>) f p =
  escape (fun () -> f (); p)

let (|||) a b = parallel [a; b]

let (++) a b = sum [a; b]

(** New channel (name) *)
let mk_chan () =
  let ch = {
    receivers = DList.create ();
    senders = DList.create ();
  } in
  ch

type run_env = {
  tasks : (process * to_replicate ref) Queue.t;
} (** Environment for running processes *)

let mk_env () =
  { tasks = Queue.create (); }

(** Push the process in the queue of processes to eval *)
let push_process ~env p to_restart =
  Queue.push (p, to_restart) env.tasks

(** Check whether there is a process to replicate now *)
let check_replicate ~env to_replicate =
  match !to_replicate with
  | ReplicateNothing -> ()
  | ReplicateThis p' ->
    (* replicate p' now; it will be useless from now on to replicate it again *)
    push_process ~env p' (ref ReplicateNothing);
    to_replicate := ReplicateNothing

(** Make a new transition node (linked to nothing) *)
let mk_transition_node transition to_replicate = 
  let node = {
    tn_transition = transition;
    tn_hook = (fun () -> ());
    tn_to_replicate = to_replicate;
  } in
  node
  
(** Perform the given transition (one send, one receive). *)
let perform_transition
: type a. env:run_env -> a transition_node -> a transition_node -> unit =
fun ~env sender receiver ->
  (* cleanup alternatives, replicate some processes if needed *)
  sender.tn_hook ();
  receiver.tn_hook ();
  check_replicate ~env sender.tn_to_replicate;
  check_replicate ~env receiver.tn_to_replicate;
  match sender.tn_transition, receiver.tn_transition with
  | Send (ch, x, send_p), Receive (ch', receive_p) ->
    assert (ch == ch');
    (* receiving channel gets the sent value *)
    let receive_p = receive_p x in
    (* push the two new processes (with no process to replicate) *)
    push_process ~env send_p (ref ReplicateNothing);
    push_process ~env receive_p (ref ReplicateNothing);
    ()
  | _ -> assert false

(** Check whether any transition in the list can be performed; otherwise,
    register all of them to their respective channels; Returns the
    list of corresponding  [transition_node] (empty if some
    transition fired immediately). *)
let try_transitions ~env transitions to_replicate =
  try
    let set_hooks, hook = List.fold_left
      (fun (set_hooks, hook) transition -> match transition with
        | Transition (Receive (ch, _) as transition) ->
          let receiver = mk_transition_node transition to_replicate in
          if DList.is_empty ch.senders
            then (* wait *)
              let dlist = DList.append ch.receivers receiver in
              (fun hook -> receiver.tn_hook <- hook) :: set_hooks,
              (fun () -> DList.remove dlist; hook ())
            else begin (* fire *)
              let sender = DList.pop ch.senders in
              perform_transition ~env sender receiver;
              hook ();  (* cancel previous sum cases *)
              raise Exit
            end
        | Transition (Send (ch, _, _) as transition) ->
          let sender = mk_transition_node transition to_replicate in
          if DList.is_empty ch.receivers
            then  (* wait *)
              let dlist = DList.append ch.senders sender in
              (fun hook -> sender.tn_hook <- hook) :: set_hooks,
              (fun () -> DList.remove dlist; hook ())
            else begin (* fire *)
              let receiver = DList.pop ch.receivers in
              perform_transition ~env sender receiver;
              hook ();  (* cancel previous sum cases *)
              raise Exit
            end)
      ([], fun () -> ()) transitions
    in
    (* we have a list of transition nodes; save it for when a transition fires *)
    List.iter (fun set_hook -> set_hook hook) set_hooks
  with Exit -> (* some transition fired immediately *)
    ()

(** Run the simulation until all processes are stuck, or stopped. *)
let run p =
  (* run tasks one by one until none remains *)
  let rec run  : env:run_env -> unit = fun ~env ->
    if not (Queue.is_empty env.tasks) then begin
      (* eval next process *)
      let p, to_replicate = Queue.pop env.tasks in
      eval_process ~env p to_replicate;
      run ~env
    end
  (* evaluate this process *)
  and eval_process : env:run_env -> process -> to_replicate ref -> unit
  = fun ~env p to_replicate ->
    match p with
    | Stop -> (* stop, but maybe there is a process to replicate *)
      check_replicate ~env to_replicate
    | New f ->
      (* apply [f] to a new chan *)
      let c = mk_chan () in
      let p' = f c in
      eval_process ~env p' to_replicate
    | Parallel l ->
      (* evaluate each process *)
      List.iter (fun p -> push_process ~env p to_replicate) l
    | Replicate p' ->
      (* run [p'] within an env where [p] is to be replicated *)
      let to_replicate' = ref (ReplicateThis p) in
      eval_process ~env p' to_replicate'
    | Escape f ->
      let p' = f () in
      push_process ~env p' to_replicate  (* yield before processing the result *)
    | Sum transitions ->
      try_transitions ~env transitions to_replicate
  in
  (* initial env *)
  let env = mk_env () in
  push_process ~env p (ref ReplicateNothing);
  run ~env
