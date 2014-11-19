
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

(** {1 Small Actor system for Lwt} *)

module ITbl = Hashtbl.Make(struct
  type t = int
  let equal (i:int) j = i=j
  let hash i = i land max_int
end)

(** {2 Actors Basics} *)

let (>>=) = Lwt.(>>=)

type 'a t = {
  mutable inbox : 'a Queue.t;
  cond : unit Lwt_condition.t;
  act : 'a t -> 'a -> unit Lwt.t;
  setup : unit -> unit Lwt.t;
  pid : int;
  mutable links : any_actor list;
  mutable monitors : monitor list;
  mutable thread : unit Lwt.t option;  (* running thread *)
}
(* invariant: thead=Some t  means  that t is running, and the
  actor is alive *)

and any_actor =
  | AnyActor : _ t -> any_actor
and monitor =
  | Monitor : [> `Died of any_actor] t -> monitor

(* send message *)
let send m x =
  Queue.push x m.inbox;
  Lwt_condition.signal m.cond ();
  Lwt.return_unit

(* [a] just died, now kill its friends *)
let propagate_dead a =
  let traversed = ITbl.create 16 in
  (* depth-first traversal of the clique of linked actors *)
  let rec traverse stack = match stack with
    | [] -> ()
    | AnyActor a :: stack' when ITbl.mem traversed a.pid ->
        traverse stack'
    | (AnyActor a) as any_a :: stack' ->
        ITbl.add traversed a.pid ();
        begin match a.thread with
          | None -> ()
          | Some t ->
              Lwt.cancel t;
              a.thread <- None;
        end;
        (* notify monitors that [a] died *)
        let monitors = a.monitors in
        Lwt.async
          (fun () ->
            Lwt_list.iter_p
              (function Monitor m -> send m (`Died any_a)
              ) monitors
            );
        (* follow links to other actors to kill *)
        let stack' = List.rev_append a.links stack' in
        traverse stack'
  in
  traverse [AnyActor a]

(* number of active actors *)
let num_active = ref 0
let on_num_active_0 = Lwt_condition.create()

let decr_num_active () =
  decr num_active;
  assert (!num_active >= 0);
  if !num_active = 0 then Lwt_condition.broadcast on_num_active_0 ()

(* how to start an actor *)
let start_ a =
  (* main loop of the actor *)
  let rec loop () =
    Lwt_condition.wait a.cond >>= fun () ->
    let x = Queue.pop a.inbox in
    a.act a x >>= fun () ->
    loop ()
  and exn_handler e =
    Lwt_log.ign_info_f ~exn:e "error in thread %d" a.pid;
    propagate_dead a;
    Lwt.return_unit
  in
  match a.thread with
  | Some _ -> failwith "start: actor already running";
  | None ->
      (* start the thread *)
      let thread = Lwt.catch (fun () -> a.setup () >>= loop) exn_handler in
      (* maintain [num_active] *)
      incr num_active;
      Lwt.on_termination thread decr_num_active;
      a.thread <- Some thread;
      ()

let kill a = propagate_dead a

let no_setup_ () = Lwt.return_unit

let pid a = a.pid

let cur_pid = ref 0

let monitor m a =
  a.monitors <- Monitor m :: a.monitors

let link a b =
  if a.thread = None
    then kill b
  else if b.thread = None
    then kill a;
  a.links <- AnyActor b :: a.links;
  b.links <- AnyActor a :: b.links;
  ()

let spawn ?(links=[]) ?(setup=no_setup_) act =
  let pid = !cur_pid in
  incr cur_pid;
  let a = {
    inbox=Queue.create ();
    cond = Lwt_condition.create();
    act;
    setup;
    pid;
    links=[];
    monitors=[];
    thread=None;
  } in
  start_ a;
  (* link now *)
  List.iter (function AnyActor b -> link a b) links;
  a

let cur_timeout_id = ref 0

let timeout a f =
  if f <= 0. then invalid_arg "timeout";
  let i = !cur_timeout_id in
  incr cur_timeout_id;
  let _ = Lwt_engine.on_timer f false
    (fun _ -> Lwt.async (fun () -> send a (`Timeout i)))
  in
  i

(* wait until num_active=0 *)
let rec wait_all () =
  if !num_active = 0
    then Lwt.return_unit
    else
      Lwt_condition.wait on_num_active_0 >>= fun () ->
      wait_all ()
