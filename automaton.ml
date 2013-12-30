
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

(** {1 Distributed Algorithms} *)

type ('s, -'i, +'o) t = 's -> 'i -> 's * 'o list
(** Transition function of an event automaton *)

module EventQueue = struct
  type t = (unit -> unit) Queue.t

  let create () = Queue.create ()

  let default = create ()

  let _process q =
    while not (Queue.is_empty q) do
      let task = Queue.pop q in
      task ()
    done

  let _schedule q task = Queue.push task q
end

(* empty callback *)
let __noop s i os = true

type ('s, 'i, 'o) instance = {
  transition : ('s, 'i, 'o) t;
  queue : EventQueue.t;
  mutable state : 's;
  mutable connections : 'o connection list;
  mutable n_callback : int;
  mutable callback : ('s -> 'i -> 's * 'o list -> bool) array;
}

(* connection to another automaton *)
and 'a connection = Conn : (_, 'a, _) instance -> 'a connection

let instantiate ?(queue=EventQueue.default) ~f init = {
  transition = f;
  queue;
  state = init;
  connections = [];
  n_callback = 0;
  callback = Array.make 3 __noop;
}

let transition a i = a.transition a.state i

let state a = a.state

(* register a new callback *)
let on_transition a k =
  if Array.length a.callback = a.n_callback
  then begin
    let callback' = Array.make (2 * a.n_callback) __noop in
    Array.blit a.callback 0 callback' 0 a.n_callback;
    a.callback <- callback';
  end;
  a.callback.(a.n_callback) <- k;
  a.n_callback <- a.n_callback + 1

let connect ~left ~right =
  left.connections <- (Conn right) :: left.connections

(* remove i-th callback of [a] *)
let _remove_callback a i =
  if i < a.n_callback
  then a.callback.(i) <- a.callback.(a.n_callback - 1);
  (* avoid memleak *)
  a.callback.(a.n_callback - 1) <- __noop;
  a.n_callback <- a.n_callback - 1

(* process callback at index [n] *)
let rec _call_callbacks a n s i o  =
  if n >= a.n_callback then ()
  else try
    let keep = a.callback.(n) s i o in
    if keep
    then _call_callbacks a (n+1) s i o
    else begin
      _remove_callback a n;
      _call_callbacks a n s i o  (* same index, the callback has been removed *)
    end
  with _ -> _call_callbacks a (n+1) s i o

(* send input to automaton *)
let rec send : type s i o. (s, i, o) instance -> i -> unit
= fun a i ->
  let first = Queue.is_empty a.queue in
  (* compute transitions *)
  let s = a.state in
  let s', os = a.transition a.state i in
  a.state <- s';
  (* callbacks *)
  _call_callbacks a 0 s i (s', os);
  (* connections to other automata *)
  List.iter
    (fun o -> _forward_connections a.connections o)
    os;
  (* if no enclosing call to [send], we need to process events *)
  if first then EventQueue._process a.queue

and _forward_connections : type a. a connection list -> a -> unit 
= fun l o -> match l with
  | [] -> ()
  | (Conn a') :: l' ->
      EventQueue._schedule a'.queue (fun () -> send a' o);
      _forward_connections l' o
