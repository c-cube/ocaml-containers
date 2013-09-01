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

(** {1 Composable State Machines} *)

(** {2 Basic interface} *)

type 'state t = {
  id : int;
  mutable state : 'state;
  mutable callbacks : 'state callback array;
  mutable callbacks_num : int; 
} (** State machine, whose states are of the type 'state,
      and that changes state upon events of the type 'event. *)

and 'a sm = 'a t

and 'event sink = 'event -> unit

and 'a transition =
  | TransitionTo of 'a
  | TransitionStay
  (** A transition of a state machine whose states are
      of type 'a *)

and 'a callback = 'a -> 'a -> bool
  (** A callback that is called during a transition between two 'a states *)

and task_queue = (unit -> unit) Queue.t
  (** Queue of tasks to process *)

type poly_ref =
  | PolyRef : 'a t -> poly_ref
  (** Polymorphic reference to a state machine *)

module SMSet = Set.Make(struct
  type t = poly_ref
  let compare st1_ref st2_ref =
    match st1_ref, st2_ref with
    | PolyRef s1, PolyRef s2 -> s1.id - s2.id
end)

let __id = ref 0
let __roots = ref SMSet.empty
let __default_callback _ _ = true
let __queue = Queue.create ()   (* queue to use to process events *)
let __fresh_id () =
  let n = !__id in
  incr __id;
  n

let make_root st =
  __roots := SMSet.add (PolyRef st) !__roots

let remove_root st =
  __roots := SMSet.remove (PolyRef st) !__roots

(* make a transition *)
let _do_transition st new_state =
  Queue.push
    (fun () ->
      let old_state = st.state in
      st.state <- new_state;
      for i = 0 to st.callbacks_num - 1 do
        try
          let keep = st.callbacks.(i) old_state new_state in
          if not keep then begin
            (* remove this callback *)
            (if i < st.callbacks_num - 1 then
              st.callbacks.(i) <- st.callbacks.(st.callbacks_num - 1));
            st.callbacks_num <- st.callbacks_num - 1;
          end;
        with e ->
          ()  (* TODO: some global error handler? *)
      done)
    __queue

(* create a SM *)
let mk_sm ~init =
  let st = {
    id = __fresh_id ();
    state = init;
    callbacks = Array.make 4 __default_callback;
    callbacks_num = 0;
  } in
  st

(* create a SM with a transition function *)
let create ?(root=false) ~init ~trans =
  let st = mk_sm ~init in
  let sink e = match trans st.state e with
    | TransitionStay -> ()
    | TransitionTo new_state ->
      _do_transition st new_state
  in
  (if root then make_root st);
  st, sink

let id st = st.id

let state st = st.state

let eq st1 st2 = st1.id = st2.id

let hash st = st.id

let compare st1 st2 = st1.id - st2.id

let register_while st callback =
  (if st.callbacks_num = Array.length st.callbacks
    then begin
      let a = Array.make (2*st.callbacks_num) __default_callback in
      Array.blit st.callbacks 0 a 0 st.callbacks_num;
      st.callbacks <- a
    end);
  st.callbacks.(st.callbacks_num) <- callback;
  st.callbacks_num <- st.callbacks_num + 1;
  ()

let register st callback =
  register_while st (fun a b -> callback a b; true)

let connect st sink =
  register_while st (fun _ new_state -> sink new_state; true)

(** {2 Combinators} *)

let map st f =
  let st' = mk_sm ~init:(f st.state) in
  let a = Weak.create 1 in
  Weak.set a 0 (Some st');
  register_while st
    (fun _ new_state ->
      match Weak.get a 0 with
      | None -> false
      | Some st' ->
        _do_transition st' (f new_state);
        true);
  st'

let filter st p =
  let st' = mk_sm ~init:st.state in
  let a = Weak.create 1 in
  Weak.set a 0 (Some st');
  register_while st
    (fun _ new_state ->
      if p
        then begin match Weak.get a 0 with
        | None -> false
        | Some st' ->
          _do_transition st' new_state;
          true
        end else true);
  st'

let seq_list l =
  let init = List.map state l in
  let _array = Array.of_list init in
  let st' = mk_sm ~init in
  let a = Weak.create 1 in
  Weak.set a 0 (Some st');
  List.iteri
    (fun i st ->
      register_while st
        (fun _ new_state ->
          match Weak.get a 0 with
          | None -> false
          | Some st' ->
            _array.(i) <- new_state;
            _do_transition st' (Array.to_list _array);
            true))
    l;
  st'

(** {2 Unix wrappers} *)

module Unix = struct
  type fd_state =
    | FD_wait of Unix.file_descr
    | FD_ready_read of Unix.file_descr
    | FD_ready_write of Unix.file_descr
    | FD_exc_condition of Unix.file_descr

  let select read write exc =
    assert false

  let run () =
    while not (Queue.is_empty __queue) do
      let task = Queue.pop __queue in
      task ()
    done;
    ()
end

