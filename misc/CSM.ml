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

(** {1 Composable State Machines}

This module defines state machines that should help design applications
with a more explicit control of state (e.g. for networking applications. *)

type ('a, 's, 'b) t = 's -> 'a -> ('b * 's) option
(** transition function that fully describes an automaton *)

type ('a, 's, 'b) automaton = ('a, 's, 'b) t

(** {2 Basic Interface} *)

let empty _st _x = None

let id () x = Some (x,())

let repeat x () () = Some (x, ())

let get_state a state x = match a state x with
  | None -> None
  | Some (_, state') -> Some (state', state')

let next a s x = a s x

let scan a (st, prev) x =
  match a st x with
  | None -> None
  | Some (y,state') ->
      Some (y::prev, (state', y::prev))

let lift f state x =
  let state' = f state x in
  Some (state', state')

let ignore_state f state x = Some (f x, state)

let ignore_arg f state _x =
  let state' = f state in
  Some (state', state')

let map_in f a state x = a state (f x)
let map_out f a state x = match a state x with
  | None -> None
  | Some (y, state') ->
      Some (f y, state')

exception ExitNest

let nest l =
  let rec eval (answers, res_states) l state x =
    match l, state with
    | [], [] ->
      Some (List.rev answers, List.rev res_states)
    | a::l', state::states' ->
        begin match a state x with
        | None -> raise ExitNest
        | Some (ans,state') ->
          eval (ans::answers, state'::res_states) l' states' x
        end
    | [], _
    | _, [] ->
      raise (Invalid_argument "CSM.next: list length mismatch")
  in
  fun state x ->
    try eval ([],[]) l state x
    with ExitNest -> None

let split a state x = match a state x with
  | None -> None
  | Some (y, state') -> Some ((y,y), state')

let unsplit merge a state x = match a state x with
  | None -> None
  | Some ((y,z), state') ->
      Some (merge y z, state')

let pair a1 a2 (s1,s2) (x1,x2) =
  match a1 s1 x1, a2 s2 x2 with
  | Some (y1,s1'), Some (y2, s2') ->
      Some ((y1,y2), (s1',s2'))
  | Some _, None
  | None, Some _
  | None, None -> None

let ( *** ) = pair

let first a state (x,keep) = match a state x with
  | None -> None
  | Some (y,state') ->
      Some ((y,keep), state')

let second a state (keep,x) = match a state x with
  | None -> None
  | Some (y,state') ->
      Some ((keep,y), state')

let (>>>) a1 a2 (s1, s2) x =
  match a1 s1 x with
  | None -> None
  | Some (y, s1') ->
      match a2 s2 y with
      | None -> None
      | Some (z, s2') ->
          Some (z, (s1', s2'))

let _flatmap_opt f o = match o with
  | None -> None
  | Some x -> f x

type ('s1,'s2) append_state =
  | Left of 's1 * 's2
  | Right of 's2

let rec append a1 a2 state x =
  match state with
  | Left (s1,s2) ->
      begin match a1 s1 x with
      | None -> append a1 a2 (Right s2) x
      | Some (y, s1') ->
          Some (y, Left (s1', s2))
      end
  | Right s2 ->
      _flatmap_opt (fun (y,s2) -> Some (y,Right s2)) (a2 s2 x)

let rec flatten (automata,state) x = match automata with
  | [] -> None
  | a::automata' ->
      match a state x with
      | None -> flatten (automata', state) x
      | Some (y, state') ->
          Some (y, (automata,state'))

let filter p a state x = match a state x with
  | None -> None
  | Some (y, state') ->
      if p y then Some (Some y, state') else Some (None, state')

type ('a, 'c, 's1, 's2) flat_map_state =
  ('s1 * (('a, 's2, 'c) t * 's2) option)

let rec flat_map f a state x =
  match state with
  | s1, None ->
      begin match a s1 x with
      | None -> None
      | Some (y, s1') ->
          let a2, s2 = f y in
          flat_map f a (s1', Some (a2,s2)) x
      end
  | s1, Some(a2,s2) ->
      begin match a2 s2 x with
      | None -> flat_map f a (s1, None) x
      | Some (z, s2') ->
          let state' = s1, Some (a2, s2') in
          Some (z, state')
      end

let run_list a ~init l =
  let rec aux acc state l = match l with
  | [] -> List.rev acc
  | x::l' ->
      match next a state x with
      | None -> List.rev acc
      | Some (y, state') ->
          aux (y::acc) state' l'
  in
  aux [] init l

(** {2 Instances} *)

module Int = struct
  let range j state () =
    if state > j then None
    else Some (state, state+1)
end

let list_map = List.map
let list_split = List.split

module List = struct
  let iter state () = match state with
    | [] -> None
    | x::l -> Some (x, l)

  let build state x = Some (x::state, x::state)
end

module Gen = struct
  type 'a gen = unit -> 'a option

  let map a state gen =
    let st = ref state in
    fun () ->
      match gen() with
      | None -> None
      | Some x ->
          begin match a !st x with
          | None -> None
          | Some (y, state') ->
              st := state';
              Some y
          end
end

module Sequence = struct
  type 'a sequence = ('a -> unit) -> unit

  exception ExitSeq

  let map a state seq =
    fun k ->
      let st = ref state in
      try
        seq (fun x -> match a !st x with
          | None -> raise ExitSeq
          | Some (y, state') ->
              st := state';
              k y)
      with ExitSeq -> ()
end

module KList = struct
  type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

  let rec map f state (l:'a klist) () =
    match l () with
    | `Nil -> `Nil
    | `Cons (x, l') ->
        begin match f state x with
        | None -> `Nil
        | Some (y, state') ->
          `Cons (y, map f state' l')
        end
end

(** {2 Mutable Interface} *)

module Mut = struct
  type ('a, 's, 'b) t = {
    next : ('a, 's, 'b) automaton;
    mutable state : 's;
  } (** mutable automaton, with in-place modification *)

  let create a ~init =
    { next=a; state=init; }

  let next a x =
    match a.next a.state x with
    | None -> None
    | Some (y,state) ->
        a.state <- state;
        Some y

  let copy a = { a with state=a.state; }

  let cur_state a = a.state

  let get_state a = {
    next=get_state a.next;
    state=a.state;
  }

  let scan a = {
    next = scan a.next;
    state = a.state, [];
  }

  let nest l =
    let nexts, states =
      list_split (list_map (fun a -> a.next, a.state) l)
    in
    { next=nest nexts; state=states; }

  let append a1 a2 = {
    next = append a1.next a2.next;
    state = Left (a1.state, a2.state);
  }

  let rec iter f a = match next a () with
    | None -> ()
    | Some y -> f y; iter f a

  module Int = struct
    let range i j = {
      next=Int.range j;
      state=i;
    }
  end

  module List = struct
    let iter l = create List.iter ~init:l

    let build l = create List.build ~init:l
  end
end
