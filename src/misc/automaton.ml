
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

(** {1 Automaton} *)

type ('s, -'i, +'o) t = 's -> 'i -> 's * 'o list
(** Transition function of an event automaton *)

type ('s, 'i, 'o) automaton = ('s, 'i, 'o) t

let map_i f a s i = a s (f i)

let map_o f a s i =
  let s', os = a s i in
  s', List.map f os

let fmap_o f a s i =
  let rec _fmap f l = match l with
    | [] -> []
    | x::l' -> f x @ _fmap f l'
  in
  let s', os = a s i in
  let os' = _fmap f os in
  s', os'

let filter_i p a s i =
  if p i
  then a s i
  else s, []

let filter_o p a s i =
  let s', os = a s i in
  s', List.filter p os

let fold f s i =
  let s' = f s i in
  s', [s']

let product f1 f2 (s1, s2) i =
  let s1', os1 = f1 s1 i in
  let s2', os2 = f2 s2 i in
  (s1', s2'), (os1 @ os2)

module I = struct
  type 'a t = 'a -> unit

  let create f = f

  let send x i = x i

  let comap f i x = i (f x)

  let filter f i x = if f x then i x
end

module O = struct
  type 'a t = {
    mutable n : int;  (* how many handlers? *)
    mutable handlers : ('a -> bool) array;
    mutable alive : keepalive;   (* keep some signal alive *)
  } (** Signal of type 'a *)

  and keepalive =
    | Keep : 'a t -> keepalive
    | NotAlive : keepalive

  let nop_handler x = true

  let create () =
    let s = {
      n = 0;
      handlers = Array.make 3 nop_handler;
      alive = NotAlive;
    } in
    s

  (* remove handler at index i *)
  let remove s i =
    (if i < s.n - 1  (* erase handler with the last one *)
      then s.handlers.(i) <- s.handlers.(s.n - 1));
    s.handlers.(s.n - 1) <- nop_handler; (* free handler *)
    s.n <- s.n - 1;
    ()

  let send s x =
    for i = 0 to s.n - 1 do
      while not (try s.handlers.(i) x with _ -> false) do
        remove s i  (* i-th handler is done, remove it *)
      done
    done

  let on s f =
    (* resize handlers if needed *)
    (if s.n = Array.length s.handlers
      then begin
        let handlers = Array.make (s.n + 4) nop_handler in
        Array.blit s.handlers 0 handlers 0 s.n;
        s.handlers <- handlers
      end);
    s.handlers.(s.n) <- f;
    s.n <- s.n + 1

  let once s f =
    on s (fun x -> ignore (f x); false)

  let propagate a b =
    on a (fun x -> send b x; true)

  let map f signal =
    let signal' = create () in
    (* weak ref *)
    let r = Weak.create 1 in
    Weak.set r 0 (Some signal');
    on signal (fun x ->
      match Weak.get r 0 with
      | None -> false
      | Some signal' -> send signal' (f x); true);
    signal'.alive <- Keep signal;
    signal'

  let filter p signal =
    let signal' = create () in
    (* weak ref *)
    let r = Weak.create 1 in
    Weak.set r 0 (Some signal');
    on signal (fun x ->
      match Weak.get r 0 with
      | None -> false
      | Some signal' -> (if p x then send signal' x); true);
    signal'.alive <- Keep signal;
    signal'
end

let connect o i =
  O.on o (fun x -> I.send i x; true)

module Instance = struct
  type ('s, 'i, 'o) t = {
    transition : ('s, 'i, 'o) automaton;
    mutable i : 'i I.t;
    o : 'o O.t;
    transitions : ('s * 'i * 's * 'o list) O.t;
    mutable state : 's;
  }

  let transition_function a = a.transition

  let i a = a.i

  let o a = a.o

  let state a = a.state

  let transitions a = a.transitions

  let send a i = I.send a.i i

  let _q = Queue.create ()

  let _process q =
    while not (Queue.is_empty q) do
      let task = Queue.pop q in
      task ()
    done

  let _schedule q task = Queue.push task q

  let _do_transition q a i =
    let s = a.state in
    let s', os = a.transition s i in
    (* update state *)
    a.state <- s';
    (* trigger the transitions asap *)
    _schedule q (fun () -> O.send a.transitions (s, i, s', os));
    List.iter
      (fun o -> _schedule q (fun () -> O.send a.o o))
      os 

  let _receive a i =
    let first = Queue.is_empty _q in
    _do_transition _q a i;
    if first then _process _q

  let create ~f init =
    let o = O.create () in
    let transitions = O.create () in
    (* create input and automaton *)
    let a = { state = init; i=Obj.magic 0; o; transition=f; transitions; } in
    a.i <- _receive a;
    a
end
