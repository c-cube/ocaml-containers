
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

(** {1 Functional streams for Lwt} *)

type 'a t = [ `Nil | `Cons of 'a * (unit -> 'a t) ] Lwt.t
type 'a stream = 'a t

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let empty = Lwt.return `Nil

let cons x l = Lwt.return (`Cons (x, fun () -> l))

let rec of_list_rec l () = match l with
  | [] -> empty
  | x :: tl -> Lwt.return (`Cons (x, of_list_rec tl))

let of_list l : 'a t = of_list_rec l ()

let rec create_rec f () : 'a t =
  f () >|= function
  | None -> `Nil
  | Some x -> `Cons (x, create_rec f)

let create f = create_rec f ()

let next l =
  l >|= function
  | `Nil -> None
  | `Cons (x, tl) -> Some (x, tl())

let next_exn l =
  l >>= function
  | `Nil -> Lwt.fail Not_found
  | `Cons (x, tl) -> Lwt.return (x, tl ())

let rec map_rec f l () =
  l >|= function
  | `Nil -> `Nil
  | `Cons (x, tl) -> `Cons (f x, map_rec f (tl ()))

let map f (l:'a t) : 'b t = map_rec f l ()

let rec map_s_rec (f:'a -> 'b Lwt.t) l () =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) ->
      f x >|= fun y -> `Cons (y, map_s_rec f (tl ()))

let map_s f l = map_s_rec f l ()

let rec append_rec l1 l2 () =
  l1 >>= function
  | `Nil -> l2
  | `Cons (x, tl1) -> Lwt.return (`Cons (x, append_rec (tl1 ()) l2))

let append l1 l2 = append_rec l1 l2 ()

let rec flat_map f l =
  l >>= function
  | `Nil -> empty
  | `Cons (x, tl) -> append (f x) (flat_map f (tl ()))

let rec iter f l =
  l >>= function
  | `Nil -> Lwt.return_unit
  | `Cons (x, tl) ->  f x; iter f (tl ())

let rec iter_s f l =
  l >>= function
  | `Nil -> Lwt.return_unit
  | `Cons (x, tl) ->  f x >>= fun () -> iter_s f (tl ())

module Queue = struct
  type 'a t = {
    bufsize : int;
    cond : unit Lwt_condition.t;
    q : 'a Queue.t;
    mutable str : 'a stream;
    mutable closed : bool;
  }

  (* function that waits for the next element, and recursively,
    returning a stream of values *)
  let rec make_stream_ t () : 'a stream =
    if t.closed then empty
    else if not (Queue.is_empty t.q)
      then (
        let x = Queue.pop t.q in
        Lwt_condition.signal t.cond ();
        Lwt.return (`Cons (x, make_stream_ t))
      )
    else
      (* wait for something to happen *)
      Lwt_condition.wait t.cond >>= make_stream_ t

  let create ?(bufsize=128) () =
    let t = {
      bufsize;
      q = Queue.create ();
      str = empty;
      cond = Lwt_condition.create ();
      closed = false;
    } in
    t.str <- make_stream_ t ();
    t

  exception ClosedQueue

  let close t =
    if not t.closed then (
      t.closed <- true;
      Lwt_condition.signal t.cond ()
    )

  let rec push_rec t x () =
    if t.closed then raise ClosedQueue;
    if Queue.length t.q = t.bufsize
      then Lwt_condition.wait t.cond >>= push_rec t x
    else (
      Queue.push x t.q;
      Lwt.return_unit
    )

  let push t x = push_rec t x ()

  let to_stream t = t.str

  let take t = assert false
  let take_exn t = assert false

end


