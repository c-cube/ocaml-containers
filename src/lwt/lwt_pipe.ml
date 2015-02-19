
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

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a step = ['a or_error | `End]

let (>>=) = Lwt.(>>=)

module LwtErr = struct
  type 'a t = 'a or_error Lwt.t

  let return x = Lwt.return (`Ok x)

  let return_unit = Lwt.return (`Ok ())

  let fail msg = Lwt.return (`Error msg)

  let (>>=) x f =
    Lwt.bind x
      (function
        | `Error msg -> fail msg
        | `Ok y -> f y
      )

  let (>|=) x f =
    Lwt.map
      (function
        | `Error _ as e -> e
        | `Ok x -> `Ok (f x)
      ) x
end

let (>>|=) = LwtErr.(>|=)

let ret_end = Lwt.return `End

exception Closed

module Pipe = struct
  (* messages given to writers through the condition *)
  type 'a msg =
    | Send of 'a step Lwt.u (* send directly to reader *)
    | SendQueue (* push into queue *)
    | Close (* close *)

  type 'a inner_buf =
    | Buf of 'a step Queue.t * int  (* buf, max size *)
    | NoBuf

  type ('a, +'perm) t = {
    close : unit Lwt.u;
    closed : unit Lwt.t;
    lock : Lwt_mutex.t;
    buf : 'a inner_buf;
    cond : 'a msg Lwt_condition.t;
    mutable keep : unit Lwt.t list;  (* do not GC, and wait for completion *)
  } constraint 'perm = [< `r | `w]

  let create ?(max_size=0) () =
    let buf = match max_size with
      | 0 -> NoBuf
      | n when n < 0 -> invalid_arg "max_size"
      | n -> Buf (Queue.create (), n)
    in
    let closed, close = Lwt.wait () in
    {
      close;
      closed;
      buf;
      lock=Lwt_mutex.create();
      cond=Lwt_condition.create();
      keep=[];
    }

  let keep p fut = p.keep <- fut :: p.keep

  let is_closed p = not (Lwt.is_sleeping p.closed)

  let close p =
    if is_closed p then Lwt.return_unit
    else (
      Lwt.wakeup p.close (); (* evaluate *)
      Lwt_condition.broadcast p.cond Close;
      Lwt.join p.keep;
    )

  let close_async p = Lwt.async (fun () -> close p)

  let on_close p = p.closed

  (* try to take next element from buffer *)
  let try_next_buf t = match t.buf with
    | NoBuf -> None
    | Buf (q, _) ->
      if Queue.is_empty q then None
      else Some (Queue.pop q)

  (* returns true if it could push successfully *)
  let try_push_buf t x = match t.buf with
    | NoBuf -> false
    | Buf (q, max_size) when Queue.length q = max_size -> false
    | Buf (q, _) -> Queue.push x q; true

  (* read next one *)
  let read t =
    Lwt_mutex.with_lock t.lock
      (fun () ->
         match try_next_buf t with
         | None when is_closed t -> ret_end (* end of stream *)
         | None ->
           let fut, send = Lwt.wait () in
           Lwt_condition.signal t.cond (Send send);
           fut
         | Some x ->
           Lwt_condition.signal t.cond SendQueue;  (* queue isn't full anymore *)
           Lwt.return x
      )

  (* write a value *)
  let write t x =
    let rec try_write () =
      if is_closed t then Lwt.fail Closed
      else if try_push_buf t x
      then Lwt.return_unit  (* into buffer, do not wait *)
      else (
        (* wait for readers to consume the queue *)
        Lwt_condition.wait ~mutex:t.lock t.cond >>= fun msg ->
        match msg with
        | Send s ->
          Lwt.wakeup s x;  (* sync with reader *)
          Lwt.return_unit
        | SendQueue -> try_write () (* try again! *)
        | Close -> Lwt.fail Closed
      )
    in
    Lwt_mutex.with_lock t.lock try_write

  let rec connect_rec r w =
    read r >>= function
    | `End -> Lwt.return_unit
    | (`Error _ | `Ok _) as step ->
      write w step >>= fun () ->
      connect_rec r w

  let connect a b =
    let fut = connect_rec a b in
    keep b fut

  (* close a when b closes *)
  let close_when_closed a b =
    Lwt.on_success b.closed
      (fun () -> close_async a)

  (* close a when every member of l closes *)
  let close_when_all_closed a l =
    let n = ref (List.length l) in
    List.iter
      (fun p -> Lwt.on_success p.closed
          (fun () ->
             decr n;
             if !n = 0 then close_async a
          )
      ) l
end

module Writer = struct
  type 'a t = ('a, [`w]) Pipe.t

  let write t x = Pipe.write t (`Ok x)

  let write_error t msg = Pipe.write t (`Error msg)

  let rec write_list t l = match l with
    | [] -> Lwt.return_unit
    | x :: tail ->
      write t x >>= fun () -> write_list t tail

  let map ~f a =
    let b = Pipe.create() in
    let rec fwd () =
      Pipe.read b >>= function
      | `Ok x -> write a (f x) >>= fwd
      | `Error msg -> write_error a msg >>= fwd
      | `End -> Lwt.return_unit
    in
    Pipe.keep b (fwd());
    (* when a gets closed, close b too *)
    Lwt.on_success (Pipe.on_close a) (fun () -> Pipe.close_async b);
    b

  let send_all l =
    if l = [] then invalid_arg "send_all";
    let res = Pipe.create () in
    let rec fwd () =
      Pipe.read res >>= function
      | `End -> Lwt.return_unit
      | `Ok x -> Lwt_list.iter_p (fun p -> write p x) l >>= fwd
      | `Error msg -> Lwt_list.iter_p (fun p -> write_error p msg) l >>= fwd
    in
    (* do not GC before res dies; close res when any outputx is closed *)
    Pipe.keep res (fwd ());
    List.iter (Pipe.close_when_closed res) l;
    res

  let send_both a b = send_all [a; b]
end

module Reader = struct
  type 'a t = ('a, [`r]) Pipe.t

  let read = Pipe.read

  let map ~f a =
    let b = Pipe.create () in
    let rec fwd () =
      Pipe.read a >>= function
      | `Ok x -> Pipe.write b (`Ok (f x)) >>= fwd
      | (`Error _) as e -> Pipe.write b e >>= fwd
      | `End -> Pipe.close b
    in
    Pipe.keep b (fwd());
    b

  let filter_map ~f a =
    let b = Pipe.create () in
    let rec fwd () =
      Pipe.read a >>= function
      | `Ok x ->
        begin match f x with
          | None -> fwd()
          | Some y -> Pipe.write b (`Ok y) >>= fwd
        end
      | (`Error _) as e -> Pipe.write b e >>= fwd
      | `End -> Pipe.close b
    in
    Pipe.keep b (fwd());
    b

  let rec fold ~f ~x t =
    read t >>= function
    | `End -> LwtErr.return x
    | `Error msg -> LwtErr.fail msg
    | `Ok y -> fold ~f ~x:(f x y) t

  let rec fold_s ~f ~x t =
    read t >>= function
    | `End -> LwtErr.return x
    | `Error msg -> LwtErr.fail msg
    | `Ok y ->
      f x y >>= fun x -> fold_s ~f ~x t

  let rec iter ~f t =
    read t >>= function
    | `End -> LwtErr.return_unit
    | `Error msg -> LwtErr.fail msg
    | `Ok x -> f x; iter ~f t

  let rec iter_s ~f t =
    read t >>= function
    | `End -> LwtErr.return_unit
    | `Error msg -> LwtErr.fail msg
    | `Ok x -> f x >>= fun () -> iter_s ~f t

  let merge_all l =
    if l = [] then invalid_arg "merge_all";
    let res = Pipe.create () in
    List.iter (fun p -> Pipe.connect p res) l;
    (* connect res' input to all members of l; close res when they all close *)
    Pipe.close_when_all_closed res l;
    res

  let merge_both a b = merge_all [a; b]
end

(** {2 Conversions} *)

let of_list l : _ Reader.t =
  let p = Pipe.create ~max_size:0 () in
  Pipe.keep p (Lwt_list.iter_s (Writer.write p) l >>= fun () -> Pipe.close p);
  p

let of_array a =
  let p = Pipe.create ~max_size:0 () in
  let rec send i =
    if i = Array.length a then Pipe.close p
    else (
      Writer.write p a.(i) >>= fun () ->
      send (i+1)
    )
  in
  Pipe.keep p (send 0);
  p

let of_string a =
  let p = Pipe.create ~max_size:0 () in
  let rec send i =
    if i = String.length a then Pipe.close p
    else (
      Writer.write p (String.get a i) >>= fun () ->
      send (i+1)
    )
  in
  Pipe.keep p (send 0);
  p

let to_rev_list r =
  Reader.fold ~f:(fun acc x -> x :: acc) ~x:[] r

let to_list r = to_rev_list r >>|= List.rev

let to_list_exn r =
  to_list r >>= function
  | `Error msg -> Lwt.fail (Failure msg)
  | `Ok x -> Lwt.return x

let to_buffer buf =
  let p = Pipe.create () in
  Pipe.keep p (
    Reader.iter ~f:(fun c -> Buffer.add_char buf c) p >>= fun _ ->
    Lwt.return_unit
  );
  p

let to_buffer_str buf =
  let p = Pipe.create () in
  Pipe.keep p (
    Reader.iter ~f:(fun s -> Buffer.add_string buf s) p >>= fun _ ->
    Lwt.return_unit
  );
  p

(** {2 Basic IO wrappers} *)

module IO = struct
  let read ?(bufsize=4096) ic : _ Reader.t =
    let buf = Bytes.make bufsize ' ' in
    let p = Pipe.create ~max_size:0 () in
    let rec send() =
      Lwt_io.read_into ic buf 0 bufsize >>= fun n ->
      if n = 0 then Pipe.close p
      else
        Writer.write p (Bytes.sub_string buf 0 n) >>= fun () ->
        send ()
    in Lwt.async send;
    p

  let read_lines ic =
    let p = Pipe.create () in
    let rec send () =
      Lwt_io.read_line_opt ic >>= function
      | None -> Pipe.close p
      | Some line -> Writer.write p line >>= fun () -> send ()
    in
    Lwt.async send;
    p

  let write oc =
    let p = Pipe.create () in
    Pipe.keep p (
      Reader.iter_s ~f:(Lwt_io.write oc) p >>= fun _ ->
      Pipe.close p
    );
    p

  let write_lines oc =
    let p = Pipe.create () in
    Pipe.keep p (
      Reader.iter_s ~f:(Lwt_io.write_line oc) p >>= fun _ ->
      Pipe.close p
    );
    p
end
