
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
  type ('a, +'perm) t = {
    close : unit Lwt.u;
    closed : unit Lwt.t;
    readers : 'a step Lwt.u Queue.t;  (* readers *)
    writers : 'a step Queue.t;
    blocked_writers : ('a step * unit Lwt.u) Queue.t; (* blocked writers *)
    max_size : int;
    mutable keep : unit Lwt.t list;  (* do not GC, and wait for completion *)
  } constraint 'perm = [< `r | `w]

  let create ?(max_size=0) () =
    let closed, close = Lwt.wait () in
    {
      close;
      closed;
      readers = Queue.create ();
      writers = Queue.create ();
      blocked_writers = Queue.create ();
      max_size;
      keep=[];
    }

  let keep p fut = p.keep <- fut :: p.keep

  let is_closed p = not (Lwt.is_sleeping p.closed)

  let close p =
    if is_closed p then Lwt.return_unit
    else (
      Lwt.wakeup p.close (); (* evaluate *)
      Lwt.join p.keep;
    )

  let close_async p = Lwt.async (fun () -> close p)

  let wait p = Lwt.map (fun _ -> ()) p.closed

  (* try to take next element from writers buffer *)
  let try_read t =
    if Queue.is_empty t.writers
    then if Queue.is_empty t.blocked_writers
      then None
      else (
        assert (t.max_size = 0);
        let x, signal_done = Queue.pop t.blocked_writers in
        Lwt.wakeup signal_done ();
        Some x
      )
    else (
      let x = Queue.pop t.writers in
      (* some writer may unblock *)
      if not (Queue.is_empty t.blocked_writers) && Queue.length t.writers < t.max_size then (
        let y, signal_done = Queue.pop t.blocked_writers in
        Queue.push y t.writers;
        Lwt.wakeup signal_done ();
      );
      Some x
    )

  (* read next one *)
  let read t = match try_read t with
     | None when is_closed t -> ret_end (* end of stream *)
     | None ->
       let fut, send = Lwt.wait () in
       Queue.push send t.readers;
       fut
     | Some x -> Lwt.return x

  (* TODO: signal writers when their value has less than max_size
  steps before being read *)

  (* write a value *)
  let write t x =
    if is_closed t then Lwt.fail Closed
    else if Queue.length t.readers > 0
      then (
        let send = Queue.pop t.readers in
        Lwt.wakeup send x;
        Lwt.return_unit
      )
    else if Queue.length t.writers < t.max_size
      then (
        Queue.push x t.writers;
        Lwt.return_unit  (* into buffer, do not wait *)
      )
    else (
      let is_done, signal_done = Lwt.wait () in
      Queue.push (x, signal_done) t.blocked_writers;
      is_done (* block *)
    )

  let rec connect_rec r w =
    read r >>= function
    | `End -> Lwt.return_unit
    | `Error _ as step -> write w step
    | `Ok _ as step ->
      write w step >>= fun () ->
      connect_rec r w

  let connect a b =
    let fut = connect_rec a b in
    keep b fut

  (* close a when b closes *)
  let link_close p ~after =
    Lwt.on_termination after.closed
      (fun _ -> close_async p)

  (* close a when every member of after closes *)
  let link_close_l p ~after =
    let n = ref (List.length after) in
    List.iter
      (fun p' -> Lwt.on_termination p'.closed
        (fun _ ->
           decr n;
           if !n = 0 then close_async p
        )
      ) after
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
      | `Error msg -> write_error a msg >>= fun _ -> Pipe.close a
      | `End -> Lwt.return_unit
    in
    Pipe.keep b (fwd());
    (* when a gets closed, close b too *)
    Pipe.link_close b ~after:a;
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
    List.iter (fun out -> Pipe.link_close res ~after:out) l;
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
      | (`Error _) as e -> Pipe.write b e >>= fun _ -> Pipe.close b
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
      | (`Error _) as e -> Pipe.write b e >>= fun _ -> Pipe.close b
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
    Pipe.link_close_l res ~after:l;
    res

  let merge_both a b = merge_all [a; b]

  let append a b =
    let c = Pipe.create () in
    Pipe.connect a c;
    Lwt.on_success (Pipe.wait a)
      (fun () ->
         Pipe.connect b c;
         Pipe.link_close c ~after:b (* once a and b finished, c is too *)
      );
    c
end

let connect ?(ownership=`None) a b =
  Pipe.connect a b;
  match ownership with
  | `None -> ()
  | `InOwnsOut -> Pipe.link_close b ~after:a
  | `OutOwnsIn -> Pipe.link_close a ~after:b

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

let to_list_rev r =
  Reader.fold ~f:(fun acc x -> x :: acc) ~x:[] r

let to_list r = to_list_rev r >>|= List.rev

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
      Lwt_io.flush oc >>= fun () ->
      Pipe.close p
    );
    p

  let write_lines oc =
    let p = Pipe.create () in
    Pipe.keep p (
      Reader.iter_s ~f:(Lwt_io.write_line oc) p >>= fun _ ->
      Lwt_io.flush oc >>= fun () ->
      Pipe.close p
    );
    p
end
