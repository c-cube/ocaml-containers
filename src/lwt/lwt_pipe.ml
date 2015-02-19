
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
let (>|=) = Lwt.(>|=)

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

let step_map f = function
  | `Ok x -> `Ok (f x)
  | (`Error _ | `End) as e -> e

let (>>|=) = LwtErr.(>|=)

let ret_end = Lwt.return `End

module Pipe = struct
  type -'a writer = 'a step -> unit Lwt.t

  type +'a reader = unit -> 'a step Lwt.t

  (* messages given to writers through the condition *)
  type 'a msg =
    | Send of 'a step Lwt.u (* send directly to reader *)
    | SendQueue (* push into queue *)

  type 'a t = {
    lock : Lwt_mutex.t;
    queue : 'a step Queue.t;
    max_size : int;
    cond : 'a msg Lwt_condition.t;
    mutable keep : unit Lwt.t list;  (* do not GC *)
  }

  let create ?(max_size=0) () = {
    queue=Queue.create();
    max_size;
    lock=Lwt_mutex.create();
    cond=Lwt_condition.create();
    keep=[];
  }

  let keep p fut = p.keep <- fut :: p.keep

  (* read next one *)
  let reader t () =
    Lwt_mutex.with_lock t.lock
      (fun () ->
         if Queue.is_empty t.queue
         then (
           let fut, send = Lwt.wait () in
           Lwt_condition.signal t.cond (Send send);
           fut
         ) else (
           (* direct pop *)
           assert (t.max_size > 0);
           let x = Queue.pop t.queue in
           Lwt_condition.signal t.cond SendQueue;  (* queue isn't full anymore *)
           Lwt.return x
         )
      )

  (* write a value *)
  let writer t x =
    let rec try_write () =
      if Queue.length t.queue < t.max_size then (
        Queue.push x t.queue;
        Lwt.return_unit
      ) else (
        (* wait for readers to consume the queue *)
        Lwt_condition.wait ~mutex:t.lock t.cond >>= fun msg ->
        match msg with
          | Send s ->
            Lwt.wakeup s x;
            Lwt.return_unit
          | SendQueue -> try_write () (* try again! *)
      )
    in
    Lwt_mutex.with_lock t.lock try_write

  let create_pair ?max_size () =
    let p = create ?max_size () in
    reader p, writer p

  let rec connect_ (r:'a reader) (w:'a writer) =
    r () >>= function
    | `End -> w `End (* then stop *)
    | (`Error _ | `Ok _) as step -> w step >>= fun () -> connect_ r w

  let pipe_into p1 p2 =
    connect_ (reader p1) (writer p2)
end

let connect r w = Pipe.connect_ r w

module Writer = struct
  type -'a t = 'a Pipe.writer

  let write t x = t (`Ok x)

  let write_error t msg = t (`Error msg)

  let write_end t = t `End

  let rec write_list t l = match l with
    | [] -> Lwt.return_unit
    | x :: tail ->
      write t x >>= fun () -> write_list t tail

  let map ~f t x = t (step_map f x)
end

module Reader = struct
  type +'a t = 'a Pipe.reader

  let read t = t ()

  let map ~f t () =
    t () >|= (step_map f)

  let rec filter_map ~f t () =
    t () >>= function
    | `Error msg -> LwtErr.fail msg
    | `Ok x ->
      begin match f x with
        | Some y -> LwtErr.return y
        | None -> filter_map ~f t ()
      end
    | `End -> ret_end

  let rec fold ~f ~x t =
    t () >>= function
    | `End -> LwtErr.return x
    | `Error msg -> LwtErr.fail msg
    | `Ok y -> fold ~f ~x:(f x y) t

  let rec fold_s ~f ~x t =
    t () >>= function
    | `End -> LwtErr.return x
    | `Error msg -> LwtErr.fail msg
    | `Ok y ->
      f x y >>= fun x -> fold_s ~f ~x t

  let rec iter ~f t =
    t () >>= function
    | `End -> LwtErr.return_unit
    | `Error msg -> LwtErr.fail msg
    | `Ok x -> f x; iter ~f t

  let rec iter_s ~f t =
    t () >>= function
    | `End -> LwtErr.return_unit
    | `Error msg -> LwtErr.fail msg
    | `Ok x -> f x >>= fun () -> iter_s ~f t

  let merge a b : _ t =
    let r, w = Pipe.create_pair () in
    Lwt.async (fun () -> Lwt.join [connect a w; connect b w]);
    r
end

(** {2 Conversions} *)

let of_list l : _ Reader.t =
  let l = ref l in
  fun () -> match !l with
    | [] -> ret_end
    | x :: tail ->
      l := tail;
      Lwt.return (`Ok x)

let of_array a =
  let i = ref 0 in
  fun () ->
    if !i = Array.length a
    then ret_end
    else (
      let x = a.(!i) in
      incr i;
      Lwt.return (`Ok x)
    )

let of_string s =
  let i = ref 0 in
  fun () ->
    if !i = String.length s
    then ret_end
    else (
      let x = String.get s !i in
      incr i;
      Lwt.return (`Ok x)
    )

let to_rev_list w =
  Reader.fold ~f:(fun acc x -> x :: acc) ~x:[] w

let to_list w = to_rev_list w >>|= List.rev

let to_list_exn w =
  to_list w >>= function
  | `Error msg -> Lwt.fail (Failure msg)
  | `Ok x -> Lwt.return x

let to_buffer buf : _ Writer.t = function
  | `Ok c ->
    Buffer.add_char buf c;
    Lwt.return_unit
  | `Error _ | `End -> Lwt.return_unit

let to_buffer_str buf = function
  | `Ok s ->
    Buffer.add_string buf s;
    Lwt.return_unit
  | `Error _ | `End -> Lwt.return_unit

(** {2 Basic IO wrappers} *)

module IO = struct
  let read ?(bufsize=4096) ic : _ Reader.t =
    let buf = Bytes.make bufsize ' ' in
    fun () ->
      Lwt_io.read_into ic buf 0 bufsize >>= fun n ->
      if n = 0 then ret_end
      else
        Lwt.return (`Ok (Bytes.sub_string buf 0 n))

  let read_lines ic () =
    Lwt_io.read_line_opt ic >>= function
    | None -> ret_end
    | Some line -> Lwt.return (`Ok line)

  let write oc = function
    | `Ok s -> Lwt_io.write oc s
    | `End | `Error _ -> Lwt.return_unit

  let write_lines oc = function
    | `Ok l -> Lwt_io.write_line oc l
    | `End | `Error _ -> Lwt.return_unit
end
