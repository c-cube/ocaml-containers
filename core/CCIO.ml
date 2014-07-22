
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

(** {1 IO Monad} *)

type _ t =
  | Return : 'a -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Bind : ('a -> 'b t) * 'a t -> 'b t
  | Star : ('a -> 'b) t * 'a t -> 'b t
  | Repeat : int * 'a t -> 'a list t
  | RepeatIgnore : int * 'a t -> unit t
  | Wrap : (unit -> 'a) -> 'a t
  | WrapJoin : (unit -> 'a t) -> 'a t

type 'a io = 'a t
type 'a or_error = [ `Ok of 'a | `Error of string ]

let (>>=) x f = Bind(f,x)

let map f x = Map(f, x)

let (>|=) x f = Map(f, x)

let return x = Return x
let pure = return

let (<*>) f a = Star (f, a)

let lift = map

let lift2 f a b =
  a >>= fun x -> map (f x) b

let lift3 f a b c =
  a >>= fun x ->
  b >>= fun y -> map (f x y) c

let repeat i a =
  if i <= 0 then Return [] else Repeat (i,a)

let repeat' i a =
  if i <= 0 then Return () else RepeatIgnore (i,a)

let rec _run : type a. a t -> a = function
  | Return x -> x
  | Map (f, a) -> f (_run a)
  | Bind (f, a) -> _run (f (_run a))
  | Star (f, a) -> _run f (_run a)
  | Repeat (i,a) -> _repeat [] i a
  | RepeatIgnore (i,a) -> _repeat_ignore i a
  | Wrap f -> f()
  | WrapJoin f -> _run (f())

and _repeat : type a. a list -> int -> a t -> a list
  = fun acc i a -> match i with
  | 0 -> List.rev acc
  | _ ->
      let x = _run a in
      _repeat (x::acc) (i-1) a

and _repeat_ignore : type a. int -> a t -> unit
  = fun i a -> match i with
  | 0 -> ()
  | _ ->
      let _ = _run a in
      _repeat_ignore (i-1) a

let _printers = ref []

exception PrinterResult of string

let _print_exn e =
  try
    List.iter
      (fun p -> match p e with
        | None -> ()
        | Some msg -> raise (PrinterResult msg)
      ) !_printers;
    Printexc.to_string e
  with PrinterResult s -> s

let run x =
  try `Ok (_run x)
  with e -> `Error (_print_exn e)

let register_printer p = _printers := p :: !_printers

(** {2 Standard Wrappers} *)

let _with_in flags filename f () =
  let ic = open_in_gen flags 0x644 filename in
  try
    f ic
  with e ->
    close_in ic;
    raise e

let with_in ?(flags=[]) filename f =
  WrapJoin (_with_in flags filename f)

let _read ic s i len () = input ic s i len
let read ic s i len = Wrap (_read ic s i len)

let _read_line ic () = Pervasives.input_line ic
let read_line ic = Wrap(_read_line ic)

let _with_out flags filename f () =
  let oc = open_out_gen flags 0x644 filename in
  try
    f oc
  with e ->
    close_out oc;
    raise e

let with_out ?(flags=[]) filename f =
  WrapJoin (_with_out flags filename f)

let _write oc s i len () = output oc s i len
let write oc s i len = Wrap (_write oc s i len)

let _write_str oc s () = output oc s 0 (String.length s)
let write_str oc s = Wrap (_write_str oc s)

let _write_buf oc buf () = Buffer.output_buffer oc buf
let write_buf oc buf = Wrap (_write_buf oc buf)

let flush oc = Wrap (fun () -> Pervasives.flush oc)

(** {2 Seq} *)

module Seq = struct
  type 'a step_result =
    | Yield of 'a
    | Stop

  type 'a gen = unit -> 'a step_result io

  type 'a t = 'a gen

  let _stop () = return Stop
  let _yield x = return (Yield x)

  let map_pure f gen () =
    gen() >>= function
    | Stop -> _stop ()
    | Yield x -> _yield (f x)

  let map f g () =
    g() >>= function
    | Stop -> _stop ()
    | Yield x -> f x >>= _yield

  let rec filter_map f g () =
    g() >>= function
    | Stop -> _stop()
    | Yield x ->
        match f x with
        | None -> filter_map f g()
        | Some y -> _yield y

  let rec flat_map f g () =
    g() >>= function
    | Stop -> _stop ()
    | Yield x ->
        f x >>= fun g' -> _flat_map_aux f g g' ()
  and _flat_map_aux f g g' () =
    g'() >>= function
    | Stop -> flat_map f g ()
    | Yield x -> _yield x

  let general_iter f acc g =
    let acc = ref acc in
    let rec _next () =
      g() >>= function
      | Stop -> _stop()
      | Yield x ->
          f !acc x >>= function
          | `Stop -> _stop()
          | `Continue (acc', ret) ->
              acc := acc';
              match ret with
              | None -> _next()
              | Some y -> _yield y
    in
    _next

  (** {6 Consume} *)

  let rec fold_pure f acc g =
    g() >>= function
    | Stop -> return acc
    | Yield x -> fold_pure f (f acc x) g

  let length g = fold_pure (fun acc _ -> acc+1) 0 g

  let rec fold f acc g =
    g() >>= function
    | Stop -> return acc
    | Yield x ->
        f acc x >>= fun acc' -> fold f acc' g

  let rec iter f g =
    g() >>= function
    | Stop -> return ()
    | Yield x -> f x >>= fun _ -> iter f g

  let of_fun g = g

  let lines ic () =
    try _yield (input_line ic)
    with End_of_file -> _stop()

  let output ?(sep="\n") oc seq =
    let first = ref true in
    iter
      (fun s ->
        ( if !first
          then (first:=false; return ())
          else write_str oc sep
        ) >>= fun () ->
        write_str oc s
      ) seq
end

(** {2 Raw} *)

module Raw = struct
  let wrap f = Wrap f
end
