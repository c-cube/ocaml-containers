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
  | Fail : string -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Bind : ('a -> 'b t) * 'a t -> 'b t
  | WithGuard: unit t * 'a t -> 'a t  (* run guard in any case *)
  | Star : ('a -> 'b) t * 'a t -> 'b t
  | Repeat : int * 'a t -> 'a list t
  | RepeatIgnore : int * 'a t -> unit t
  | Wrap : (unit -> 'a) -> 'a t
  | SequenceMap : ('a -> 'b t) * 'a list -> 'b list t

type 'a io = 'a t
type 'a with_finalizer = ('a t * unit t) t
type 'a or_error = [ `Ok of 'a | `Error of string ]

let (>>=) x f = Bind(f,x)

let bind ?finalize f a = match finalize with
  | None -> Bind(f,a)
  | Some b -> WithGuard (b, Bind (f,a))

let map f x = Map(f, x)

let (>|=) x f = Map(f, x)

let return x = Return x
let pure = return

let fail msg = Fail msg

let (<*>) f a = Star (f, a)

let lift = map

let lift2 f a b =
  a >>= fun x -> map (f x) b

let lift3 f a b c =
  a >>= fun x ->
  b >>= fun y -> map (f x y) c

let sequence_map f l =
  SequenceMap (f,l)

let sequence l =
  let _id x = x in
  SequenceMap(_id, l)

let repeat i a =
  if i <= 0 then Return [] else Repeat (i,a)

let repeat' i a =
  if i <= 0 then Return () else RepeatIgnore (i,a)

(** {2 Finalizers} *)

let (>>>=) a f =
  a >>= function
  | x, finalizer -> WithGuard (finalizer, x >>= f)

(** {2 Running} *)

exception IOFailure of string

let rec _run : type a. a t -> a = function
  | Return x -> x
  | Fail msg -> raise (IOFailure msg)
  | Map (f, a) -> f (_run a)
  | Bind (f, a) -> _run (f (_run a))
  | WithGuard (g, a) ->
      begin try
        let res = _run a in
        _run g;
        res
      with e ->
        _run g;
        raise e
      end
  | Star (f, a) -> _run f (_run a)
  | Repeat (i,a) -> _repeat [] i a
  | RepeatIgnore (i,a) -> _repeat_ignore i a
  | Wrap f -> f()
  | SequenceMap (f, l) -> _sequence_map f l []
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
and _sequence_map : type a b. (a -> b t) -> a list -> b list -> b list
  = fun f l acc -> match l with
  | [] -> List.rev acc
  | a::tail ->
      let x = _run (f a) in
      _sequence_map f tail (x::acc)

let _printers =
  ref [
    (* default printer *)
    ( function IOFailure msg
    | Sys_error msg -> Some msg
    | Exit -> Some "exit"
    | _ -> None
    )
  ]

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

exception IO_error of string

let run_exn x =
  try _run x
  with e -> raise (IO_error (_print_exn e))

let register_printer p = _printers := p :: !_printers

(** {2 Standard Wrappers} *)

let _open_in mode flags filename () =
  open_in_gen flags mode filename
let _close_in ic () = close_in ic

let with_in ?(mode=0o644) ?(flags=[]) filename =
  Wrap (_open_in mode flags filename)
  >>= fun ic ->
  Return (Return ic, Wrap (_close_in ic))

let _read ic s i len () = input ic s i len
let read ic s i len = Wrap (_read ic s i len)

let _read_line ic () =
  try Some (Pervasives.input_line ic)
  with End_of_file -> None
let read_line ic = Wrap(_read_line ic)

let rec _read_lines ic acc =
  read_line ic
  >>= function
  | None -> return (List.rev acc)
  | Some l -> _read_lines ic (l::acc)

let read_lines ic = _read_lines ic []

let read_all ic = Wrap(fun () -> CCIO.read_all ic)

let _open_out mode flags filename () =
  open_out_gen flags mode filename
let _close_out oc () = close_out oc

let with_out ?(mode=0o644) ?(flags=[]) filename =
  Wrap(_open_out mode (Open_wronly::flags) filename)
  >>= fun oc ->
  Return(Return oc, Wrap(_close_out oc))

let with_out_a ?mode ?(flags=[]) filename =
  with_out ?mode ~flags:(Open_creat::Open_append::flags) filename

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2

let output_str_ = Pervasives.output_substring

#else

let output_str_ = Pervasives.output

#endif

let _write oc s i len () = output_str_ oc s i len
let write oc s i len = Wrap (_write oc s i len)

let _write_str oc s () = output_str_ oc s 0 (String.length s)
let write_str oc s = Wrap (_write_str oc s)

let _write_line oc l () =
  output_string oc l;
  output_char oc '\n'
let write_line oc l = Wrap (_write_line oc l)

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

  let rec filter f g () =
    g() >>= function
    | Stop -> _stop()
    | Yield x ->
        if f x then _yield x else filter f g()

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

  let take n seq =
    general_iter
      (fun n x -> if n<=0
        then return `Stop
        else return (`Continue (n-1, Some x))
      ) n seq

  let drop n seq =
    general_iter
      (fun n x -> if n<=0
        then return (`Continue (n, Some x))
        else return (`Continue (n-1, None))
      ) n seq

  let take_while p seq =
    general_iter
      (fun () x ->
        p x >|= function
          | true -> `Continue ((), Some x)
          | false -> `Stop
      ) () seq

  let drop_while p seq =
    general_iter
      (fun dropping x ->
        if dropping
        then p x >|= function
          | true -> `Continue (true, None)
          | false -> `Continue (false, Some x)
        else return (`Continue (false, Some x))
      ) true seq

  (* apply all actions from [l] to [x] *)
  let rec _apply_all_to x l = match l with
    | [] -> return ()
    | f::tail -> f x >>= fun () -> _apply_all_to x tail

  let _tee funs g () =
    g() >>= function
    | Stop -> _stop()
    | Yield x ->
        _apply_all_to x funs >>= fun () ->
        _yield x

  let tee funs g = match funs with
    | [] -> g
    | _::_ -> _tee funs g

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

  let empty () = _stop()

  let singleton x =
    let first = ref true in
    fun () ->
      if !first then (first := false; _yield x) else _stop()

  let cons x g =
    let first = ref true in
    fun () ->
      if !first then (first := false; _yield x) else g()

  let of_list l =
    let l = ref l in
    fun () -> match !l with
      | [] -> _stop()
      | x::tail -> l:= tail; _yield x

  let of_array a =
    let i = ref 0 in
    fun () ->
      if !i = Array.length a
      then _stop()
      else (
        let x = a.(!i) in
        incr i;
        _yield x
      )

  (* TODO: wrapper around with_in? using bind ~finalize:... ? *)

  let chunks ~size ic =
    let buf = Buffer.create size in
    let eof = ref false in
    let next() =
      if !eof then _stop()
      else try
        Buffer.add_channel buf ic size;
        let s = Buffer.contents buf in
        Buffer.clear buf;
        _yield s
      with End_of_file ->
        let s = Buffer.contents buf in
        eof := true;
        if s="" then _stop() else _yield s
    in
    next

  let lines ic () =
    try _yield (input_line ic)
    with End_of_file -> _stop()

  let words _g =
    failwith "words: not implemented yet"
    (* TODO: state machine that goes:
        - 0: read input chunk
        - switch to "search for ' '", and yield word
        - goto 0 if no ' ' found
        - yield leftover when g returns Stop
    let buf = Buffer.create 32 in
    let next() =
      g() >>= function
      | Stop -> _stop
      | Yield s ->
          Buffer.add_string buf s;
          search_
    in
    next
    *)

  let output ?sep oc seq =
    let first = ref true in
    iter
      (fun s ->
        (* print separator *)
        ( if !first
          then (first:=false; return ())
          else match sep with
            | None -> return ()
            | Some sep -> write_str oc sep
        ) >>= fun () ->
        write_str oc s
      ) seq
    >>= fun () -> flush oc
end

(** {6 File and file names} *)

module File = struct
  type t = string

  let to_string f = f

  let make f =
    if Filename.is_relative f
      then Filename.concat (Sys.getcwd()) f
      else f

  let exists f = Wrap (fun () -> Sys.file_exists f)

  let is_directory f = Wrap (fun () -> Sys.is_directory f)

  let remove f = Wrap (fun () -> Sys.remove f)

  let _read_dir d () =
    if Sys.is_directory d
    then
      let arr = Sys.readdir d in
      Seq.map_pure make (Seq.of_array arr)
    else Seq.empty

  let rec _walk d () =
    if Sys.is_directory d
    then
      let arr = Sys.readdir d in
      let tail = Seq.of_array arr in
      let tail = Seq.flat_map
        (fun s -> return (_walk (Filename.concat d s) ()))
        tail
      in Seq.cons (`Dir,d) tail
    else Seq.singleton (`File, d)

  let walk t = Wrap (_walk t)

  let read_dir ?(recurse=false) d =
    if recurse
    then walk d
      >|= Seq.filter_map
      (function
        | `File, f -> Some f
        | `Dir, _ -> None
      )
    else Wrap (_read_dir d)

  let rec _read_dir_rec d () =
    if Sys.is_directory d
    then
      let arr = Sys.readdir d in
      let arr = Seq.of_array arr in
      let arr = Seq.map_pure (fun s -> Filename.concat d s) arr in
      Seq.flat_map
        (fun s ->
          if Sys.is_directory s
            then return (_read_dir_rec s ())
            else return (Seq.singleton s)
        ) arr
    else Seq.empty
end

(** {2 Raw} *)

module Raw = struct
  let wrap f = Wrap f
end
