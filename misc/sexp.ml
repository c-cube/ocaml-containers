(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Simple S-expression parsing/printing} *)

type 'a or_error = [ `Ok of 'a | `Error of string ]
type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

type t =
  | Atom of string
  | List of t list

let eq a b = a = b

let compare a b = Pervasives.compare a b

let hash a = Hashtbl.hash a

(** {2 Serialization (encoding)} *)

let _must_escape s =
  try
    for i = 0 to String.length s - 1 do
      let c = String.unsafe_get s i in
      match c with
      | ' ' | ')' | '(' | '"' | '\n' | '\t' -> raise Exit
      | _ when Char.code c > 127 -> raise Exit  (* non-ascii *)
      | _ -> ()
    done;
    false
  with Exit -> true

let rec to_buf b t = match t with
  | Atom s when _must_escape s -> Printf.bprintf b "\"%s\"" (String.escaped s)
  | Atom s -> Buffer.add_string b s
  | List [] -> Buffer.add_string b "()"
  | List [x] -> Printf.bprintf b "(%a)" to_buf x
  | List l ->
      Buffer.add_char b '(';
      List.iteri
        (fun i t' -> (if i > 0 then Buffer.add_char b ' '; to_buf b t'))
        l;
      Buffer.add_char b ')'

let to_string t =
  let b = Buffer.create 128 in
  to_buf b t;
  Buffer.contents b

let rec print fmt t = match t with
  | Atom s when _must_escape s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | Atom s -> Format.pp_print_string fmt s
  | List [] -> Format.pp_print_string fmt "()"
  | List [x] -> Format.fprintf fmt "@[<hov2>(%a)@]" print x
  | List l ->
      Format.open_hovbox 2;
      Format.pp_print_char fmt '(';
      List.iteri
        (fun i t' -> (if i > 0 then Format.fprintf fmt "@ "; print fmt t'))
        l;
      Format.pp_print_char fmt ')';
      Format.close_box ()

let rec print_noindent fmt t = match t with
  | Atom s when _must_escape s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | Atom s -> Format.pp_print_string fmt s
  | List [] -> Format.pp_print_string fmt "()"
  | List [x] -> Format.fprintf fmt "(%a)" print_noindent x
  | List l ->
      Format.pp_print_char fmt '(';
      List.iteri
        (fun i t' -> (if i > 0 then Format.pp_print_char fmt ' '; print_noindent fmt t'))
        l;
      Format.pp_print_char fmt ')'

let to_chan oc t =
  let fmt = Format.formatter_of_out_channel oc in
  print fmt t;
  Format.pp_print_flush fmt ()

let seq_to_file filename seq =
  let oc = open_out filename in
  try
    seq
      (fun t -> to_chan oc t; output_char oc '\n');
    close_out oc
  with e ->
    close_out oc;
    raise e

let to_file filename t = seq_to_file filename (fun k -> k t)

(** {2 Deserialization (decoding)} *)

type 'a parse_result = ['a or_error | `End ]
type 'a partial_result = [ 'a parse_result | `Await ]

module Streaming = struct
  type token =
    | Open
    | Close
    | Atom of string

  type decode_state =
    | St_start
    | St_atom
    | St_quoted
    | St_escaped
    | St_raw_char1 of int
    | St_raw_char2 of int
    | St_yield of token
    | St_error of string
    | St_end

  type decoder = {
    mutable st : decode_state;
    mutable i : int;
    mutable line : int;
    mutable col : int;
    mutable stop : bool;
    buf : Buffer.t;
    atom : Buffer.t;  (* atom being parsed *)
  }

  let mk_decoder () = {
    i = 0;
    st = St_start;
    line = 1;
    col = 1;
    stop = false;
    buf=Buffer.create 32;
    atom = Buffer.create 32;
  }

  exception NeedMoar
  exception Error of string
  exception EOI

  (* yield [x] with current state [st] *)
  let _yield d st x =
    d.st <- st;
    x

  (* read the next char *)
  let _next_char d =
    if d.i = Buffer.length d.buf
      then (
        (* need more input; reset buffer to put it in *)
        Buffer.clear d.buf;
        d.i <- 0;
        raise NeedMoar
      ) else (
        let c = Buffer.nth d.buf d.i in
        d.i <- d.i + 1;
        d.col <- d.col + 1;
        c
      )

  let _take_buffer b =
    let s = Buffer.contents b in
    Buffer.clear b;
    s

  let _newline d =
    d.line <- d.line + 1;
    d.col <- 0;
    ()

  (* raise an error *)
  let _error d msg =
    let b = Buffer.create 32 in
    Printf.bprintf b "at %d, %d: " d.line d.col;
    Printf.kbprintf
      (fun b ->
        let msg' = Buffer.contents b in
        d.st <- St_error msg';
        raise (Error msg'))
      b msg

  let _end d =
    d.st <- St_end;
    raise EOI

  let _is_digit c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'
  let _digit2i c = Char.code c - Char.code '0'

  (* next token *)
  let rec _next d st =
    d.st <- st;
    match st with
    | St_error msg -> raise (Error msg)
    | St_end -> _end d
    | St_yield x ->
        (* yield the given token, then start a fresh one *)
        _yield d St_start x
    | St_start when d.stop -> _end d
    | St_start ->
        (* start reading next token *)
        let c = _next_char d in
        begin match c with
        | '\n' -> _newline d; _next d St_start
        | ' ' | '\t' -> _next d St_start
        | '(' -> _yield d St_start Open
        | ')' -> _yield d St_start Close
        | '"' -> _next d St_quoted
        | _ -> (* read regular atom *)
            Buffer.add_char d.atom c;
            _next d St_atom
        end
    | St_atom when d.stop ->
        let a = _take_buffer d.atom in
        _yield d St_end (Atom a)
    | St_atom ->
        (* reading an unquoted atom *)
        let c = _next_char d in
        begin match c with
        | '\n' ->
            _newline d;
            let a = _take_buffer d.atom in
            _yield d St_start (Atom a)
        | ' ' | '\t' ->
            let a = _take_buffer d.atom in
            _yield d St_start (Atom a)
        | ')' ->
            let a = _take_buffer d.atom in
            _yield d (St_yield Close) (Atom a)
        | '(' ->
            let a = _take_buffer d.atom in
            _yield d (St_yield Open) (Atom a)
        | '"' -> _error d "unexpected \" (parsing atom %s)" (Buffer.contents d.atom)
        | '\\' -> _error d "unexpected \\"
        | _ ->
            Buffer.add_char d.atom c;
            _next d St_atom
        end
    | St_quoted when d.stop ->
        let a = _take_buffer d.atom in
        _yield d St_end (Atom a)
    | St_quoted ->
        (* reading an unquoted atom *)
        let c = _next_char d in
        begin match c with
        | '\\' -> _next d St_escaped
        | '"' ->
            let a = _take_buffer d.atom in
            _yield d St_start (Atom a)
        | _ ->
            Buffer.add_char d.atom c;
            _next d St_quoted
        end
    | (St_escaped | St_raw_char1 _ | St_raw_char2 _) when d.stop ->
        _error d "unexpected end of input (escaping)"
    | St_escaped ->
        begin match _next_char d with
          | 'n' -> Buffer.add_char d.atom '\n'; _next d St_quoted
          | 't' -> Buffer.add_char d.atom '\t'; _next d St_quoted
          | 'r' -> Buffer.add_char d.atom '\r'; _next d St_quoted
          | 'b' -> Buffer.add_char d.atom '\b'; _next d St_quoted
          | '"' -> Buffer.add_char d.atom '"'; _next d St_quoted
          | '\\' -> Buffer.add_char d.atom '\\'; _next d St_quoted
          | c when _is_digit c -> _next d (St_raw_char1 (_digit2i c))
          | c -> _error d "unexpected escaped character %c" c
        end
    | St_raw_char1 i ->
        begin match _next_char d with
          | c when _is_digit c -> _next d (St_raw_char2 (i*10 + _digit2i c))
          | c -> _error d "expected digit, got %c" c
        end
    | St_raw_char2 i ->
        begin match _next_char d with
          | c when _is_digit c ->
              (* read an escaped char *)
              Buffer.add_char d.atom (Char.chr (i*10+_digit2i c));
              _next d St_quoted
          | c -> _error d "expected digit, got %c" c
        end

  let feed d s i len =
    if d.stop then failwith "Sexp.Streaming.feed: end of input reached";
    Buffer.add_substring d.buf s i len

  let reached_end d =
    d.stop <- true

  let next d =
    try
      `Ok (_next d d.st)
    with
    | NeedMoar -> `Await
    | Error msg -> `Error msg
    | EOI -> `End
end

module ParseGen = struct
  type 'a t = unit -> 'a parse_result

  let to_list g : 'a list or_error =
    let rec aux acc = match g() with
      | `Error e -> `Error e
      | `Ok x -> aux (x::acc)
      | `End -> `Ok (List.rev acc)
    in
    aux []

  let head g = match g() with
    | `End -> `Error "expected at least one element"
    | #or_error as x -> x

  let head_exn g = match g() with
    | `Ok x -> x
    | `Error msg -> failwith msg
    | `End -> failwith "expected at least one element"

  let take n g =
    assert (n>=0);
    let n = ref n in
    fun () ->
      if !n = 0 then `End
      else (
        decr n;
        g()
      )
end

(* hidden parser state *)
type parser_state = {
  ps_d : Streaming.decoder;
  mutable ps_stack : t list list;
}

let mk_ps () = {
  ps_d = Streaming.mk_decoder ();
  ps_stack = [];
}

let _error ps msg =
  let msg' = Printf.sprintf "at %d,%d: %s"
    ps.ps_d.Streaming.line ps.ps_d.Streaming.col msg in
  `Error msg'

(* next token, or await *)
let rec _next ps : t partial_result = match Streaming.next ps.ps_d with
  | `Ok (Streaming.Atom s) ->
      _push ps (Atom s)
  | `Ok Streaming.Open ->
      ps.ps_stack <- [] :: ps.ps_stack;
      _next ps
  | `Ok Streaming.Close ->
      begin match ps.ps_stack with
      | [] -> _error ps "unbalanced ')'"
      | l :: stack ->
          ps.ps_stack <- stack;
          _push ps (List (List.rev l))
      end
  | `Error msg -> `Error msg
  | `Await -> `Await
  | `End -> `End

(* push a S-expr on top of the parser stack *)
and _push ps e = match ps.ps_stack with
  | [] ->
      `Ok e
  | l :: tl ->
      ps.ps_stack <- (e :: l) :: tl;
      _next ps

(* parse from a generator of string slices *)
let _parse_gen g : t ParseGen.t =
  let ps = mk_ps() in
  let rec next () = match _next ps with
    | `Await ->
        begin match g() with
        | None -> Streaming.reached_end ps.ps_d
        | Some (s,i,len) -> Streaming.feed ps.ps_d s i len
        end;
        next()
    | `Ok x -> `Ok x
    | `Error e -> `Error e
    | `End -> `End
  in
  next

let parse_gen g =
  _parse_gen
    (fun () ->
      match g() with
      | None -> None
      | Some s -> Some (s,0,String.length s)
    )

(* singleton generator *)
let _gen1 x =
  let first = ref true in
  fun () ->
    if !first then (first:=false; Some x) else None

let parse_string s =
  parse_gen (_gen1 s)

let parse_chan ?(bufsize=1024) ic =
  let buf = String.make bufsize ' ' in
  let stop = ref false in
  let gen () =
    if !stop then None
    else
      let n = input ic buf 0 bufsize in
      if n=0 then (stop:=true; None) else Some (buf,0,n)
  in
  _parse_gen gen

(** {6 Blocking} *)

let parse1_chan ic =
  ParseGen.head (parse_chan ic)

let parse1_string s =
  ParseGen.head (parse_string s)

let parse_l_chan ?bufsize ic =
  ParseGen.to_list (parse_chan ?bufsize ic)

let parse_l_file ?bufsize filename =
  let ic = open_in filename in
  try
    let l = parse_l_chan ?bufsize ic in
    close_in ic;
    l
  with e ->
    close_in ic;
    `Error (Printexc.to_string e)

let parse_l_string s =
  ParseGen.to_list (parse_string s)

let parse_l_gen g =
  ParseGen.to_list (parse_gen g)

let parse_l_seq seq =
  let ps = mk_ps() in
  let l = ref [] in
  (* read as many expressions as possible *)
  let rec _nexts () = match _next ps with
    | `Ok x -> l := x :: !l; _nexts ()
    | `Error e -> raise (Streaming.Error e)
    | `End -> raise Streaming.EOI
    | `Await -> ()
  in
  try
    seq
      (fun s -> Streaming.feed ps.ps_d s 0 (String.length s); _nexts ());
    Streaming.reached_end ps.ps_d;
    _nexts ();
    `Ok (List.rev !l)
  with
  | Streaming.Error msg -> `Error msg
  | Streaming.EOI -> `Ok (List.rev !l)
