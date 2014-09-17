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

let equal a b = a = b

let compare a b = Pervasives.compare a b

let hash a = Hashtbl.hash a

let _with_in filename f =
  let ic = open_in filename in
  try
    let x = f ic in
    close_in ic;
    x
  with e ->
    close_in ic;
    `Error (Printexc.to_string e)

let _with_out filename f =
  let oc = open_out filename in
  try
    let x = f oc in
    close_out oc;
    x
  with e ->
    close_out oc;
    raise e

(** {2 Serialization (encoding)} *)

(* shall we escape the string because of one of its chars? *)
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

let to_file_seq filename seq =
  _with_out filename
    (fun oc -> 
      seq (fun t -> to_chan oc t; output_char oc '\n')
    )

let to_file filename t = to_file_seq filename (fun k -> k t)

(** {2 Deserialization (decoding)} *)

type 'a parse_result = ['a or_error | `End ]
type 'a partial_result = [ 'a parse_result | `Await ]

module Source = struct
  type individual_char =
    | NC_yield of char
    | NC_end
    | NC_await

  type t = unit -> individual_char
  type source = t

  module Manual = struct
    type t = {
      mutable i : int;    (* offset *)
      mutable stop : bool;
      buf : Buffer.t;     (* accessible chunk of input *)
    }
      
    let make() = {
      i = 0;
      stop = false;
      buf=Buffer.create 32;
    }

    let to_src d () =
      if d.i = Buffer.length d.buf
        then
          if d.stop then NC_end else NC_await
        else (
          let c = Buffer.nth d.buf d.i in
          d.i <- d.i + 1;
          NC_yield c
        )

    let feed d s i len =
      if d.stop then failwith "Sexp.Streaming.Manual.feed: reached EOI";
      Buffer.add_substring d.buf s i len

    let reached_end d = d.stop <- true
  end

  let of_string s =
    let i = ref 0 in
    fun () ->
      if !i=String.length s
      then NC_end
      else (
        let c = String.get s !i in
        incr i;
        NC_yield c
      )

  let of_chan ?(bufsize=1024) ic =
    let buf = String.make bufsize ' ' in
    let i = ref 0 in
    let n = ref 0 in
    let stop = ref false in
    let rec next() =
      if !stop then NC_end
      else if !i = !n
      then ( (* refill *)
        i := 0;
        n := input ic buf 0 bufsize;
        if !n = 0 then (stop := true; NC_end) else next()
      ) else (  (* yield *)
        let c = String.get buf !i in
        incr i;
        NC_yield c
      )
    in next

  let of_gen g =
    let s = ref "" in
    let i = ref 0 in
    let stop = ref false in
    let rec next() =
      if !stop then NC_end
      else if !i = String.length !s
      then (
        match g() with
        | None -> stop := true; NC_end
        | Some buf -> s := buf; i := 0; next () 
      ) else (
        let c = String.get !s !i in
        incr i;
        NC_yield c
      )
    in next
end

module Lexer = struct
  (** An individual character returned by a source *)
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

  type t = {
    src : Source.t;
    atom : Buffer.t;    (* atom being parsed *)
    mutable st : decode_state;
    mutable line : int;
    mutable col : int;
  }

  let make src = {
    src;
    st = St_start;
    line = 1;
    col = 1;
    atom = Buffer.create 32;
  }

  let of_string s = make (Source.of_string s)

  let of_chan ic = make (Source.of_chan ic)

  let line t = t.line
  let col t = t.col

  (* yield [x] with current state [st] *)
  let _yield d st x =
    d.st <- st;
    `Ok x

  let _take_buffer b =
    let s = Buffer.contents b in
    Buffer.clear b;
    s

  (* raise an error *)
  let _error d msg =
    let b = Buffer.create 32 in
    Printf.bprintf b "at %d, %d: " d.line d.col;
    Printf.kbprintf
      (fun b ->
        let msg' = Buffer.contents b in
        d.st <- St_error msg';
        `Error msg')
      b msg

  let _end d =
    d.st <- St_end;
    `End

  let _is_digit c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'
  let _digit2i c = Char.code c - Char.code '0'

  (* next token *)
  let rec _next d st : token partial_result =
    match st with
    | St_error msg -> `Error msg
    | St_end -> _end d
    | St_yield x ->
        (* yield the given token, then start a fresh one *)
        _yield d St_start x
    | _ ->
        d.st <- st;
        _process_next d st

  (* read and proces the next character *)
  and _process_next d st =
    match d.src () with
    | Source.NC_end ->
        begin match st with
        | St_error _ | St_end | St_yield _ -> assert false
        | St_start -> _end d
        | St_atom ->
            let a = _take_buffer d.atom in
            _yield d St_end (Atom a)
        | St_quoted ->
            let a = _take_buffer d.atom in
            _yield d St_end (Atom a)
        | (St_escaped | St_raw_char1 _ | St_raw_char2 _) ->
            _error d "unexpected end of input (escaping)"
        end
    | Source.NC_await -> `Await
    | Source.NC_yield c ->
        if c='\n'
          then (d.col <- 1; d.line <- d.line + 1)
          else (d.col <- d.col + 1);
        (* use the next char *)
        match st with
        | St_error _ | St_end | St_yield _ -> assert false
        | St_start ->
            begin match c with
            | ' ' | '\t' | '\n' -> _next d St_start
            | '(' -> _yield d St_start Open
            | ')' -> _yield d St_start Close
            | '"' -> _next d St_quoted
            | _ -> (* read regular atom *)
                Buffer.add_char d.atom c;
                _next d St_atom
            end
        | St_atom ->
            begin match c with
            | ' ' | '\t' | '\n' ->
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
        | St_quoted ->
            (* reading an unquoted atom *)
            begin match c with
            | '\\' -> _next d St_escaped
            | '"' ->
                let a = _take_buffer d.atom in
                _yield d St_start (Atom a)
            | _ ->
                Buffer.add_char d.atom c;
                _next d St_quoted
            end
        | St_escaped ->
            begin match c with
              | 'n' -> Buffer.add_char d.atom '\n'; _next d St_quoted
              | 't' -> Buffer.add_char d.atom '\t'; _next d St_quoted
              | 'r' -> Buffer.add_char d.atom '\r'; _next d St_quoted
              | 'b' -> Buffer.add_char d.atom '\b'; _next d St_quoted
              | '"' -> Buffer.add_char d.atom '"'; _next d St_quoted
              | '\\' -> Buffer.add_char d.atom '\\'; _next d St_quoted
              | _ when _is_digit c -> _next d (St_raw_char1 (_digit2i c))
              | _ -> _error d "unexpected escaped character %c" c
            end
        | St_raw_char1 i ->
            begin match c with
              | _ when _is_digit c -> _next d (St_raw_char2 (i*10 + _digit2i c))
              | _ -> _error d "expected digit, got %c" c
            end
        | St_raw_char2 i ->
            begin match c with
              | c when _is_digit c ->
                  (* read an escaped char *)
                  Buffer.add_char d.atom (Char.chr (i*10+_digit2i c));
                  _next d St_quoted
              | c -> _error d "expected digit, got %c" c
            end

  let next d = _next d d.st
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
  ps_d : Lexer.t;
  mutable ps_stack : t list list;
}

let mk_ps src = {
  ps_d = Lexer.make src;
  ps_stack = [];
}

let _error ps msg =
  let msg' = Printf.sprintf "at %d,%d: %s" (Lexer.line ps.ps_d) (Lexer.col ps.ps_d) msg in
  `Error msg'

(* next token, or await *)
let rec _next ps : t partial_result =
  match Lexer.next ps.ps_d with
  | `Ok (Lexer.Atom s) ->
      _push ps (Atom s)
  | `Ok Lexer.Open ->
      ps.ps_stack <- [] :: ps.ps_stack;
      _next ps
  | `Ok Lexer.Close ->
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

(* assume [ps] never needs [`Await] *)
let _never_block ps () = match _next ps with
  | `Await -> assert false
  | `Ok x -> `Ok x
  | `Error e -> `Error e
  | `End -> `End

(* parse from a generator of string slices *)
let parse_gen g : t ParseGen.t =
  let ps = mk_ps (Source.of_gen g) in
  _never_block ps

let parse_string s =
  let ps = mk_ps (Source.of_string s) in
  _never_block ps

let parse_chan ?bufsize ic =
  let ps = mk_ps (Source.of_chan ?bufsize ic) in
  _never_block ps

(** {6 Blocking} *)

let of_chan ic =
  ParseGen.head (parse_chan ic)

let of_string s =
  ParseGen.head (parse_string s)

let of_file f =
  _with_in f of_chan

module L = struct
  let to_buf b l =
    List.iter (to_buf b) l

  let to_string l =
    let b = Buffer.create 32 in
    to_buf b l;
    Buffer.contents b

  let to_chan oc l =
    let fmt = Format.formatter_of_out_channel oc in
    List.iter (Format.fprintf fmt "%a@." print) l;
    Format.pp_print_flush fmt ()

  let to_file filename l =
    _with_out filename (fun oc -> to_chan oc l)

  let of_chan ?bufsize ic =
    ParseGen.to_list (parse_chan ?bufsize ic)

  let of_file ?bufsize filename =
    _with_in filename
      (fun ic -> of_chan ?bufsize ic)

  let of_string s =
    ParseGen.to_list (parse_string s)

  let of_gen g =
    ParseGen.to_list (parse_gen g)

  exception OhNoes of string
  exception StopNaow

  let of_seq seq =
    let src = Source.Manual.make () in
    let ps = mk_ps (Source.Manual.to_src src) in
    let l = ref [] in
    (* read as many expressions as possible *)
    let rec _nexts () = match _next ps with
      | `Ok x -> l := x :: !l; _nexts ()
      | `Error e -> raise (OhNoes e)
      | `End -> raise StopNaow
      | `Await -> ()
    in
    try
      seq
        (fun s -> Source.Manual.feed src s 0 (String.length s); _nexts ());
      Source.Manual.reached_end src;
      _nexts ();
      `Ok (List.rev !l)
    with
    | OhNoes msg -> `Error msg
    | StopNaow -> `Ok (List.rev !l)
end
