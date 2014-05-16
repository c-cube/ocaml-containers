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

type t =
  | K of string * t  (* keyword *)
  | I of int
  | S of string
  | L of t list

let eq a b = a = b

let compare a b = Pervasives.compare a b

let hash a = Hashtbl.hash a

(** {2 Serialization (encoding)} *)

let rec to_buf b t = match t with
  | I i -> Printf.bprintf b "%d" i
  | S s -> Buffer.add_string b (String.escaped s)
  | K (s, t') ->
    assert (s.[0] = ':');
    Buffer.add_string b s;
    Buffer.add_char b ' ';
    to_buf b t'
  | L l ->
    Buffer.add_char b '(';
    List.iteri (fun i t' -> (if i > 0 then Buffer.add_char b ' '; to_buf b t')) l;
    Buffer.add_char b ')'

let to_string t =
  let b = Buffer.create 32 in
  to_buf b t;
  Buffer.contents b

(* TODO: improve (slow and ugly) *)
let fmt fmt t =
  let b = Buffer.create 32 in
  to_buf b t;
  Format.pp_print_string fmt (Buffer.contents b)

(** {2 Deserialization (decoding)} *)

(** Deserialization is based on the {! decoder} type. Parsing can be
    incremental, in which case the input is provided chunk by chunk and
    the decoder contains the parsing state. Once a Sexpr value
    has been parsed, other values can still be read. *)

type decoder = {
  mutable buf : string;  (* input buffer *)
  mutable i : int;  (* index in buf *)
  mutable len : int;  (* length of substring to read *)
  mutable c : int;  (* line *)
  mutable l : int;  (* column *)
  mutable state : parse_result;
  mutable stack : partial_state list;
} (** Decoding state *)

(** Result of parsing *)
and parse_result =
  | ParseOk of t
  | ParseError of string
  | ParsePartial

(** Partial state of the parser *)
and partial_state =
  | PS_I of bool * int (* sign and integer *)
  | PS_S of Buffer.t (* parsing a string *)
  | PS_S_escape of Buffer.t (* parsing a string; prev char is \ *)
  | PS_L of t list
  | PS_key of string  (* key, waiting for value *)
  | PS_return of t  (* bottom of stack *)
  | PS_error of string (* error *)

let mk_decoder () =
  let dec = {
    buf = "";
    i = 0;
    len = 0;
    c = 0;
    l = 0;
    state = ParsePartial;
    stack = [];
  } in
  dec

let is_empty dec = dec.len = 0
let cur dec = dec.buf.[dec.i]

let junk dec =
  (* update line/column *)
  (if cur dec = '\n'
    then (dec.c <- 0; dec.l <- dec.l + 1)
    else dec.c <- dec.c + 1);
  dec.i <- dec.i + 1;
  dec.len <- dec.len - 1

let next dec =
  let c = cur dec in
  junk dec;
  c

(* parse value *)
let rec parse_rec dec =
  match dec.stack with
  | [PS_return v] ->  (* return value *)
    dec.stack <- [];
    dec.state <- ParseOk v;
    dec.state
  | [PS_error s] -> (* failure *)
    dec.stack <- [];
    dec.state <- ParseError s;
    dec.state
  | _ ->
    if is_empty dec then ParsePartial (* wait *)
    else begin
      let c = next dec in
      (match dec.stack, c with
      | PS_S_escape b :: stack, 'n' ->
        Buffer.add_char b '\n';
        dec.stack <- PS_S b :: stack
      | PS_S_escape b :: stack, 't' ->
        Buffer.add_char b '\t';
        dec.stack <- PS_S b :: stack
      | (PS_S_escape b) :: stack, ('(' | '\\' | ')' | ' ') ->
        Buffer.add_char b c;
        dec.stack <- (PS_S b) :: stack;
      | (PS_key s) :: _, (')' | '\n' | ' ' | '\t') -> (* error *)
        error dec ("keyword " ^ s ^ " expected value")
      | _, ')' -> (* special case for ')' *)
        close_paren dec
      | ((PS_L _ | PS_key _) :: _ | []), '-' ->  (* negative num *)
        dec.stack <- PS_I (false, 0) :: dec.stack
      | ((PS_L _ | PS_key _) :: _ | []), '0' .. '9' ->  (* positive num *)
        dec.stack <- PS_I (true, Char.code c - Char.code '0') :: dec.stack
      | (PS_I (sign, i)) :: stack, '0' .. '9' ->
        dec.stack <- PS_I (sign, (Char.code c - Char.code '0') + 10 * i) :: stack;
      | (PS_I (sign, i)) :: stack, (' ' | '\t' | '\n') ->
        terminate_token dec
      | stack, '(' ->
        dec.stack <- PS_L [] :: stack  (* push new list *)
      | PS_S b :: stack, (' ' | '\t' | '\n') -> (* parsed a string *)
        terminate_token dec
      | PS_S b :: stack, '\\' ->
        dec.stack <- PS_S_escape b :: stack  (* escape next char *)
      | PS_S b :: _, _ ->
        Buffer.add_char b c  (* just a char of the string *)
      | _, (' ' | '\t' | '\n') ->  (* skip *)
        ()
      | stack, c ->
        let b = Buffer.create 7 in
        Buffer.add_char b c;
        dec.stack <- PS_S b :: stack
      );
      parse_rec dec
    end
(* When a value is parsed, push it on the stack (possibly collapsing it) *)
and push_value dec v =
  match v, dec.stack with
  | _, [] ->
    dec.stack <- [PS_return v] (* finished *)
  | _, (PS_L l) :: stack ->
    (* add to list *)
    dec.stack <- (PS_L (v :: l)) :: stack;
  | v, ((PS_key s) :: stack) ->
    (* parsed a key/value *)
    dec.stack <- stack;
    push_value dec (K (s, v))
  | _ ->
    error dec "unexpected value"
(* closing parenthesis: may terminate several states at once *)
and close_paren dec =
  match dec.stack with
  | PS_L l :: stack ->
    dec.stack <- stack;
    push_value dec (L (List.rev l))
  | (PS_I _ | PS_S _) :: stack ->
    terminate_token dec;
    close_paren dec  (* parenthesis still not closed *)
  | _ ->
    error dec "Sexp: unexpected ')'"
(* terminate current token *)
and terminate_token dec =
  match dec.stack with
  | [] -> assert false
  | (PS_I (sign, i)) :: stack ->
    dec.stack <- stack;
    push_value dec (I (if sign then i else ~- i))  (* parsed int *)
  | (PS_S b) :: stack ->
    dec.stack <- stack;
    let s = Buffer.contents b in
    if s.[0] = ':'
      then dec.stack <- (PS_key s) :: stack  (* keyword, wait for value *)
      else push_value dec (S s)
  | _ ->
    error dec "Sexp: ill-terminated token"
(* signal error *)
and error dec msg =
  let msg = Printf.sprintf "Sexp: error at line %d, column %d: %s"
    dec.l dec.c msg in
  dec.stack <- [PS_error msg]

(* exported parse function *)
let parse dec s i len =
  (if i < 0 || i+len > String.length s
    then invalid_arg "Sexp.parse: not a valid substring");
  (* add the input to [dec] *)
  if dec.len = 0
    then begin
      dec.buf <- s;
      dec.i <- i;
      dec.len <- len;
    end else begin
      (* use a buffer to merge the stored input and the new input *)
      let b = Buffer.create (dec.len + len) in
      Buffer.add_substring b dec.buf dec.i dec.len;
      Buffer.add_substring b s i len;
      dec.buf <- Buffer.contents b;
      dec.i <- 0;
      dec.len <- dec.len + len;
    end;
  (* state machine *)
  parse_rec dec

let reset dec =
  dec.l <- 0;
  dec.c <- 0;
  dec.i <- 0;
  dec.len <- 0;
  dec.state <- ParsePartial;
  dec.stack <- [];
  ()

let state dec = dec.state

let rest dec =
  String.sub dec.buf dec.i dec.len

let rest_size dec =
  dec.len

let parse_string s =
  let dec = mk_decoder () in
  parse dec s 0 (String.length s)

let of_string s =
  match parse_string s with
  | ParseOk t -> t
  | ParsePartial -> invalid_arg "Sexp: partial parse"
  | ParseError msg -> invalid_arg msg

(* tests:

let s = Sexp.of_string "(0 a b c 42 :foo 45 :bar (hello-world foo\\tb\\na\\(\\)r -421) (41 -52) 0)";;
Sexp.to_string s;;
*)
