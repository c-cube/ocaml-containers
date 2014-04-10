
(*
copyright (c) 2014, simon cruanes
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

(** {1 Full-Streaming API of Bencode} *)

type token =
  | Int of int
  | String of string
  | BeginDict
  | EndDict
  | BeginList
  | EndList

module Encode = struct
  type sink =
    [ `File of string
    | `Out of out_channel
    | `Buf of Buffer.t
    ]

  type t = {
    write_string : string -> unit;
    write_char : char -> unit;
    on_close : unit -> unit;
  }
  
  let nop() = ()

  let create = function
    | `Out o ->
        { write_string=output_string o
        ; write_char=output_char o
        ; on_close = nop
        }
    | `File f ->
        let o = open_in f in
        { write_string=output_string o
        ; write_char=output_char o
        ; on_close = (fun () -> close_out o)
        }
    | `Buf b ->
        { write_string=Buffer.add_string b
        ; write_char=Buffer.add_char b
        ; on_close =nop
        }

    let push out tok = match tok with
      | Int i ->
          out.write_char 'i';
          out.write_string (string_of_int i);
          out.write_char 'e'
      | String s ->
          out.write_string (string_of_int (String.length s));
          out.write_char ':';
          out.write_string s
      | BeginDict ->
          out.write_char 'd'
      | EndDict ->
          out.write_char 'e'
      | BeginList ->
          out.write_char 'l'
      | EndList ->
          out.write_char 'e'
end

module Decode = struct
  type source =
    [ `File of string
    | `In of in_channel
    | `String of string
    | `Manual
    ]

  type result =
    | Yield of token
    | Error of string
    | End
    | Await  (** The user needs to call {!feed} with some input *)

  type t = {
    read_string : string -> int -> int -> int;
    read_char : unit -> char;
    mutable buf : string;  (* buffer *)
    mutable i : int;  (* index in buf *)
    mutable len : int;  (* length of substring to read *)
    mutable c : int;  (* line *)
    mutable l : int;  (* column *)
    mutable state : result;
    mutable stack : partial_state list;
  }

  let __default = {
    read_string = (fun _ _ _ -> assert false);
    read_char = (fun _ -> '\000');
    buf = "";
    i = 0;
    len = 0;
    c = 0;
    l = 0;
    state = Error "no input";
    stack = [];
  }

  let create = function
    | `File f ->


  val create : source -> t
  (** Create a new decoder with the given source. *)

  type decoder = {
    mutable buf : string;  (* buffer *)
    mutable i : int;  (* index in buf *)
    mutable len : int;  (* length of substring to read *)
    mutable c : int;  (* line *)
    mutable l : int;  (* column *)
    mutable state : parse_result;
    mutable stack : partial_state list;
  }

  (** Result of parsing *)
  and parse_result =
    | ParseOk of t
    | ParseError of string
    | ParsePartial

  (** Partial state of the parser *)
  and partial_state =
    | PS_I of bool * int (* sign and integer *)
    | PS_S of int ref * string   (* index in string, plus string *)
    | PS_L of t list
    | PS_D of t SMap.t  (* in dictionary *)
    | PS_D_key of string * t SMap.t  (* parsed key, wait for value *) 
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
        | (PS_I (sign, i)) :: stack, '0' .. '9' ->
          dec.stack <- PS_I (sign, (Char.code c - Char.code '0') + 10 * i) :: stack;
        | (PS_I (_, 0)) :: stack, '-' ->
          dec.stack <- PS_I (false, 0) :: stack  (* negative number *)
        | (PS_I (sign, i)) :: stack, 'e' ->
          dec.stack <- stack;
          push_value dec (I (if sign then i else ~- i))
        | ((PS_D _ | PS_D_key _ | PS_L _) :: _ | []), '0' .. '9' ->
          (* initial length of string *)
          dec.stack <- (PS_I (true, Char.code c - Char.code '0')) :: dec.stack
        | (PS_I (sign, i)) :: stack, ':' ->
          if i < 0
            then error dec "string length cannot be negative"
          else if i = 0 then  (* empty string *)
            let _ = dec.stack <- stack in
            push_value dec (S "")
          else (* prepare to parse a string *)
            dec.stack <- (PS_S (ref 0, String.create i)) :: stack;
        | (PS_S (n, s)) :: stack, _ ->
          s.[!n] <- c;
          incr n;
          (* value completed *)
          (if !n = String.length s
            then
              let _ = dec.stack <- stack in
              push_value dec (S s));
        | stack, 'i' ->
          dec.stack <- (PS_I (true, 0)) :: stack
        | stack, 'l' ->
          dec.stack <- PS_L [] :: stack;
        | stack, 'd' ->
          dec.stack <- PS_D SMap.empty :: stack
        | (PS_L l) :: stack, 'e' -> (* end of list *)
          dec.stack <- stack;
          push_value dec (L (List.rev l))
        | (PS_D d) :: stack, 'e' -> (* end of dict *)
          dec.stack <- stack;
          push_value dec (D d)
        | (PS_D_key _) :: _, 'e' -> (* error *)
          error dec "missing value in dict"
        | _ -> (* generic error *)
          error dec (Printf.sprintf "expected value, got %c" c));
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
    | S key, ((PS_D d) :: stack) ->
      (* new key for the map *)
      dec.stack <- (PS_D_key (key, d)) :: stack;
    | _, ((PS_D d) :: _) ->
      (* error: key must be string *)
      error dec "dict keys must be strings"
    | _, (PS_D_key (key, d)) :: stack ->
      (* new binding for the map *)
      dec.stack <- (PS_D (SMap.add key v d)) :: stack;
    | _ -> assert false
  (* signal error *)
  and error dec msg =
    let msg = Printf.sprintf "Bencode: error at line %d, column %d: %s"
      dec.l dec.c msg in
    dec.stack <- [PS_error msg]

  (* exported parse function *)
  let parse dec s i len =
    (if i < 0 || i+len > String.length s
      then invalid_arg "Bencode.parse: not a valid substring");
    (* add the input to [dec] *)
    if dec.len = 0
      then begin
        dec.buf <- String.copy s;
        dec.i <- i;
        dec.len <- len;
      end else begin
        (* use a buffer to merge the stored input and the new input *)
        let buf' = String.create (dec.len + len - dec.i) in
        String.blit dec.buf dec.i buf' 0 dec.len;
        String.blit s i buf' dec.len len;
        dec.buf <- buf';
        dec.i <- 0;
        dec.len <- dec.len + len - dec.i;
      end;
    (* state machine *)
    parse_rec dec

  val feed : t -> string -> unit
  (** For manual mode, provide some input *)

  type result =
    | Yield of token
    | End
    | Await  (** The user needs to call {!feed} with some input *)

  val next : t -> result
end

