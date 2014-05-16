
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
  | BeginList
  | End

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
        let o = open_out f in
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
      | End ->
          out.write_char 'e'
      | BeginList ->
          out.write_char 'l'
end

module Decode = struct
  type result =
    | Yield of token
    | Error of string
    | Await  (** The user needs to call {!feed} with some input *)

  type state =
    | Start
    | ParsingInt of int
    | ParsingString of string

  type t = {
    mutable buf : string;  (* buffer *)
    mutable i : int;  (* index in buf *)
    mutable len : int;  (* length of substring to read *)
    mutable c : int;  (* line *)
    mutable l : int;  (* column *)
    mutable state : state;
  }

  let create () = {
    buf = "";
    i = 0;
    len = 0;
    c = 0;
    l = 0;
    state = Start;
  }

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

  (*
  (* parse value *)
  let rec parse_rec dec =
    if is_empty dec then Await (* wait *)
    else begin
      let c = next dec in
      match dec.state, c with
      | Start, 'l' ->
          Yield StartList
      | Start, 'd' ->
          Yield StartDict
      | Start, 'e' ->
          Yield End
      | Start, 'i' ->
          dec.state <- ParsingInt 0
      | ParsingString i, 'e' ->
          dec.state <- Start;
          Yield (Int i)
      | 
  *)

  let feed dec = assert false

  let next dec = assert false
end

