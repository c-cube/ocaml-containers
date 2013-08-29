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

(** {6 B-encoding} *)


module SMap = Map.Make(String)

type t =
  | I of int
  | S of string
  | L of t list
  | D of t SMap.t

let rec eq t1 t2 = match t1, t2 with
  | I i1, I i2 -> i1 = i2
  | S s1, S s2 -> s1 = s2
  | L l1, L l2 ->
    (try List.for_all2 eq l1 l2 with Invalid_argument _ -> false)
  | D d1, D d2 ->
    SMap.equal eq d1 d2
  | _ -> false

let hash t = Hashtbl.hash t

let dict_of_list l =
  let d = List.fold_left
    (fun d (k, v) -> SMap.add k v d)
    SMap.empty l
  in
  D d

(** {2 Serialization (encoding)} *)

(* length of an encoded int, in bytes *)
let _len_int i =
  match i with
  | 0 -> 1
  | _ when i < 0 -> 2 + int_of_float (log10 (float_of_int ~-i))
  | _ -> 1 + int_of_float (log10 (float_of_int i))

(* length of an encoded string, in bytes *)
let _len_str s =
  _len_int (String.length s) + 1 + String.length s

let rec size t = match t with
  | I i -> 2 + _len_int i
  | S s -> _len_str s
  | L l -> List.fold_left (fun acc i -> acc + size i) 2 l
  | D map -> SMap.fold (fun k v acc -> acc + _len_str k + size v) map 2

let write_in_string t buf o =
  let pos = ref o in
  let rec append t = match t with
  | I i -> write_char 'i'; write_int i; write_char 'e'
  | S s -> write_str s
  | L l ->
    write_char 'l';
    List.iter append l;
    write_char 'e';
  | D m ->
    write_char 'd';
    SMap.iter (fun key t' -> write_str key; append t') m;
    write_char 'e'
  and write_int i =
    let s = string_of_int i in
    String.blit s 0 buf !pos (String.length s);
    pos := !pos + String.length s
  and write_str s =
    write_int (String.length s);
    write_char ':';
    String.blit s 0 buf !pos (String.length s);
    pos := !pos + String.length s
  and write_char c =
    buf.[!pos] <- c;
    incr pos
  in
  append t

let to_string t =
  let len = size t in
  let s = String.create len in
  write_in_string t s 0;
  s

let to_buf buf t =
  Buffer.add_string buf (to_string t)

let to_chan ch t =
  let b = Buffer.create 25 in
  to_buf b t;
  Buffer.output_buffer ch b

let fmt formatter t =
  let b = Buffer.create 25 in
  to_buf b t;
  Format.pp_print_string formatter (Buffer.contents b)

let rec pretty fmt t = match t with
  | I i -> Format.fprintf fmt "%d" i
  | S s -> Format.fprintf fmt "@[<h>\"%s\"@]" s
  | L l ->
    Format.fprintf fmt "@[<hov 2>[@,";
    List.iteri (fun i t' -> (if i > 0 then Format.pp_print_char fmt ' '); pretty fmt t') l;
    Format.fprintf fmt "]@]";
  | D d ->
    Format.fprintf fmt "@[<hov 2>{@,";
    SMap.iter
      (fun k t' -> Format.fprintf fmt "%a -> %a@ " pretty (S k) pretty t')
      d;
    Format.fprintf fmt "}@]";
    ()

let pretty_to_str t =
  let b = Buffer.create 15 in
  Format.fprintf (Format.formatter_of_buffer b) "%a@?" pretty t;
  Buffer.contents b

(** {2 Deserialization (decoding)} *)

(** Deserialization is based on the {! decoder} type. Parsing can be
    incremental, in which case the input is provided chunk by chunk and
    the decoder contains the parsing state. Once a B-encoded value
    has been parsed, other values can still be read. *)

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
  | ParsePartial -> invalid_arg "Bencode: partial parse"
  | ParseError msg -> invalid_arg msg
