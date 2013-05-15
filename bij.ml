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

(** {1 Bijective Serializer/Deserializer} *)

type _ t =
  | Unit : unit t
  | String : string t
  | Int : int t
  | Bool : bool t
  | Float : float t
  | List : 'a t -> 'a list t
  | Many : 'a t -> 'a list t
  | Opt : 'a t -> 'a option t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Map : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t
  | Switch : ('a -> char) * (char * 'a t) list -> 'a t

type 'a bij = 'a t

(** {2 Bijection description} *)

let unit_ = Unit
let string_ = String
let int_ = Int
let bool_ = Bool
let float_ = Float
let list_ l = List l
let many l = Many l
let opt t = Opt t
let pair a b = Pair(a,b)
let triple a b c = Triple (a,b,c)

let map ~inject ~extract b = Map (inject, extract, b)
let switch select l = Switch (select, l)

exception EOF

exception EncodingError of string
  (** Raised when decoding is impossible *)

exception DecodingError of string
  (** Raised when decoding is impossible *)

(** {2 Source of parsing} *)

module type SOURCE = sig
  type t

  val eof : t -> bool
    (** End of input reached? *)

  val cur : t -> char
    (** Current char *)

  val junk : t -> unit
    (** Discard current char *)
end

module SourceStr = struct
  type t = {
    str : string;
    mutable idx : int;
  }

  let create str =
    { str;
      idx = 0;
    }

  let eof t = t.idx = String.length t.str

  let cur t =
    if eof t then raise EOF else t.str.[t.idx]

  let junk t =
    if t.idx >= String.length t.str
      then raise EOF
      else t.idx <- t.idx + 1
end

module SourceStream = struct
  type t = char Stream.t

  let eof t = match Stream.peek t with
    | None -> true
    | Some _ -> false

  let cur t = match Stream.peek t with
    | None -> raise EOF
    | Some c -> c

  let junk t = Stream.junk t
end

module SourceChan = struct
  type t = {
    chan : in_channel;
    buf : string;
    mutable len : int;
    mutable idx : int;
  }

  let create ?(bufsize=256) ic =
    let t = { chan = ic;
      buf = String.make bufsize ' ';
      len = 0;
      idx = 0;
    } in
    (* fill the buffer *)
    t.len <- input t.chan t.buf 0 bufsize;
    t
  
  let eof t = t.len = 0

  let cur t =
    if eof t
      then raise EOF
      else t.buf.[t.idx]

  let junk t =
    (if t.len = 0 then raise EOF);
    t.idx <- t.idx + 1;
    if t.idx = t.len
      then begin  (* refill *)
        t.idx <- 0;
        t.len <- input t.chan t.buf 0 (String.length t.buf)
      end
end

(** {2 Sink: Where to print} *)

module type SINK = sig
  type t
  val write : t -> string -> int -> int -> unit  (* write substring [i..i+len] *)
  val write_char : t -> char -> unit
  val write_int : t -> int -> unit
  val write_bool : t -> bool -> unit
  val write_float : t -> float -> unit
end

module SinkBuf = struct
  type t = Buffer.t

  let write t str i len = Buffer.add_substring t str i len
  let write_char t c = Buffer.add_char t c
  let write_int t i = Printf.bprintf t "%d" i
  let write_bool t b = Printf.bprintf t "%B" b
  let write_float t f = Printf.bprintf t "%f" f
end

module SinkChan = struct
  type t = out_channel

  let write t str i len = output t str i len
  let write_char t c = output_char t c
  let write_int t i = Printf.fprintf t "%d" i
  let write_bool t b = Printf.fprintf t "%B" b
  let write_float t f = Printf.fprintf t "%f" f
end

(** {2 Encoding/decoding} *)

module type ENCODE = sig
  type sink
  val encode : bij:'a t -> sink -> 'a -> unit
end

module type DECODE = sig
  type source
  val decode : bij:'a t -> source -> 'a
end

module SexpEncode(Sink : SINK) = struct
  type sink = Sink.t

  (* print escaped string to [sink] *)
  let escape sink s =
    (* function that escapes into the given sink *)
    let rec really_escape sink s i =
      if i = String.length s
        then ()  (* done *)
        else begin
          (match s.[i] with
          | '\n' -> Sink.write sink "\\n" 0 2
          | '\t' -> Sink.write sink "\\t" 0 2
          | ' ' | ')' ->
            Sink.write_char sink '\\';
            Sink.write_char sink s.[i];
          | c ->
            Sink.write_char sink c);
          really_escape sink s (i+1)
        end
    in
    (* search for a char to escape, if any *)
    let rec search s i =
      if i = String.length s
        then Sink.write sink s 0 i (* no escaping needed *)
        else match s.[i] with
        | ' ' | '\t' | '\n' | ')' ->  (* must escape *)
          Sink.write sink s 0 i;
          really_escape sink s i (* escape starting at i *)
        | _ -> search s (i+1)
    in
    search s 0

  let encode ~bij sink x = 
    let open Sink in
    let rec encode : type a. a bij -> a -> unit = fun bij x ->
      match bij, x with
      | Unit, () -> ()
      | String, s -> escape sink s
      | Int, i -> Sink.write_int sink i
      | Bool, b -> Sink.write_bool sink b
      | Float, f -> Sink.write_float sink f
      | List bij', l ->
        Sink.write_char sink '(';
        List.iter
          (fun x -> Sink.write_char sink ' '; encode bij' x)
          l;
        Sink.write_char sink ')'
      | Many _, [] -> failwith "Bij.encode: expected non-empty list"
      | Many bij', l ->
        Sink.write_char sink '(';
        List.iteri
          (fun i x -> (if i > 0 then Sink.write_char sink ' '); encode bij' x)
          l;
        Sink.write_char sink ')'
      | Opt bij, None ->
        encode (List bij) []
      | Opt bij, Some x ->
        encode (List bij) [x]
      | Pair (bij_a, bij_b), (a, b) ->
        Sink.write_char sink '(';
        encode bij_a a;
        Sink.write_char sink ' ';
        encode bij_b b;
        Sink.write_char sink ')'
      | Triple (bij_a, bij_b, bij_c), (a, b, c) ->
        Sink.write_char sink '(';
        encode bij_a a;
        Sink.write_char sink ' ';
        encode bij_b b;
        Sink.write_char sink ' ';
        encode bij_c c;
        Sink.write_char sink ')'
      | Map (inject, _, bij'), x ->
        let y = inject x in
        encode bij' y
      | Switch (select, l), x ->
        let c = select x in
        try
          let bij' = List.assq c l in
          encode bij' x
        with Not_found ->
          raise (EncodingError "no encoding in switch")
    in encode bij x

end

module SexpDecode(Source : SOURCE) = struct
  type source = Source.t

  let decode ~bij source =
    let rec cur () = Source.cur source
    and junk () = Source.junk source
    and eof () = Source.eof source
    in
    (* eat whitespace *)
    let rec whitespace () =
      if not (eof ()) then match cur () with
        | ' ' | '\t' | '\n' -> junk (); whitespace ()
        | _ -> ()
    in
    (* decode using the [bij] *)
    let rec decode : type a. a bij -> a = fun bij ->
      whitespace ();
      match bij with
      | Unit -> ()
      | String -> decode_string (Buffer.create 5)
      | Int -> decode_int 0
      | Float ->
        begin try float_of_string (decode_string (Buffer.create 3))
        with Failure _ -> raise (DecodingError ("expected float"))
        end
      | Bool ->
        begin match decode_string (Buffer.create 4) with
        | "true" -> true
        | "false" -> false
        | s -> raise (DecodingError ("expected bool, got " ^ s))
        end
      | List bij' ->
        decode_open ();
        let l = decode_list bij' [] in
        decode_close ();
        l
      | Many bij' ->
        decode_open ();
        let l = decode_list bij' [] in
        decode_close ();
        if l = [] then raise (DecodingError "expected non empty list") else l
      | Opt bij' ->
        decode_open ();
        let l = decode_list bij' [] in
        decode_close ();
        begin match l with
        | [] -> None
        | [x] -> Some x
        | _ ->  raise (DecodingError "expected option")
        end
      | Pair (bija, bijb) ->
        decode_open ();
        let a = decode bija in
        let b = decode bijb in
        decode_close ();
        a, b
      | Triple (bija, bijb, bijc) ->
        decode_open ();
        let a = decode bija in
        let b = decode bijb in
        let c = decode bijc in
        decode_close ();
        a, b, c
      | Map (_, extract, bij') ->
        let x = decode bij' in
        extract x
      | Switch (_, choices) -> decode_switch choices
    and decode_open : unit -> unit = fun () -> match cur () with
      | '(' -> junk ()  (* done *)
      | _ -> raise (DecodingError "expected '('")
    and decode_close : unit -> unit = fun () ->
      whitespace ();  (* on close, first eat whitespace *)
      match cur () with
      | ')' -> junk ()  (* done *)
      | _ -> raise (DecodingError "expected ')'")
    and decode_int : int -> int = fun i ->
      if eof () then i
      else match cur () with
      | '-' when i = 0 -> junk (); ~- (decode_int 0)  (* negative *)
      | c when Char.code c >= Char.code '0' && Char.code c <= Char.code '9' ->
        junk ();
        decode_int (i * 10 + (Char.code c - Char.code '0'))
      | _ -> i
    and decode_string : Buffer.t -> string = fun buf ->
      if eof () then Buffer.contents buf
      else match cur() with
      | ' ' | '\t' | '\n' | ')' -> Buffer.contents buf
      | '\\' -> junk (); Buffer.add_char buf (cur ()); junk (); decode_string buf
      | c -> Buffer.add_char buf c; junk (); decode_string buf
    and decode_list : type a. a t -> a list -> a list = fun bij l ->
      whitespace ();
      match cur() with
      | ')' -> List.rev l  (* done *)
      | _ ->
        let x = decode bij in
        decode_list bij (x :: l)
    and decode_switch : type a. (char * a t) list -> a = fun choices ->
      let c = cur () in
      junk ();
      let bij =
        try List.assq c choices 
        with Not_found -> 
          try List.assq ' ' choices
          with Not_found -> raise (DecodingError "no choice")
      in
      decode bij
    in
    decode bij
end

module SexpStr = struct
  module SexpEncodeBuf = SexpEncode(SinkBuf)
  module SexpDecodeString = SexpDecode(SourceStr)

  let to_string ~bij x =
    let b = Buffer.create 15 in
    SexpEncodeBuf.encode ~bij b x;
    Buffer.contents b

  let of_string ~bij s =
    SexpDecodeString.decode ~bij (SourceStr.create s)
end
