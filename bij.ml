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

exception EncodingError of string
  (** Raised when decoding is impossible *)

exception DecodingError of string
  (** Raised when decoding is impossible *)

(** {2 Source of parsing} *)

module Source = struct
  type t = string -> int  (* fills the buffer *)

  let of_str s =
    let i = ref 0 in
    fun buf ->
      let len = min (String.length s - !i) (String.length buf) in
      if len = 0
        then 0  (* done *)
        else begin
          String.blit s !i buf 0 len;
          i := !i + len;
          len
        end

  let of_stream str =
    fun buf ->
      let rec fill i =
        if i = String.length buf
          then i
          else match Stream.peek str with
          | None -> i  (* done *)
          | Some c ->
            buf.[i] <- c;
            Stream.junk str;
            fill (i+1)
      in
      fill 0

  let of_chan ic =
    fun buf ->
      input ic buf 0 (String.length buf)
end

(** {2 Sink: Where to print} *)

module Sink = struct
  type t = {
    mutable write : string -> unit;
    mutable write_int : int -> unit;
    mutable write_bool : bool -> unit;
    mutable write_float : float -> unit;
  }

  let of_buf buf =
    { write = (fun s -> Buffer.add_string buf s);
      write_int = (fun i -> Printf.bprintf buf "%d" i);
      write_bool = (fun b -> Printf.bprintf buf "%B" b);
      write_float = (fun f -> Printf.bprintf buf "%f" f);
    }

  let of_chan oc =
    { write = (fun s -> output_string oc s);
      write_int = (fun i -> Printf.fprintf oc "%d" i);
      write_bool = (fun b -> Printf.fprintf oc "%B" b);
      write_float = (fun f -> Printf.fprintf oc "%f" f);
    }
end

(** {2 Encoding/decoding} *)

module Sexp = struct
  (* escape string *)
  let escape s =
    (* function that escapes into the given buffer *)
    let rec really_escape buf s i =
      if i = String.length s
        then Buffer.contents buf
        else begin
          (match s.[i] with
          | '\n' -> Buffer.add_string buf "\\n"
          | '\t' -> Buffer.add_string buf "\\t"
          | ' ' | ')' ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf s.[i]
          | c -> Buffer.add_char buf c);
          really_escape buf s (i+1)
        end
    in
    (* search for a char to escape, if any *)
    let rec search s i =
      if i = String.length s then s (* no escaping needed *)
      else match s.[i] with
        | ' ' | '\t' | '\n' | ')' ->  (* must escape *)
          let buf = Buffer.create (String.length s + 1) in
          Buffer.add_substring buf s 0 i;
          really_escape buf s i (* escape starting at i *)
        | _ -> search s (i+1)
    in
    search s 0

  let encode ~bij sink x = 
    let open Sink in
    let rec encode : type a. a bij -> a -> unit = fun bij x ->
      match bij, x with
      | Unit, () -> ()
      | String, s -> sink.write (escape s)
      | Int, i -> sink.write_int i
      | Bool, b -> sink.write_bool b
      | Float, f -> sink.write_float f
      | List bij', l ->
        sink.write "(";
        List.iter
          (fun x -> sink.write " "; encode bij' x)
          l;
        sink.write ")"
      | Many _, [] -> failwith "Bij.encode: expected non-empty list"
      | Many bij', l ->
        sink.write "(";
        List.iter
          (fun x -> sink.write " "; encode bij' x)
          l;
        sink.write ")"
      | Opt bij, None ->
        encode (List bij) []
      | Opt bij, Some x ->
        encode (List bij) [x]
      | Pair (bij_a, bij_b), (a, b) ->
        sink.write "(";
        encode bij_a a;
        sink.write " ";
        encode bij_b b;
        sink.write ")"
      | Triple (bij_a, bij_b, bij_c), (a, b, c) ->
        sink.write "(";
        encode bij_a a;
        sink.write " ";
        encode bij_b b;
        sink.write " ";
        encode bij_c c;
        sink.write ")"
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

  let to_string ~bij x =
    let b = Buffer.create 15 in
    encode ~bij (Sink.of_buf b) x;
    Buffer.contents b

  let decode ~bij source =
    let str = String.make 64 '_' in
    let pos = ref 0 in
    let len = ref 0 in
    (* current token *)
    let rec cur () =
      if eof ()
        then raise (EncodingError "unexpected EOF")
        else str.[!pos]
    and eof () = !len = 0
    and refill () =
      len := source str;
      pos := 0
    and junk () =
      incr pos;
      if !pos >= !len then refill ()
    (* eat whitespace *)
    and whitespace () =
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
    refill ();   (* first input *)
    decode bij

  let of_string ~bij s = decode ~bij (Source.of_str s)
end
