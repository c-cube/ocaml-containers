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

(** {1 Very simple JSON parser/printer} *)

type t =
  | Int of int
  | Float of float
  | String of string
  | Null
  | Bool of bool
  | List of t list
  | Object of (string * t) list

(** {2 Print/parse} *)

let lex =
  Genlex.make_lexer ["{"; "}"; ":"; ","; "["; "]"; "true"; "false"; "null"]

exception EOF

let parse chars =
  let tokens = lex chars in
  let open Stream in
  let rec next () =
    match peek tokens with
    | None -> raise EOF (* end stream *)
    | Some (Genlex.Kwd "{") ->
      junk tokens;
      let args = read_pairs [] in
      (match peek tokens with
        | Some (Genlex.Kwd "}") ->
          junk tokens; Object args
        | _ -> raise (Stream.Error "expected '}'"))
    | Some (Genlex.Kwd "[") ->
      junk tokens;
      let args = read_list [] in
      (match peek tokens with
        | Some (Genlex.Kwd "]") ->
          junk tokens; List args
        | _ -> raise (Stream.Error "expected ']'"))
    | Some (Genlex.Int i) -> junk tokens; Int i
    | Some (Genlex.Float f) -> junk tokens; Float f
    | Some (Genlex.Kwd "true") -> junk tokens; Bool true
    | Some (Genlex.Kwd "false") -> junk tokens; Bool false
    | Some (Genlex.Kwd "null") -> junk tokens; Null
    | Some (Genlex.String s) -> junk tokens; String s
    | _ -> raise (Stream.Error "expected JSON value")
  and read_list acc =
    match peek tokens with
    | Some (Genlex.Kwd "]") -> List.rev acc  (* yield *)
    | _ ->
      let t = next () in
      (match peek tokens with
        | Some (Genlex.Kwd ",") ->
          junk tokens;
          read_list (t::acc)  (* next *)
        | Some (Genlex.Kwd "]") ->
          read_list (t::acc)  (* next *)
        | _ -> raise (Stream.Error "expected ','"))
  and read_pairs acc =
    match peek tokens with
    | Some (Genlex.Kwd "}") -> List.rev acc  (* yield *)
    | _ ->
      let k, v = pair () in
      (match peek tokens with
        | Some (Genlex.Kwd ",") ->
          junk tokens;
          read_pairs ((k,v)::acc)  (* next *)
        | Some (Genlex.Kwd "}") ->
          read_pairs ((k,v)::acc)  (* next *)
        | _ -> raise (Stream.Error "expected ','"))
  and pair () = 
    match Stream.npeek 2 tokens with
    | [Genlex.String k; Genlex.Kwd ":"] ->
      junk tokens; junk tokens;
      let v = next () in
      k, v
    | _ -> raise (Stream.Error "expected pair")
  in
  Stream.from
    (fun _ ->
      try Some (next ())
      with EOF -> None)

let parse_one chars =
  Stream.peek (parse chars)

let rec output oc t =
  match t with
  | Null -> output_string oc "null"
  | Bool true -> output_string oc "true"
  | Bool false -> output_string oc "false"
  | Int i -> Printf.fprintf oc "%d" i
  | Float f -> Printf.fprintf oc "%f" f
  | String s -> Printf.fprintf oc "\"%s\"" (String.escaped s)
  | List l ->
    output_string oc "[";
    List.iteri
      (fun i t ->
        (if i > 0 then output_string oc ", ");
        output oc t)
      l;
    output_string oc "]"
  | Object pairs ->
    output_string oc "{";
    List.iteri
      (fun i (k,v) ->
        (if i > 0 then output_string oc ", ");
        Printf.fprintf oc "\"%s\": " k;
        output oc v)
      pairs;
    output_string oc "}"

let rec pp fmt t =
  match t with
  | Null -> Format.pp_print_string fmt "null"
  | Bool true -> Format.pp_print_string fmt "true"
  | Bool false -> Format.pp_print_string fmt "false"
  | Int i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%f" f
  | String s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | List l ->
    Format.pp_print_string fmt "[";
    List.iteri
      (fun i t  ->
        (if i > 0 then Format.pp_print_string fmt ", ");
        pp fmt t)
      l;
    Format.pp_print_string fmt "]"
  | Object pairs ->
    Format.pp_print_string fmt "{";
    List.iteri
      (fun i (k,v) ->
        (if i > 0 then Format.pp_print_string fmt ", ");
        Format.fprintf fmt "\"%s\": " k;
        pp fmt v)
      pairs;
    Format.pp_print_string fmt "}"

let to_string t =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a@?" pp t;
  Buffer.contents buf

(** {2 Utils *)

exception TypeError of string * t

