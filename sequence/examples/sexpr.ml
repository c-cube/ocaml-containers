(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* {1 Basic S-expressions, with printing and parsing} *)

(** S-expression *)
type t =
  | Atom of string  (** An atom *)
  | List of t list  (** A list of S-expressions *)

(** Token that compose a Sexpr once serialized *)
type token = [`Open | `Close | `Atom of string]

(** {2 Traverse a sequence of tokens} *)

(** Iterate on the S-expression, calling the callback with tokens *)
let rec iter f s = match s with
  | Atom a -> f (`Atom a)
  | List l -> f `Open; iter_list f l; f `Close
and iter_list f l = match l with
  | [] -> ()
  | x::l' -> iter f x; iter_list f l'

(** Traverse. This yields a sequence of tokens *)
let traverse s = Sequence.from_iter (fun k -> iter k s)

(** Returns the same sequence of tokens, but during iteration, if
    the structure of the Sexpr corresponding to the sequence
    is wrong (bad parenthesing), Invalid_argument is raised
    and iteration is stoped *)
let validate seq =
  let depth = ref 0 in
  Sequence.map
    (fun tok -> match tok with
     | `Open -> incr depth; tok
     | `Close -> if !depth = 0
      then raise (Invalid_argument "wrong parenthesing")
      else decr depth; tok
     | _ -> tok)
    seq

(** {2 Text <-> tokens} *)

(** Lex: create a sequence of tokens from the given in_channel. *)
let lex input =
  let seq_fun k =
    let in_word = ref false in
    let buf = Buffer.create 128 in
    (* loop. TODO handle escaping of (), and "" *)
    let rec next c =
      match c with
      | '(' -> k `Open
      | ')' -> flush_word(); k `Close
      | ' ' | '\t' | '\n' -> flush_word ()
      | c -> in_word := true; Buffer.add_char buf c
    (* finish the previous word token *)
    and flush_word () =
      if !in_word then begin
        (* this whitespace follows a word *)
        let word = Buffer.contents buf in
        Buffer.clear buf;
        in_word := false;
        k (`Atom word)
      end
    in
    Sequence.iter next input
  in 
  Sequence.from_iter seq_fun

(** Build a Sexpr from a sequence of tokens *)
let of_seq seq =
  (* called on every token *)
  let rec k stack token = match token with
    | `Open -> `Open :: stack
    | `Close -> collapse [] stack
    | `Atom a -> (`Expr (Atom a)) :: stack
  (* collapse last list into an `Expr *)
  and collapse acc stack = match stack with
  | `Open::stack' -> `Expr (List acc) :: stack'
  | `Expr a::stack' -> collapse (a :: acc) stack'
  | _ -> assert false
  in
  (* iterate on the sequence, given an empty initial stack *)
  let stack = Sequence.fold k [] seq in
  (* stack should contain exactly one expression *)
  match stack with
  | [`Expr expr] -> expr
  | [] -> failwith "no Sexpr could be parsed"
  | _ -> failwith "too many elements on the stack"

(** {2 Printing} *)

(** Print a token on the given formatter *)
let pp_token formatter token = match token with
  | `Open -> Format.fprintf formatter "@[("
  | `Close -> Format.fprintf formatter ")@]"
  | `Atom s -> Format.pp_print_string formatter s

(** Print a sequence of Sexpr tokens on the given formatter *)
let pp_tokens formatter tokens =
  let first = ref true in
  let last = ref false in
  Sequence.iter
    (fun token ->
      (match token with
      | `Open -> (if not !first then Format.fprintf formatter " "); first := true
      | `Close -> first := false; last := true
      | _ -> if !first then first := false else Format.fprintf formatter " ");
      pp_token formatter token;
      if !last then last := false)
    tokens

(** Pretty-print the S-expr. If [indent] is true, the S-expression
    is printed with indentation. *)
let pp_sexpr ?(indent=false) formatter s =
  if indent
    then Format.fprintf formatter "@[<hov 4>%a@]" pp_tokens (traverse s)
    else pp_tokens formatter (traverse s)

(** {2 Serializing} *)

let output_seq name subexpr k =
  k `Open;
  k (`Atom name);
  Sequence.iter k subexpr;
  k `Close

let output_str name str k =
  k `Open;
  k (`Atom name);
  k (`Atom str);
  k `Close

(** {2 Parsing} *)

(** Monadic combinators for parsing data from a sequence of tokens,
    without converting to concrete S-expressions.

    The [one] parser can raise ParseFailure if it fails to parse
    the atomic type. *)

(** parser that returns a 'a *)
type 'a parser =
  | Return : 'a -> 'a parser
  | One : (token -> 'a) -> 'a parser
  | Zero : (token -> 'a parser) -> 'a parser
  (* | Maybe of (token -> 'a option) *)
  | Bind : ('b parser * ('b -> 'a parser)) -> 'a parser
  | Fail : string -> 'a parser

exception ParseFailure of string

let (>>=) p f = Bind (p, f)

let (>>) p p' = p >>= fun _ -> p'

let return x = Return x

let fail reason = Fail reason

let one f = One f

let skip = One (fun _ -> ())

let lookahead f = Zero f

let left = One (function | `Open -> ()
                         | _ -> raise (ParseFailure "expected '('"))

let right = One (function | `Close -> ()
                          | _ -> raise (ParseFailure "expected ')'"))

let pair f g =
  f >>= fun x ->
  g >>= fun y ->
  return (x, y)

let triple f g h =
  f >>= fun x ->
  g >>= fun y ->
  h >>= fun z ->
  return (x, y, z)

(** [(name,p) ^|| p'] behaves as p if the next token is [`Atom name], and
    like [p'] otherwise *)
let (^||) (name,p) p' =
  lookahead
    (fun token -> match token with
    | `Atom s when s = name -> skip >> p ()
    | _ -> p')

(** Maps the value returned by the parser *)
let map p f = p >>= fun x -> return (f x)

let p_str = one
  (function | `Atom s -> s | _ -> raise (ParseFailure "expected string"))

let p_int = one
  (function | `Atom s -> (try int_of_string s
                          with Failure _ -> raise (ParseFailure "expected int"))
              | _ -> raise (ParseFailure "expected int"))

let p_bool = one
  (function | `Atom s -> (try bool_of_string s
                          with Failure _ -> raise (ParseFailure "expected bool"))
              | _ -> raise (ParseFailure "expected bool"))

let p_float = one
  (function | `Atom s -> (try float_of_string s
                          with Failure _ -> raise (ParseFailure "expected float"))
              | _ -> raise (ParseFailure "expected float"))

let many p =
  let rec elements token =
    match token with
    | `Close -> return []
    | _ ->
      p >>= fun x ->
      lookahead elements >>= fun l ->
      return (x :: l)
  in
  left >> lookahead elements >>= fun l -> right >> return l

let many1 p =
  p >>= fun x ->
  many p >>= fun l ->
  return (x::l)

(** parsing state that returns a 'a *)
type 'a state =
  | Bottom : 'a state
  | Push : ('b parser * ('b -> 'a state)) -> 'a state

(** Actually parse the sequence of tokens, with a callback to be called
    on every parsed value. The callback decides whether to push another
    state or whether to continue. *)
let parse_k p tokens k =
  let rec state = Push(p, fun x -> match k x with `Stop -> Bottom | `Continue -> state) in
  (* Token handler. It also takes the current parser. *)
  let rec one_step state token =
    match reduce state with
    | Bottom ->  (* should not happen, unless there are too many tokens *)
      raise (ParseFailure "unexpected ')'")
    | Push (Return _, cont) ->
      assert false (* should be reduced *)
    | Push (Zero f, cont) ->
      let p' = f token in
      let state' = Push (p', cont) in
      one_step state' token  (* do not consume token *)
    | Push (One f, cont) ->
      let x = f token in
      let state' = cont x in
      reduce state'  (* consume token *)
    (* | Maybe f, _ -> let x = f token in (Obj.magic cont) x *)
    | Push (Bind (p', cont'), cont) ->
      let cont'' x =
        let p'' = cont' x in
        Push (p'', cont)
      in
      let state' = Push (p', cont'') in
      one_step state' token  (* do not consume token *)
    | Push (Fail reason, _) -> raise (ParseFailure reason)
  (* Reduce parser state *)
  and reduce state = match state with
    | Push (Return x, cont) ->
      let state' = cont x in
      reduce state'
    | _ -> state
  in
  (* iterate on the tokens *)
  ignore (Sequence.fold one_step state tokens)

(** Parse one value *)
let parse p tokens =
  let res = ref None in
  parse_k p tokens (fun x -> res := Some x; `Stop);
  (* return result *)
  match !res with
  | None -> raise (ParseFailure "incomplete input")
  | Some x -> x

(** Parse a sequence of values *)
let parse_seq p tokens =
  let seq_fun k =
    parse_k p tokens (fun x -> k x; `Continue)
  in
  Sequence.from_iter seq_fun

