
(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 Applicative Parser Combinators} *)

type ('a,'b) result = [`Error of 'b | `Ok of 'a]

type multiplicity =
  | Star      (* 0 or more *)
  | Plus      (* 1 or more *)
  | Question  (* 0 or 1 *)

let str fmt = Printf.sprintf fmt

module CharSet = Set.Make(Char)
module CharMap = Map.Make(Char)

let print_char_set set =
  let l = CharSet.fold
      (fun c acc -> str "'%c'" c :: acc) set [] in
  String.concat ", " l

let domain_of_char_map m =
  CharMap.fold (fun c _ set -> CharSet.add c set) m CharSet.empty

let print_char_map map =
  let l = CharMap.fold
      (fun c _ acc -> str "'%c'" c :: acc) map [] in
  String.concat ", " l

(* function composition *)
let compose f g x = f (g x)

let string_of_list l =
  let b = Bytes.make (List.length l) ' ' in
  List.iteri (fun i c -> Bytes.set b i c) l;
  Bytes.unsafe_to_string b

type _ t =
  | Return : 'a -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Filter: ('a -> bool) * 'a t -> 'a t
  | App : ('a -> 'b) t * 'a t -> 'b t
  | AppLeft : 'a t * 'b t -> 'a t
  | AppRight : 'a t * 'b t -> 'b t
  | Fail : string -> 'a t
  | Int : int t
  | Float : float t
  | AnyOf : CharSet.t -> char t
  | Many : CharSet.t * 'a t * unit t * multiplicity -> 'a list t
  | Skip : CharSet.t * 'a t * multiplicity -> unit t (* same as Many, but ignores *)
  | SwitchC : 'a t CharMap.t * 'a t option -> 'a t
  | SwitchS :'a trie -> 'a t
  | Fix : ('a t -> 'a t) -> 'a t
  | Eof : unit t

(* a prefix trie *)
and 'a trie =
  | TrieEmpty
  | TrieNode of 'a t option * 'a trie CharMap.t

let return x = Return x

let success = Return ()

let fail msg = Fail msg

let app f x = App (f, x)

let map f x = match x with
  | Map (g, y) -> Map (compose f g, y)
  | Return x -> Return (f x)
  | _ -> Map (f,x)

let filter f x = Filter (f, x)

let int = Int

let float = Float

let int_first_char =
  lazy (CharSet.of_list ['-'; '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])

(* a set of characters that are valid as first characters of a parser *)
type possible_first_chars =
  | Set of CharSet.t
  | AllChars
  | NoChar
  | IsFail of string

(* set of possibilities for the first char of a parser *)
let rec possible_first_chars
  : type a. a t -> possible_first_chars
  = function
  | Return _ -> NoChar
  | Map (_, x) -> possible_first_chars x
  | Filter (_, x) -> possible_first_chars x
  | App (f, _) -> possible_first_chars f
  | AppLeft (a, _) -> possible_first_chars a
  | AppRight (a, _) -> possible_first_chars a
  | Fail e -> IsFail e
  | Int -> Set (Lazy.force int_first_char)
  | Float -> Set (Lazy.force int_first_char)
  | AnyOf set -> Set set
  | Many(set, _, _, _) -> Set set
  | Skip (set, _, _) -> Set set
  | SwitchC (map, None) -> Set (domain_of_char_map map)
  | SwitchC (_, Some _) -> AllChars
  | SwitchS TrieEmpty -> assert false
  | SwitchS (TrieNode (_, m)) -> Set (domain_of_char_map m)
  | Fix f ->
    let p = f (Fix f) in
    possible_first_chars p
  | Eof -> NoChar

let many ?(sep=success) t =
  match possible_first_chars t with
  | Set set -> Many (set, t, sep, Star)
  | IsFail msg -> Fail msg
  | AllChars -> invalid_arg (str "many: invalid parser (always succeeds)")
  | NoChar -> invalid_arg (str "many: invalid parser (does not consume input)")

let many1 ?(sep=success) t =
  match possible_first_chars t with
  | Set set -> Many (set, t, sep, Plus)
  | IsFail msg -> Fail msg
  | AllChars -> invalid_arg (str "many: invalid parser (always succeeds)")
  | NoChar -> invalid_arg (str "many: invalid parser (does not consume input)")

let skip t =
  match possible_first_chars t with
  | Set set -> Skip (set, t, Star)
  | IsFail msg -> Fail msg
  | AllChars -> invalid_arg (str "skip: invalid parser (always succeeds)")
  | NoChar -> invalid_arg (str "skip: invalid parser (does not consume input)")

let skip1 t =
  match possible_first_chars t with
  | Set set -> Skip (set, t, Plus)
  | IsFail msg -> Fail msg
  | AllChars -> invalid_arg (str "skip: invalid parser (always succeeds)")
  | NoChar -> invalid_arg (str "skip: invalid parser (does not consume input)")

let opt t =
  match possible_first_chars t with
  | Set set ->
    map
      (function
        | [x] -> Some x
        | [] -> None
        | _ -> assert false
      ) (Many (set, t, success, Question))
  | IsFail msg -> Fail msg
  | AllChars -> map (fun x -> Some x) t  (* always succeeds *)
  | NoChar -> invalid_arg (str "opt: invalid parser (does not consume input)")

let set_of_string s =
  let set = ref CharSet.empty in
  String.iter
    (fun c ->
       if CharSet.mem c !set
       then invalid_arg (str "any_of: duplicate char %c" c);
       set := CharSet.add c !set
    ) s;
  !set

let any_of s = AnyOf (set_of_string s)

let char c = AnyOf (CharSet.singleton c)

let spaces = skip (any_of " \t")
let spaces1 = skip1 (any_of " \t")

let white = skip (any_of " \t\n")
let white1 = skip1 (any_of " \t\n")

let alpha_lower_ = set_of_string "abcdefghijklmonpqrstuvwxyz"
let alpha_upper_ = set_of_string "ABCDEFGHIJKLMONPQRSTUVWXYZ"
let num_ = set_of_string "0123456789"
let alpha_ = CharSet.union alpha_lower_ alpha_upper_

let alpha_lower = AnyOf alpha_lower_
let alpha_upper = AnyOf alpha_upper_
let num = AnyOf num_
let alpha = AnyOf alpha_
let alpha_num = AnyOf (CharSet.union num_ alpha_)

let eof = Eof

let switch_c ?default l =
  if l = [] then match default with
    | None -> invalid_arg "switch_c: empty list";
    | Some d -> d
  else
  let map = List.fold_left
      (fun map (c, t) ->
         if CharMap.mem c map
         then invalid_arg (str "switch_c: duplicate char %c" c);
         CharMap.add c t map
      ) CharMap.empty l
  in
  SwitchC (map, default)

exception ExnIsFail of string

let choice l =
  if l = [] then invalid_arg "choice: empty list";
  (* build a switch by first char *)
  try
    (* a map and possibly a default parser *)
    let map, def = List.fold_left
        (fun (map, def) p ->
           match possible_first_chars p, def with
           | AllChars, Some _ ->
             invalid_arg "choice: ambiguous, several parsers accept any input"
           | AllChars, None -> map, Some p
           | NoChar, _ -> map, def
           | IsFail msg, _ -> raise (ExnIsFail msg)
           | Set set, def ->
             if CharSet.exists (fun c -> CharMap.mem c map) set
             then invalid_arg
                 (str "choice: ambiguous parsers (overlap on {%s})"
                    (print_char_set (CharSet.inter set (domain_of_char_map map))));
             let map = CharSet.fold
               (fun c map -> CharMap.add c p map)
               set map
             in map, def
        ) (CharMap.empty, None) l
    in
    SwitchC (map, def)
  with ExnIsFail msg ->
    fail msg

(* build prefix trie *)
let switch_s l =
  if l = [] then invalid_arg "switch_s: empty list";
  (* add parser p in trie [t], with key slice of [s]  starting at [i] *)
  let rec add_trie t s i p =
    if i = String.length s
    then match t with
      | TrieEmpty -> TrieNode (Some p, CharMap.empty)
      | TrieNode (Some _, _) -> invalid_arg (str "duplicate key \"%s\"" s)
      | TrieNode (None, m) -> TrieNode (Some p, m)
    else
      let c = String.get s i in
      match t with
      | TrieEmpty ->
        let sub = add_trie TrieEmpty s (i+1) p in
        TrieNode (None, CharMap.singleton c sub)
      | TrieNode (opt, map) ->
        try
          let sub = CharMap.find c map in
          let sub = add_trie sub s (i+1) p in
          TrieNode (opt, CharMap.add c sub map)
        with Not_found ->
          let sub = add_trie TrieEmpty s (i+1) p in
          TrieNode (opt, CharMap.add c sub map)
  in
  let trie =
    List.fold_left
      (fun trie (s, p) ->
         if s = "" then invalid_arg "switch_s: empty string";
         add_trie trie s 0 p
      ) TrieEmpty l
  in
  SwitchS trie

let bool =
  switch_s
    [ "true", Return true
    ; "false", Return false
    ]

let fix f = Fix f

module Infix = struct
  let (>|=) x f = map f x
  let (<*>) = app
  let (<<) a b = AppLeft (a, b)  (* return (fun x y -> x) <*> a <*> b *)
  let (>>) a b = AppRight (a, b) (* return (fun x y -> y) <*> a <*> b *)
  let (<+>) a b = choice [a; b]
end

include Infix

(* TODO: more efficient version, with buffer *)
let word =
  return (fun c s -> string_of_list (c :: s)) <*> alpha <*> many alpha_num

(** {2 Signatures} *)

type error = {
  line: int;
  col: int;
  msg: string;
}

let string_of_error e = str "at %d:%d; %s" e.line e.col e.msg

exception Error of error

module type S = sig
  type source
  (** Source of characters *)

  val parse : source -> 'a t -> ('a, error) result
  (** Parse the given source using the parser, and returns the parsed value. *)

  val parse': source -> 'a t -> ('a, string) result
  (** Same as {!parse}, but returns a user-friendly string in case of failure *)

  val parse_exn : source -> 'a t -> 'a
  (** Unsafe version of {!parse}.
      @raise Error if parsing fails *)
end

(** {2 Build a parser from a given Monadic Input} *)

module type INPUT = sig
  type t

  val read : t -> Bytes.t -> int -> int -> int
end

type token =
  | Yield of char
  | EOF

module type READER = sig
  type t
  type source

  val create : source -> t
  val peek : t -> token  (* peek; do not consume *)
  val next : t -> token  (* read and consume *)
  val junk : t -> unit   (* consume last token, obtained with junk *)
  val line : t -> int
  val col : t -> int
end

module ReaderOfInput(I : INPUT) : READER with type source = I.t = struct
  type t = {
    mutable rline : int;
    mutable rcol : int;
    input : I.t;
    buf : Bytes.t;
    mutable i : int;
    mutable len : int;
  }
  type source = I.t

  let line t = t.rline
  let col t = t.rcol

  let create input = {
    rline=0;
    rcol=0;
    input;
    buf = Bytes.make 1024 ' ';
    i=1;
    len=1; (* trick for initialization *)
  }

  let read_next t =
    let c = Bytes.get t.buf t.i in
    t.i <- t.i + 1;
    if c = '\n' then (
      t.rcol <- 0;
      t.rline <- t.rline + 1;
    ) else (
      t.rcol <- t.rcol + 1
    );
    Yield c

  let refill t =
    t.len <- I.read t.input t.buf 0 (Bytes.length t.buf);
    t.i <- 0;
    ()

  let next t =
    if t.len = 0 then EOF
    else if t.i = t.len
    then (
      refill t;
      if t.len = 0 then EOF else read_next t
    ) else read_next t

  let peek t =
    if t.i = t.len
    then refill t;
    Yield (Bytes.get t.buf t.i)

  let junk t =
    assert (t.len > 0 && t.i < t.len);
    t.i <- t.i + 1
end

module MakeFromReader(R : READER) : S with type source = R.source = struct
  type source = R.source

  let error r msg =
    raise (Error {
      line = R.line r;
      col = R.col r;
      msg;
    })
  let errorf r fmt =
    Printf.ksprintf
      (fun msg -> error r msg)
      fmt

  let is_int c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
  let to_int c = Char.code c - Char.code '0'

  let rec parse_int r sign i = match R.peek r with
    | EOF -> i
    | Yield c when is_int c ->
      R.junk r;
      parse_int r sign (10 * i + to_int c)
    | Yield '-' when i = 0 && sign ->
      (* switch sign: only on first char *)
      R.junk r;
      parse_int r false 0
    | _ -> if sign then i else -i

  let parse_float _r _buf = assert false

  let rec parse_rec : type a. R.t -> a t -> a =
    fun r p -> match p with
      | Return x -> x
      | Map (f, x) ->
        let y = parse_rec r x in
        f y
      | Filter (f, x) ->
        let y = parse_rec r x in
        if f y then y else errorf r "filter failed"
      | App (f, x) ->
        let f' = parse_rec r f in
        let x' = parse_rec r x in
        f' x'
      | AppLeft (a, b) ->
        let a' = parse_rec r a in
        let _ = parse_rec r b in
        a'
      | AppRight (a, b) ->
        let _ = parse_rec r a in
        let b' = parse_rec r b in
        b'
      | Fail msg -> error r msg
      | Int -> parse_int r true 0
      | Float -> parse_float r (Buffer.create 8)
      | AnyOf set ->
        begin match R.next r with
          | EOF -> errorf r "expected any of {%s}, got EOF" (print_char_set set)
          | Yield c ->
            if CharSet.mem c set then c
            else errorf r "expected any of {%s}, got %c" (print_char_set set) c
        end
      | Many (set, p, sep, mult) -> parse_many r ~set ~sep ~p ~mult []
      | Skip (set, p, mult) -> parse_skip r ~set ~p ~mult
      | SwitchC (map, def) ->
        begin match R.peek r with
          | EOF -> errorf r "expected any of {%s}, got EOF" (print_char_map map)
          | Yield c ->
            begin try
                let p' = CharMap.find c map in
                parse_rec r p'
              with Not_found ->
                match def with
                | None ->
                  errorf r "expected any of {%s}, got %c" (print_char_map map) c
                | Some d -> parse_rec r d
            end
        end
      | SwitchS TrieEmpty -> assert false
      | SwitchS (TrieNode (Some p, _)) ->
        parse_rec r p
      | SwitchS (TrieNode (None, map)) ->
        begin match R.next r with
          | EOF -> errorf r "expected any of {%s}, got EOF" (print_char_map map)
          | Yield c ->
            begin try
                let trie = CharMap.find c map in
                parse_rec r (SwitchS trie) (* recurse in subtree *)
              with Not_found ->
                errorf r "expected any of {%s}, got %c" (print_char_map map) c
            end
        end
      | Fix f ->
        let p = f (Fix f) in
        parse_rec r p
      | Eof ->
        begin match R.next r with
          | EOF -> ()
          | Yield c -> errorf r "expected EOF, got %c" c
        end

  and parse_many
    : type a. R.t -> set:CharSet.t -> p:a t -> sep:unit t ->
      mult:multiplicity -> a list -> a list
    = fun r ~set ~p ~sep ~mult acc ->
      match R.peek r with
      | EOF -> List.rev acc
      | Yield c ->
        if CharSet.mem c set
        then
          let x = parse_rec r p in
          match mult with
          | Question -> assert (acc = []); [x]
          | Plus | Star ->
            let _ = parse_rec r sep in (* separator *)
            parse_many r ~set ~p ~sep ~mult:Star (x::acc)
        else if mult = Plus
        then errorf r "expected {%s}, got %c" (print_char_set set) c
        else List.rev acc

  and parse_skip
    : type a. R.t -> set:CharSet.t -> p:a t -> mult:multiplicity -> unit
    = fun r ~set ~p ~mult ->
      match R.peek r with
      | EOF -> ()
      | Yield c ->
        if CharSet.mem c set
        then
          let _ = parse_rec r p in
          match mult with
          | Question -> ()
          | Plus | Star -> parse_skip r ~set ~p ~mult:Star
        else if mult = Plus
        then errorf r "expected {%s}, got %c" (print_char_set set) c
        else ()

  (* public functions *)
  let parse_exn src p =
    let r = R.create src in
    parse_rec r p

  let parse src p =
    let r = R.create src in
    try
      `Ok (parse_rec r p)
    with Error e ->
      `Error e

  let parse' src p = match parse src p with
    | `Ok x -> `Ok x
    | `Error e -> `Error (string_of_error e)
end

module Make(I : INPUT) = struct
  module R = ReaderOfInput(I)
  include MakeFromReader(R)
end

module Str = MakeFromReader(struct
  (* reader of string *)
  type t = {
    str : string;
    mutable i : int;
    mutable rcol : int;
    mutable rline : int;
  }
  type source = string

  let create str = {
    str;
    i = 0;
    rcol = 1;
    rline = 1;
  }
  let line t = t.rline
  let col t = t.rcol
  let peek t =
    if t.i = String.length t.str then EOF else Yield (String.get t.str t.i)
  let junk t =
    assert (t.i < String.length t.str);
    t.i <- t.i + 1
  let next t =
    if t.i = String.length t.str then EOF
    else (
      let c = String.get t.str t.i in
      t.i <- t.i + 1;
      if c = '\n' then (
        t.rcol <- 1;
        t.rline <- t.rline + 1
      ) else t.rcol <- t.rcol + 1;
      Yield c
    )
end)

module Chan = Make(struct
  type t = in_channel
  let read = input
end)
