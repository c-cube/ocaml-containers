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

let print_char = function
  | '\t' -> "\\t"
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '"' -> "\\\""
  | c -> str "%c" c

let print_char_set set =
  let buf = Buffer.create 32 in
  Buffer.add_char buf '"';
  CharSet.iter (fun c -> Buffer.add_string buf (print_char c)) set;
  Buffer.add_char buf '"';
  Buffer.contents buf

let domain_of_char_map m =
  CharMap.fold (fun c _ set -> CharSet.add c set) m CharSet.empty

let print_char_map map =
  let l = CharMap.fold
      (fun c _ acc -> print_char c :: acc) map [] in
  String.concat ", " l

let ppmap ?(sep=", ") pp_k pp_v fmt m =
  let first = ref true in
  CharMap.iter
    (fun k v ->
      if !first then first := false else Format.pp_print_string fmt sep;
      pp_k fmt k;
      Format.pp_print_string fmt " → ";
      pp_v fmt v;
      Format.pp_print_cut fmt ()
    ) m;
  ()

let set_of_string s =
  let set = ref CharSet.empty in
  String.iter
    (fun c ->
       if CharSet.mem c !set
       then invalid_arg (str "any_of: duplicate char %c" c);
       set := CharSet.add c !set
    ) s;
  !set

(* add [c -> p] to the map, for every [c] in [set] *)
let map_add_set init set p =
  CharSet.fold
    (fun c map -> CharMap.add c p map)
    set init

(* function composition *)
let compose f g x = f (g x)

let str_of_l l =
  let b = Bytes.make (List.length l) ' ' in
  List.iteri (fun i c -> Bytes.set b i c) l;
  Bytes.unsafe_to_string b

type 'a t = {
  mutable st : 'a parse_or_compiled;
}

(* syntactic version *)
and _ parse =
  | Many : 'a t * unit t * multiplicity -> 'a list parse
  | Skip : 'a t * multiplicity -> unit parse (* same as Many, but ignores *)
  | Lazy : 'a t lazy_t -> 'a parse

(* compiled version *)
and _ compiled =
  | C_Return : 'a -> 'a compiled
  | C_Map : ('a -> 'b) * 'a t -> 'b compiled
  | C_Filter: ('a -> bool) * 'a t -> 'a compiled
  | C_App : ('a -> 'b) t * 'a t -> 'b compiled
  | C_AppLeft : 'a t * 'b t -> 'a compiled
  | C_AppRight : 'a t * 'b t -> 'b compiled
  | C_Fail : string -> 'a compiled
  | C_Int : int compiled
  | C_Float : float compiled
  | C_Junk : unit compiled (* ignore next char *)
  | C_AnyOf : CharSet.t -> char compiled
  | C_SwitchC : 'a t CharMap.t * 'a t option -> 'a compiled
  | C_Eof : unit compiled

and 'a parse_or_compiled =
  | Parse of 'a parse
  | Compiled of 'a compiled

(** {2 Helpers} *)

(* build a new parser *)
let make p = {st=Parse p}
let make_c c = {st=Compiled c}
let make_pc st = {st}

let ppmult fmt = function
  | Star -> Format.pp_print_string fmt "*"
  | Plus -> Format.pp_print_string fmt "+"
  | Question -> Format.pp_print_string fmt "?"

let print fmt p =
  let depth = ref 0 in
  (* print up to a given limit into lazy values *)
  let rec print_aux
    : type a. Format.formatter -> a t -> unit
    = fun fmt p ->
      let ppstr = Format.pp_print_string
      and ppf fmt x = Format.fprintf fmt x in
      let ppc fmt c = ppf fmt "'%s'" (print_char c) in
      match p.st with
      | Compiled (C_Return _) -> ppstr fmt "<ret>"
      | Compiled (C_Map (_, x)) -> ppf fmt "@[(map@ %a)@]" print_aux x
      | Compiled (C_Filter (_, x)) -> ppf fmt "@[(filter@ %a)@]" print_aux x
      | Compiled (C_App (f, x)) -> ppf fmt "@[<2>@[%a@]@ <*>@ @[%a@]@]" print_aux f print_aux x
      | Compiled (C_AppLeft (a, b)) -> ppf fmt "@[%a@ <<@ %a@]" print_aux a print_aux b
      | Compiled (C_AppRight (a, b)) -> ppf fmt "@[%a@ >>@ %a@]" print_aux a print_aux b
      | Compiled (C_Fail _) -> ppf fmt "<fail>"
      | Compiled C_Int -> ppstr fmt "<int>"
      | Compiled C_Float -> ppstr fmt "<float>"
      | Compiled C_Junk -> ppstr fmt "<junk>"
      | Compiled (C_AnyOf set) -> ppf fmt "@[(any@ %s)@]" (print_char_set set)
      | Parse (Many (p, sep, mult)) ->
        ppf fmt "@[<2>(@[%a@]@ sep:@[%a@])%a@]" print_aux p print_aux sep ppmult mult
      | Parse (Skip (p, mult)) ->
        ppf fmt "@[<2>(skip @[%a@]%a)@]" print_aux p ppmult mult
      | Compiled (C_SwitchC (map, None)) ->
        ppf fmt "@[<hv2>(switch@ @[%a@])@]" (ppmap ppc print_aux) map
      | Compiled (C_SwitchC (map, Some o)) ->
        ppf fmt "@[<hv2>(switch@ @[%a@]@ or:%a)@]" (ppmap ppc print_aux) map print_aux o
      | Parse (Lazy _) when !depth > 3 -> ppf fmt "<lazy>"
      | Parse (Lazy (lazy p)) ->
        incr depth;
        print_aux fmt p;
        decr depth
      | Compiled C_Eof -> ppstr fmt "<eof>"
  in
  print_aux fmt p

let int_first_char = lazy (set_of_string "-0123456789")
let float_first_char = lazy (set_of_string ".-0123456789")

(* a set of characters that are valid as first characters of a parser *)
type possible_first_chars =
  | Set of CharSet.t
  | AllChars
  | NoChar
  | NoCharOrSet of CharSet.t (* either no char, or something starting with set *)
  | IsFail of string

let ret_set set = match CharSet.cardinal set with
  | 0 -> NoChar
  | 256 -> AllChars
  | _ -> Set set

let ret_no_char_or set = match CharSet.cardinal set with
  | 0 -> NoChar
  | 256 -> AllChars
  | _ -> NoCharOrSet set

(* pfc of parsing a or b *)
let union_pfc a b = match a, b with
  | Set a, Set b -> ret_set (CharSet.union a b)
  | NoCharOrSet s, Set s'
  | Set s', NoCharOrSet s -> ret_no_char_or (CharSet.union s s')
  | NoChar, Set s
  | Set s, NoChar -> ret_no_char_or s
  | NoCharOrSet s, NoCharOrSet s' -> ret_no_char_or (CharSet.union s s')
  | IsFail e, _ | _, IsFail e -> IsFail e
  | AllChars, _ | _, AllChars -> AllChars
  | NoChar, o | o, NoChar -> o

(* pfc of parsing a then b *)
let then_pfc a b = match a, b with
  | Set a, Set b -> ret_set (CharSet.union a b)
  | NoCharOrSet s, NoCharOrSet s' -> ret_no_char_or (CharSet.union s s')
  | NoCharOrSet s, Set s' -> ret_set (CharSet.union s s')
  | NoCharOrSet s, NoChar -> ret_no_char_or s
  | Set s, _ -> ret_set s
  | IsFail e, _ | _, IsFail e -> IsFail e
  | AllChars, _ | _, AllChars -> AllChars
  | NoChar, o -> o

let (<|||>) a b = match a with
  | NoChar -> Lazy.force b
  | NoCharOrSet _ -> then_pfc a (Lazy.force b)
  | _ -> a

(* set of possibilities for the first char of a parser *)
let rec pfc : type a. a t -> possible_first_chars = fun t -> pfc_pc t.st

and pfc_pc
  : type a. a parse_or_compiled -> possible_first_chars
  = function
  | Parse p -> pfc_p p
  | Compiled c -> pfc_c c

and pfc_p
  : type a. a parse -> possible_first_chars
  = function
  | Many (p, _, (Question | Star)) -> union_pfc (pfc p) NoChar
  | Many (p, _, Plus) -> pfc p
  | Skip (p, (Question | Star)) -> union_pfc (pfc p) NoChar
  | Skip (p, Plus) -> pfc p
  | Lazy (lazy p) -> pfc p

and pfc_c
  : type a. a compiled -> possible_first_chars
  = function
  | C_Return _ -> NoChar
  | C_Map (_, x) -> pfc x
  | C_Filter (_, x) -> pfc x
  | C_App (f, x) -> pfc f <|||> lazy (pfc x)
  | C_AppLeft (a, b) -> pfc a <|||> lazy (pfc b)
  | C_AppRight (a, b) -> pfc a <|||> lazy (pfc b)
  | C_Fail e -> IsFail e
  | C_Int -> Set (Lazy.force int_first_char)
  | C_Float -> Set (Lazy.force float_first_char)
  | C_Junk -> AllChars
  | C_AnyOf set -> ret_set set
  | C_SwitchC (map, None) -> ret_set (domain_of_char_map map)
  | C_SwitchC (map, Some o) ->
    let s = domain_of_char_map map in
    union_pfc (ret_set s) (pfc o)
  | C_Eof -> NoChar

let possible_first_chars = pfc

(** {2 Combinators} *)

let return x = make_c (C_Return x)
let pure = return

let success = pure ()

let fail msg = make_c (C_Fail msg)

let junk = make_c C_Junk

let failf fmt = Printf.ksprintf (fun msg -> fail msg) fmt

let map f x = match x.st with
  | Compiled (C_Map (g, y)) -> make_c (C_Map (compose f g, y))
  | Compiled (C_Return x) -> pure (f x)
  | _ -> make_c (C_Map (f, x))

let app f x = match f.st with
  | Compiled (C_Return f) -> map f x
  | _ -> make_c (C_App (f, x))

let fun_and f f' x = f x && f' x

let filter f x = match x.st with
  | Compiled (C_Return y) -> if f y then return y else fail "filter failed"
  | Compiled (C_Filter (f', y)) -> make_c (C_Filter (fun_and f f', y))
  | _ -> make_c (C_Filter (f, x))

let app_left a b = make_c (C_AppLeft (a, b))  (* return (fun x y -> x) <*> a <*> b *)

let app_right a b = make_c (C_AppRight (a, b)) (* return (fun x y -> y) <*> a <*> b *)

let int = make_c C_Int

let float = make_c C_Float

let many ?(sep=success) p = make (Many (p, sep, Star))

let many1 ?(sep=success) p = make (Many (p, sep, Plus))

let skip p = make (Skip (p, Star))

let skip1 p = make (Skip (p, Plus))

let opt p =
    map
      (function
        | [x] -> Some x
        | [] -> None
        | _ -> assert false
      ) (make (Many (p, success, Question)))

let any_of' s = make_c (C_AnyOf s)
let any_of s = any_of' (set_of_string s)

let char c = any_of' (CharSet.singleton c)

let spaces = skip (any_of " \t")
let spaces1 = skip1 (any_of " \t")

let white = skip (any_of " \t\n")
let white1 = skip1 (any_of " \t\n")

let alpha_lower_ = set_of_string "abcdefghijklmonpqrstuvwxyz"
let alpha_upper_ = set_of_string "ABCDEFGHIJKLMONPQRSTUVWXYZ"
let num_ = set_of_string "0123456789"
let alpha_ = CharSet.union alpha_lower_ alpha_upper_
let symbols_ = set_of_string "|!;$#@%&-_/="

let alpha_lower = any_of' alpha_lower_
let alpha_upper = any_of' alpha_upper_
let num = any_of' num_
let symbols = any_of' symbols_
let alpha = any_of' alpha_
let alpha_num = any_of' (CharSet.union num_ alpha_)

let eof = make_c C_Eof

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
  make_c (C_SwitchC (map, default))

exception ExnIsFail of string

let make_switch_c a b = make_c (C_SwitchC (a, b))

(* binary choice: compiled into decision tree *)
let rec merge a b =
  (* build a switch by first char *)
  try
    begin match a.st, b.st with
      | Compiled (C_SwitchC (map_a, def_a)),
        Compiled (C_SwitchC (map_b, def_b)) ->
        (* merge jump tables *)
        let def = match def_a, def_b with
          | None, None -> None
          | Some d, None
          | None, Some d -> Some d
          | Some _, Some _ ->
          invalid_arg "choice: ambiguous, several parsers accept any input"
        in
        let map = CharMap.merge
            (fun _ a b -> match a, b with
              | Some a', Some b' -> Some (merge a' b')
              | Some m, None
              | None, Some m -> Some m
              | None, None -> assert false
            ) map_a map_b
        in
        make_switch_c map def
      | Compiled (C_SwitchC (map, def)), other
      | other, Compiled (C_SwitchC (map, def)) ->
        let map', def' = match pfc_pc other, def with
          | AllChars, _ ->
            invalid_arg "choice: ambiguous, several parsers accept any input"
          | NoChar, None -> map, Some (make_pc other)
          | NoChar, Some _ ->
            invalid_arg "choice: ambiguous"
          | IsFail msg, _ -> raise (ExnIsFail msg)
          | NoCharOrSet set, def
          | Set set, def ->
            if CharSet.exists (fun c -> CharMap.mem c map) set
            then invalid_arg
                (str "choice: ambiguous parsers (overlap on {%s})"
                   (print_char_set (CharSet.inter set (domain_of_char_map map))));
            (* else: merge jump tables *)
            let map = map_add_set map set (make_pc other) in
            map, def
        in
        make_switch_c map' def'
      | _ ->
        begin match possible_first_chars a, possible_first_chars b with
          | (Set set1 | NoCharOrSet set1), (Set set2 | NoCharOrSet set2) ->
            if CharSet.exists (fun c -> CharSet.mem c set2) set1
            then invalid_arg
                (str "choice: ambiguous parsers (overlap on {%s})"
                   (print_char_set (CharSet.inter set1 set2)));
            let map = map_add_set CharMap.empty set1 a in
            let map = map_add_set map set2 b in
            make_switch_c map None
          | IsFail e, _ | _, IsFail e -> raise (ExnIsFail e)
          | Set s, NoChar -> make_switch_c (map_add_set CharMap.empty s a) (Some b)
          | NoChar, Set s -> make_switch_c (map_add_set CharMap.empty s b) (Some a)
          | AllChars, _ | _, AllChars ->
            invalid_arg "choice: ambiguous parsers (one accepts everything)"
          | (NoChar | NoCharOrSet _), (NoChar | NoCharOrSet _) ->
            invalid_arg "choice: ambiguous parsers (both accept nothing)"
        end
    end
  with ExnIsFail msg -> make_c (C_Fail msg)

let rec choice = function
  | [] -> invalid_arg "choice: empty list";
  | [x] -> x
  | a :: tl -> merge a (choice tl)

(* temporary structure for buildings switches *)
type 'a trie =
  | TrieLeaf of 'a t
  | TrieNode of 'a trie CharMap.t

let trie_empty = TrieNode CharMap.empty

let rec parser_of_trie : type a. a trie -> a t = function
  | TrieLeaf p -> p
  | TrieNode m ->
    make_switch_c (CharMap.map parser_of_trie' m) None
(* consume next char, then build sub-trie *)
and parser_of_trie'
  : type a. a trie -> a t
  = fun x -> app_right junk (parser_of_trie x)

(* build prefix trie *)
let switch_s l =
  if l = [] then invalid_arg "switch_s: empty list";
  (* add parser p in trie [t], with key slice of [s]  starting at [i] *)
  let rec add_trie t s i p =
    if i = String.length s
    then match t with
      | TrieNode m when CharMap.is_empty m -> TrieLeaf p
      | TrieNode _ -> invalid_arg (str "key \"%s\" is prefix of another key" s)
      | TrieLeaf _  -> invalid_arg (str "duplicate key \"%s\"" s)
    else
      let c = String.get s i in
      match t with
      | TrieLeaf _ ->
        invalid_arg (str "key \"%s\" is prefixed by another key" s)
      | TrieNode map ->
        try
          let sub = CharMap.find c map in
          let sub = add_trie sub s (i+1) p in
          TrieNode (CharMap.add c sub map)
        with Not_found ->
          let sub = add_trie trie_empty s (i+1) p in
          TrieNode (CharMap.add c sub map)
  in
  let trie =
    List.fold_left
      (fun trie (s, p) ->
         if s = "" then invalid_arg "switch_s: empty string";
         add_trie trie s 0 p
      ) trie_empty l
  in
  parser_of_trie trie

let bool =
  switch_s
    [ "true", return true
    ; "false", return false
    ]

let fix f =
  (* outermost lazy needed for the recursive definition *)
  let rec r = {
    st=Parse (Lazy (lazy (f r)));
  } in
  r

module Infix = struct
  let (>|=) x f = map f x
  let (<*>) = app
  let (<<) = app_left
  let (>>) = app_right
  let (<+>) a b = choice [a; b]
  let (<::>) a b = pure (fun x l -> x::l) <*> a <*> b
end

include Infix

let word =
  pure (fun c s -> str_of_l (c :: s)) <*> alpha <*> many alpha_num

let quoted =
  let q = char '"' in
  let escaped = char '\\' >> char '"' in
  let inner = choice [escaped; alpha_num; any_of "()' \t\n|!;$#@%&-_/=~.,:<>[]"] in
  q >> (many inner >|= str_of_l) << q

(** {2 Compilation} *)

let encode_cons x sep tl = pure (fun x _sep tl -> x :: tl) <*> x <*> sep <*> tl

let encode_many
  : type a. set:CharSet.t -> p:a t -> self:a list t -> sep:unit t -> a list t
  = fun ~set ~p ~self ~sep ->
    let on_success = encode_cons p sep self
    and on_fail = pure [] in
    make_switch_c (map_add_set CharMap.empty set on_success) (Some on_fail)

let encode_opt ~set x =
  let mk_one x = [x] in
  let on_success = make_c (C_Map (mk_one, x))
  and on_fail = pure [] in
  make_switch_c (map_add_set CharMap.empty set on_success) (Some on_fail)

let encode_skip
  : type a. set:CharSet.t -> p:a t -> self:unit t -> unit t
  = fun ~set ~p ~self ->
    let on_success = p >> self
    and on_fail = pure () in
    make_switch_c (map_add_set CharMap.empty set on_success) (Some on_fail)

let many_
  : type a. sep:unit t -> mult:multiplicity -> p:a t -> a list t
  = fun ~sep ~mult ~p -> match possible_first_chars p with
  | Set set ->
    begin match mult with
      | Star -> fix (fun self -> encode_many ~set ~sep ~p ~self)
      | Plus -> encode_cons p sep (fix (fun self -> encode_many ~set ~sep ~p ~self))
      | Question -> encode_opt ~set p
    end
  | IsFail msg -> fail msg
  | NoCharOrSet _ -> invalid_arg (str "many: invalid parser (might not consume input)")
  | AllChars -> invalid_arg (str "many: invalid parser (always succeeds)")
  | NoChar -> invalid_arg (str "many: invalid parser (does not consume input)")

let skip_ : type a. mult:multiplicity -> p:a t -> unit t
  = fun ~mult ~p -> match possible_first_chars p with
  | Set set ->
    begin match mult with
      | Star -> fix (fun self -> encode_skip ~set ~p ~self)
      | Plus -> p >> fix (fun self -> encode_skip ~set ~p ~self)
      | Question -> encode_opt ~set p >> pure ()
    end
  | IsFail msg -> fail msg
  | NoCharOrSet _ -> invalid_arg (str "many: invalid parser (might not consume input)")
  | AllChars -> invalid_arg (str "skip: invalid parser (always succeeds)")
  | NoChar -> invalid_arg (str "skip: invalid parser (does not consume input)")

let rec compile
  : type a. a t -> a compiled
  = fun t -> match t.st with
  | Compiled c -> c (* already compiled *)
  | Parse (Many (p, sep, mult)) ->
      let c = compile (many_ ~sep ~mult ~p) in
      t.st <- Compiled c;
      c
  | Parse (Skip (p, mult)) ->
    let c = compile (skip_ ~mult ~p) in
    t.st <- Compiled c;
    c
  | Parse (Lazy (lazy p)) ->
    let c = compile p in
    t.st <- Compiled c;
    c

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
    rline=1;
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

  let rec parse_int r ~sign i = match R.peek r with
    | EOF -> i
    | Yield c when is_int c ->
      R.junk r;
      parse_int r ~sign (10 * i + to_int c)
    | Yield '-' when i = 0 && sign ->
      (* switch sign: only on first char *)
      R.junk r;
      parse_int r ~sign:false 0
    | _ -> if sign then i else -i

  let parse_float _r _buf = assert false

  let rec parse_rec : type a. R.t -> a t -> a =
    fun r p -> match compile p with
      | C_Return x -> x
      | C_Map (f, x) ->
        let y = parse_rec r x in
        f y
      | C_Filter (f, x) ->
        let y = parse_rec r x in
        if f y then y else errorf r "filter failed"
      | C_App (f, x) ->
        let f' = parse_rec r f in
        let x' = parse_rec r x in
        f' x'
      | C_AppLeft (a, b) ->
        let a' = parse_rec r a in
        let _ = parse_rec r b in
        a'
      | C_AppRight (a, b) ->
        let _ = parse_rec r a in
        let b' = parse_rec r b in
        b'
      | C_Fail msg -> error r msg
      | C_Int -> parse_int r ~sign:true 0
      | C_Float -> parse_float r (Buffer.create 8)
      | C_Junk -> R.junk r
      | C_AnyOf set ->
        begin match R.next r with
          | EOF -> errorf r "expected any of %s, got EOF" (print_char_set set)
          | Yield c ->
            if CharSet.mem c set then c
            else errorf r "expected any of %s, got '%s'" (print_char_set set) (print_char c)
        end
      | C_SwitchC (map, def) ->
        begin match R.peek r with
          | EOF -> errorf r "expected any of %s, got EOF" (print_char_map map)
          | Yield c ->
            begin try
              let p' = CharMap.find c map in
              parse_rec r p'
            with Not_found -> match def with
              | None ->
                errorf r "expected any of %s, got %c" (print_char_map map) c
              | Some d -> parse_rec r d
            end
        end
      | C_Eof ->
        begin match R.next r with
          | EOF -> ()
          | Yield c -> errorf r "expected EOF, got %c" c
        end

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
