(* This file is free software. See file "license" for more details. *)

(** {1 Very Simple Parser Combinators} *)

open CCShims_

(*$inject
  module T = struct
    type tree = L of int | N of tree * tree
  end
  open T
  open Result

  let mk_leaf x = L x
  let mk_node x y = N(x,y)

  let ptree = fix @@ fun self ->
    skip_space *>
    ( (char '(' *> (pure mk_node <*> self <*> self) <* char ')')
      <|>
      (U.int >|= mk_leaf) )

  let ptree' = fix_memo @@ fun self ->
    skip_space *>
    ( (char '(' *> (pure mk_node <*> self <*> self) <* char ')')
      <|>
      (U.int >|= mk_leaf) )

  let rec pptree = function
    | N (a,b) -> Printf.sprintf "N (%s, %s)" (pptree a) (pptree b)
    | L x -> Printf.sprintf "L %d" x

  let errpp pp = function
    | Ok x -> "Ok " ^ pp x
    | Error s -> "Error " ^ s

  let errpptree = errpp pptree

  let erreq eq x y = match x, y with
    | Ok x, Ok y -> eq x y
    | Error _ , Error _ -> true
    | _ -> false ;;
*)

(*$= & ~printer:errpptree
  (Ok (N (L 1, N (L 2, L 3)))) \
    (parse_string ptree "(1 (2 3))" )
  (Ok (N (N (L 1, L 2), N (L 3, N (L 4, L 5))))) \
    (parse_string ptree "((1 2) (3 (4 5)))" )
  (Ok (N (L 1, N (L 2, L 3)))) \
    (parse_string ptree' "(1 (2 3))" )
  (Ok (N (N (L 1, L 2), N (L 3, N (L 4, L 5))))) \
    (parse_string ptree' "((1 2) (3 (4 5)))" )
*)

(*$R
  let p = U.list ~sep:"," U.word in
  let printer = function
    | Ok l -> "Ok " ^ CCFormat.(to_string (Dump.list string_quoted)) l
    | Error s -> "Error " ^ s
  in
  assert_equal ~printer
    (Ok ["abc"; "de"; "hello"; "world"])
    (parse_string p "[abc , de, hello ,world  ]");
*)

(*$R
  let test n =
    let p = CCParse.(U.list ~sep:"," U.int) in

    let l = CCList.(1 -- n) in
    let l_printed =
      CCFormat.(to_string (within "[" "]" (list ~sep:(return ",") int))) l in

    let l' = CCParse.parse_string_exn p l_printed in

    assert_equal ~printer:Q.Print.(list int) l l'
  in
  test 300_000;

*)

(*$R
  let open CCParse.Infix in
  let module P = CCParse in

  let parens p = P.char '(' *> p <* P.char ')' in
  let add = P.char '+' *> P.return (+) in
  let sub = P.char '-' *> P.return (-) in
  let mul = P.char '*' *> P.return ( * ) in
  let div = P.char '/' *> P.return ( / ) in
  let integer =
  P.chars1_if (function '0'..'9'->true|_->false) >|= int_of_string in

  let chainl1 e op =
  P.fix (fun r ->
    e >>= fun x -> (op <*> P.return x <*> r) <|> P.return x) in

  let expr : int P.t =
  P.fix (fun expr ->
    let factor = parens expr <|> integer in
    let term = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub)) in

  assert_equal (Ok 6) (P.parse_string expr "4*1+2");
  assert_equal (Ok 12) (P.parse_string expr "4*(1+2)");
  ()
*)

(* TODO: [type position = {state: state; i: int}] and recompute line, col
   on demand *)
type position = {
  pos_buffer: string;
  pos_offset: int;
  mutable pos_lc: (int * int) option;
}

module Position = struct
  type t = position

  (* actually re-compute line and column from the buffer *)
  let compute_line_and_col_ (s:string) (off:int) : int * int =
    let i = ref 0 in
    let continue = ref true in
    let line = ref 1 in
    let col = ref 1 in
    while !continue && !i < off && !i < String.length s do
      match String.index_from s !i '\n' with
        | exception Not_found ->
          col := off - !i; continue := false;
        | j when j > off ->
          col := off - !i; continue := false;
        | j -> incr line; i := j+1;
    done;
    !line, !col

  let line_and_column self =
    match self.pos_lc with
      | Some tup -> tup
      | None ->
        let tup = compute_line_and_col_ self.pos_buffer self.pos_offset in
        self.pos_lc <- Some tup; (* save *)
        tup

  let line self = fst (line_and_column self)
  let column self = snd (line_and_column self)
  let pp out self =
    let l, c = line_and_column self in
    Format.fprintf out "at line %d, column %d" l c
end

module Error = struct
  type t = {
    msg: unit -> string;
    pos: position;
  }

  let position self = self.pos
  let line_and_column self = Position.line_and_column self.pos

  let msg self = self.msg()
  let to_string self =
    let line,col = line_and_column self in
    Printf.sprintf "at line %d, char %d:\n%s" line col (self.msg())

  let pp out self =
    let line,col = line_and_column self in
    Format.fprintf out "at line %d, char %d:@ %s" line col (self.msg())
end

type 'a or_error = ('a, Error.t) result

module Memo_tbl = Hashtbl.Make(struct
    type t = int * int  (* id of parser, position *)
    let equal ((a,b):t)(c,d) = a=c && b=d
    let hash = Hashtbl.hash
  end)

module Memo_state = struct
  (* table of closures, used to implement universal type *)
  type t = (unit -> unit) Memo_tbl.t

  (* unique ID for each parser *)
  let id_ = ref 0
end

(** Purely functional state passed around *)
type state = {
  str: string; (* the input *)
  i: int; (* offset in [input.str] *)
  memo : Memo_state.t option ref; (* Memoization table, if any *)
}

let[@inline] char_equal (a : char) b = Stdlib.(=) a b
let string_equal = String.equal

(* FIXME: printer for error
let () = Printexc.register_printer
    (function
      | ParseError (b,msg) ->
        Some (Format.sprintf "@[<v>%s@ %s@]" (msg()) (string_of_branch b))
      | _ -> None)
   *)

let[@inline] const_str_ x () : string = x

let state_of_string str =
  let s = {
    str;
    i=0;
    memo=ref None;
  } in
  s

let[@inline] is_done st = st.i >= String.length st.str
let[@inline] cur st = st.str.[st.i]

let pos_of_st_ st : position = {pos_buffer=st.str; pos_offset=st.i; pos_lc=None}
let mk_error_ st msg : Error.t = {Error.msg; pos=pos_of_st_ st}

(* consume one char, passing it to [ok]. *)
let consume_ st ~ok ~err =
  if is_done st then (
    let msg = const_str_ "unexpected end of input" in
    err (mk_error_ st msg)
  ) else (
    let c = st.str.[st.i] in
    ok {st with i=st.i + 1} c
  )

type 'a t = {
  run: 'b. state -> ok:(state -> 'a -> 'b) -> err:(Error.t -> 'b) -> 'b;
} [@@unboxed]
(** Takes the input and two continuations:
    {ul
      {- [ok] to call with the result and new state when it's done}
      {- [err] to call when the parser met an error}
    }
*)

let return x : _ t = {
  run=fun st ~ok ~err:_ -> ok st x
}

let pure = return

let map f (p: 'a t) : _ t = {
  run=fun st ~ok ~err ->
    p.run st
      ~ok:(fun st x -> ok st (f x))
      ~err
}

let bind f (p:'a t) : _ t = {
  run=fun st ~ok ~err ->
    p.run st
      ~ok:(fun st x ->
          let p2 = f x in
          p2.run st ~ok ~err)
      ~err
}

let ap (f:_ t) (a:_ t) : _ t = {
  run=fun st ~ok ~err ->
    f.run st
      ~ok:(fun st f ->
          a.run st ~ok:(fun st x -> ok st (f x)) ~err)
      ~err
}

let ap_left (a:_ t) (b:_ t) : _ t = {
  run=fun st ~ok ~err ->
    a.run st
      ~ok:(fun st x ->
          b.run st ~ok:(fun st _ -> ok st x) ~err)
      ~err
}

let ap_right (a:_ t) (b:_ t) : _ t = {
  run=fun st ~ok ~err ->
    a.run st
      ~ok:(fun st _ ->
          b.run st ~ok:(fun st x -> ok st x) ~err)
      ~err
}

let or_ (p1:'a t) (p2:'a t) : _ t = {
  run=fun st ~ok ~err ->
    p1.run st ~ok
      ~err:(fun _e -> p2.run st ~ok ~err)
}

let both a b = {
  run=fun st ~ok ~err ->
    a.run st
      ~ok:(fun st xa ->
          b.run st ~ok:(fun st xb -> ok st (xa,xb)) ~err)
      ~err
}

let set_error_message msg (p:'a t) : _ t = {
  run=fun st ~ok ~err ->
    p.run st ~ok
      ~err:(fun _e -> err (mk_error_ st (const_str_ msg)))
}


module Infix = struct
  let[@inline] (>|=) p f = map f p
  let[@inline] (>>=) p f = bind f p
  let (<*>) = ap
  let (<* ) = ap_left
  let ( *>) = ap_right
  let (<|>) = or_
  let (|||) = both
  let[@inline] (<?>) p msg = set_error_message msg p

  include CCShimsMkLet_.Make(struct
      type nonrec 'a t = 'a t
      let (>>=) = (>>=)
      let (>|=) = (>|=)
      let monoid_product = both
    end)
end

include Infix

let map2 f x y = pure f <*> x <*> y
let map3 f x y z = pure f <*> x <*> y <*> z

let junk_ (st:state) : state =
  assert (st.i < String.length st.str);
  {st with i=st.i + 1}

let eoi = {
  run=fun st ~ok ~err ->
  if is_done st
  then ok st ()
  else err (mk_error_ st (const_str_ "expected end of input"))
}

(*$= & ~printer:(errpp Q.Print.bool) ~cmp:(erreq (=))
  (Ok true) (parse_string (U.bool <* eoi) "true")
  (Error "") (parse_string (U.bool <* eoi) "true ")
  (Ok true) (parse_string (U.bool <* skip_white <* eoi) "true")
*)

let fail msg : _ t = {
  run=fun st ~ok:_ ~err ->
    err (mk_error_ st (const_str_ msg))
}
let failf msg = Printf.ksprintf fail msg

let parsing what p = {
  run=fun st ~ok ~err ->
    p.run st ~ok
      ~err:(fun e ->
          let msg() =
            Printf.sprintf "while parsing %s:\n%s" what (e.Error.msg ())
          in
          err {e with Error.msg})
}

let nop = {
  run=fun st ~ok ~err:_ -> ok st ();
}

let any_char = {
  run=fun st ~ok ~err -> consume_ st ~ok ~err
}

let char c : _ t = {
  run=fun st ~ok ~err ->
    consume_ st
      ~ok:(fun st c2 ->
          if char_equal c c2 then ok st c
          else (
            let msg() = Printf.sprintf "expected '%c', got '%c'" c c2 in
            err (mk_error_ st msg)
          ))
      ~err
}

let char_if ?descr p = {
  run=fun st ~ok ~err ->
    consume_ st
      ~ok:(fun st c ->
          if p c then ok st c
          else (
            let msg() =
              let rest = match descr with
                | None -> ""
                | Some d -> Printf.sprintf ", expected %s" d
              in
              Printf.sprintf "unexpected char '%c'%s" c rest
            in
            err (mk_error_ st msg)
          ))
      ~err
}

let chars_if p = {
  run=fun st ~ok ~err:_ ->
    let i0 = st.i in
    let i = ref i0 in
    while
      let st = {st with i = !i} in
      not (is_done st) && p (cur st)
    do
      incr i;
    done;
    ok {st with i = !i} (String.sub st.str i0 (!i - i0))
}

let chars1_if ?descr p = {
  run=fun st ~ok ~err ->
    (chars_if p).run st
      ~ok:(fun st s ->
          if string_equal s ""
          then (
            let msg() =
              let what = match descr with
                | None -> ""
                | Some d -> Printf.sprintf " for %s" d
              in
              Printf.sprintf "expected non-empty sequence of chars%s" what
            in
            err (mk_error_ st msg)
          ) else ok st s)
      ~err
}

let skip_chars p : _ t =
  let rec self = {
    run=fun st ~ok ~err ->
      if not (is_done st) && p (cur st) then (
        let st = junk_ st in
        self.run st ~ok ~err
      ) else ok st ()
  }
  in
  self

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
let is_num = function '0' .. '9' -> true | _ -> false
let is_alpha_num = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
let is_space = function ' ' | '\t' -> true | _ -> false
let is_white = function ' ' | '\t' | '\n' -> true | _ -> false

let space = char_if is_space
let white = char_if is_white

let endline =
  char_if ~descr:"end-of-line ('\\n')" (function '\n' -> true | _ -> false)
let skip_space = skip_chars is_space
let skip_white = skip_chars is_white

let try_or p1 ~f ~else_:p2 = {
  run=fun st ~ok ~err ->
    p1.run st
      ~ok:(fun st x -> (f x).run st ~ok ~err)
      ~err:(fun _ -> p2.run st ~ok ~err)
}

let suspend f = {
  run=fun st ~ok ~err ->
    let p = f () in
    p.run st ~ok ~err
}

(* read [len] chars at once *)
let any_chars len : _ t = {
  run=fun st ~ok ~err ->
    if st.i + len <= String.length st.str then (
      let s = String.sub st.str st.i len in
      let st = {st with i = st.i + len} in
      ok st s
    ) else (
      let msg() =
        Printf.sprintf "expected to be able to consume %d chars" len
      in
      err (mk_error_ st msg)
    )
}

let exact s = {
  run=fun st ~ok ~err ->
    (* parse a string of length [String.length s] and compare with [s] *)
    (any_chars (String.length s)).run st
      ~ok:(fun st s2 ->
          if string_equal s s2 then ok st s
          else (
            let msg() = Printf.sprintf "expected %S, got %S" s s2 in
            err (mk_error_ st msg)
          )
        )
      ~err
}

let string = exact

let fix f =
  let rec self = {
    run=fun st ~ok ~err ->
      (Lazy.force f_self).run st ~ok ~err
  }
  and f_self = lazy (f self) in
  self

let try_ p = p

let try_opt p : _ t = {
  run=fun st ~ok ~err:_ ->
    p.run st
      ~ok:(fun st x -> ok st (Some x))
      ~err:(fun _ -> ok st None)
}

let optional p : _ t = {
  run=fun st ~ok ~err:_ ->
    p.run st
      ~ok:(fun st _x -> ok st ())
      ~err:(fun _ -> ok st ())
}

let many_until ~until p : _ t =
  fix
    (fun self ->
       try_or until ~f:(fun _ -> pure [])
         ~else_:(
           p >>= fun x ->
           self >|= fun l -> x :: l
         ))

let many p : _ t =
  fix
    (fun self ->
       try_or p
         ~f:(fun x -> self >|= fun tl -> x :: tl)
         (pure []))

(*
(* parse many [p], as a difference list *)
let many_rec_ p : (_ list -> _ list) t =
  let rec self = {
    run=fun st ~ok ~err ->
      if is_done st then ok st (fun l->l) (* empty list *)
      else (
        p.run st
          ~ok:(fun st x ->
              self.run st
                ~ok:(fun st f -> ok st (fun l -> x :: f l))
                ~err)
          ~err
      )
  } in
  self

let many p : _ t = {
  run=fun st ~ok ~err ->
    (many_rec_ p).run st
      ~ok:(fun st f -> ok st (f []))
      ~err
}
   *)

(*$R
  let p0 = skip_white *> U.int in
  let p = (skip_white *> char '(' *> many p0) <* (skip_white <* char ')') in
  let printer =  CCFormat.(to_string @@ Dump.result  @@ Dump.list int) in
  assert_equal ~printer
    (Ok [1;2;3]) (parse_string p "(1 2 3)");
  assert_equal ~printer
    (Ok [1;2; -30; 4]) (parse_string p "( 1 2    -30 4 )")
  *)


let many1 p =
  p >>= fun x ->
  many p >|= fun l -> x :: l

(* skip can be made efficient by not allocating intermediate parsers *)
let skip p : _ t =
  let rec self = {
    run=fun st ~ok ~err ->
      p.run st
        ~ok:(fun st _ -> self.run st ~ok ~err)
        ~err:(fun _ ->
            ok st ())
  } in
  self

let sep_until ~until ~by p =
  let rec read_p = lazy (
    p >>= fun x ->
    (until *> pure [x])
    <|>
    (by *> (Lazy.force read_p >|= fun tl -> x :: tl))
  ) in
  (until *> pure [])
  <|> (Lazy.force read_p)

let sep ~by p =
  let rec read_p = lazy (
    try_or p
      ~f:(fun x ->
          (eoi *> pure [x])
          <|>
          try_or by
            ~f:(fun _ -> Lazy.force read_p >|= fun tl -> x :: tl)
            (pure [x]))
      (pure [])
  ) in
  Lazy.force read_p

(*$inject
  let aword = chars1_if (function 'a'..'z'|'A'..'Z'->true|_ -> false);;
*)
(*$= & ~printer:(errpp Q.Print.(list string))
(Ok ["a";"b";"c"]) \
  (parse_string (optional (char '/') *> sep ~by:(char '/') aword) "/a/b/c")
(Ok ["a";"b";"c"]) \
  (parse_string (optional (char '/') *> sep ~by:(char '/') aword) "a/b/c")
*)

let sep1 ~by p =
  p >>= fun x ->
  sep ~by p >|= fun tl ->
  x :: tl

let lookahead p : _ t = {
  run=fun st ~ok ~err ->
    p.run st
      ~ok:(fun _st x -> ok st x) (* discard old state *)
      ~err
}

(*$= & ~printer:(errpp Q.Print.(string))
  (Ok "abc") (parse_string (lookahead (string "ab") *> (string "abc")) "abcd")
*)

let line : _ t = {
  run=fun st ~ok ~err ->
    if is_done st then err (mk_error_ st (const_str_ "expected a line, not EOI"))
    else (
      match String.index_from st.str st.i '\n' with
      | j ->
        let s = String.sub st.str st.i (j - st.i) in
        ok {st with i=j+1} s
      | exception Not_found ->
        err (mk_error_ st (const_str_ "unterminated line"))
    )
}

(*$=
  (Ok "1234") (parse_string line "1234\nyolo")
  (Ok ("1234", "yolo")) (parse_string (line ||| line) "1234\nyolo\nswag")
*)

(* parse a string [s] using [p_sub], then parse [s] using [p].
   The result is that of parsing [s] using [p], but the state is
   the one after using [p_sub], and errors are translated back into the context
   of [p_sub].
   This can be useful for example in [p_sub line some_line_parser]. *)
let parse_sub_ p_sub p : _ t = {
  run=fun st0 ~ok ~err ->
    let p = p <* eoi in (* make sure [p] reads all *)
    p_sub.run st0
      ~ok:(fun st1 s ->
          p.run (state_of_string s)
            ~ok:(fun _ r -> ok st1 r)
            ~err:(fun e ->
                let pos = e.pos in
                let pos = {
                  pos_buffer=pos.pos_buffer;
                  pos_offset=pos.pos_offset + st0.i;
                  pos_lc=None;
                } in
                err {e with pos}))
      ~err
}

let each_line p : _ t =
  fix
    (fun self ->
       try_or eoi
         ~f:(fun _ -> pure [])
         (parse_sub_ line p >>= fun x ->
          self >|= fun tl -> x :: tl))

(*$= & ~printer:(errpp Q.Print.(list @@ list int))
  (Ok ([[1;1];[2;2];[3;3]])) \
    (parse_string (each_line (sep ~by:skip_space U.int)) "1 1\n2 2\n3   3\n")
*)

let memo (type a) (p:a t) : a t =
  let id = !Memo_state.id_ in
  incr Memo_state.id_;
  let r = ref None in (* used for universal encoding *)

  {run=fun st ~ok ~err ->
    let tbl = match !(st.memo) with
      | Some t -> t
      | None ->
        let tbl = Memo_tbl.create 32 in
        st.memo := Some tbl;
        tbl
    in

    match
      r := None;
      let f = Memo_tbl.find tbl (st.i, id) in
      f();
      !r
    with
      | None -> assert false
      | Some (Ok (st,x)) -> ok st x
      | Some (Error e) -> err e
      | exception Not_found ->
        (* parse, and save *)
        p.run st
          ~ok:(fun st' x ->
            Memo_tbl.replace tbl (st.i,id) (fun () -> r := Some (Ok (st',x)));
            ok st' x)
          ~err:(fun e ->
            Memo_tbl.replace tbl (st.i,id) (fun () -> r := Some (Error e));
            err e)
  }

let fix_memo f =
  let rec p = {
    run=fun st ~ok ~err -> (Lazy.force p').run st ~ok ~err
  }
  and p' = lazy (memo (f p)) in
  p

exception ParseError of Error.t

let stringify_result = function
  | Ok _ as x -> x
  | Error e -> Error (Error.to_string e)

let parse_string_exn p s =
  p.run (state_of_string s)
    ~ok:(fun _st x -> x)
    ~err:(fun e -> raise (ParseError e))

let parse_string_e p s =
  p.run (state_of_string s)
    ~ok:(fun _st x -> Ok x)
    ~err:(fun e -> Error e)

let parse_string p s = parse_string_e p s |> stringify_result

let read_all_ ic =
  let buf = Buffer.create 1024 in
  begin
    try
      while true do
        let line = input_line ic in
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
      done;
      assert false
    with End_of_file -> ()
  end;
  Buffer.contents buf

let parse_file_e p file =
  let ic = open_in file in
  let s = read_all_ ic in
  let r = parse_string_e p s in
  close_in ic;
  r

let parse_file p file = parse_file_e p file |> stringify_result

let parse_file_exn p file =
  match parse_file_e p file with
    | Ok x -> x
    | Error e -> raise (ParseError e)

module U = struct
  let sep_ = sep

  let list ?(start="[") ?(stop="]") ?(sep=";") p =
    string start *> skip_white *>
    sep_until
      ~until:(skip_white <* string stop)
      ~by:(skip_white *> string sep *> skip_white) p

  let int =
    skip_white *>
    chars1_if ~descr:"integer" (fun c -> is_num c || char_equal c '-')
    >>= fun s ->
    try return (int_of_string s)
    with Failure _ -> fail "expected an int"

  (*$= & ~printer:(errpp Q.Print.int) ~cmp:(erreq (=))
    (Ok 42) (parse_string U.int " 42")
    (Ok 2) (parse_string U.int "2")
    (Error "") (parse_string U.int "abc")
    (Error "") (parse_string U.int "")
  *)

  let in_paren (p:'a t) : 'a t =
    skip_white *>
    (char '(' *> skip_white *> p <* skip_white <* char ')')

  let in_parens_opt (p:'a t) : 'a t =
    fix (fun self ->
      skip_white *>
      try_or
        (char '(')
        ~f:(fun _ -> skip_white *> self <* skip_white <* char ')')
        ~else_:p)

  (*$= & ~printer:(errpp Q.Print.int) ~cmp:(erreq (=))
    (Ok 15) (parse_string (U.in_paren (U.in_paren U.int)) "( ( 15) )")
    (Ok 2) (parse_string (U.in_paren U.int) "(2)")
    (Error "") (parse_string (U.in_paren U.int) "2")
    (Error "") (parse_string (U.in_paren U.int) "")
    (Ok 2) (parse_string (U.in_parens_opt U.int) "((((2))))")
    (Ok 2) (parse_string (U.in_parens_opt U.int) "2")
    (Ok 200) (parse_string (U.in_parens_opt U.int) "( (  200 ) )")
  *)

  let option p =
    skip_white *>
    try_or
      (string "Some")
      ~f:(fun _ -> skip_white *> p >|= fun x -> Some x)
      ~else_:(string "None" *> return None)

  (*$= & ~printer:(errpp Q.Print.(option int)) ~cmp:(erreq (=))
    (Ok (Some 12)) (parse_string U.(option int) " Some 12")
    (Ok None) (parse_string U.(option int) "  None")
    (Ok (Some 0)) (parse_string U.(option int) "Some 0")
    (Ok (Some 0)) (parse_string U.(in_parens_opt @@ option int) "(( Some 0) )")
  *)

  let hexa_int =
    (exact "0x" <|> return "") *>
    begin
      chars1_if (function '0' .. '9' | 'a'..'f' | 'A'..'F' -> true | _ -> false)
      >|= fun s ->
      let i = ref 0 in
      String.iter
        (fun c ->
           let n = match c with
             | '0' .. '9' -> Char.code c - Char.code '0'
             | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
             | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
             | _ -> assert false
           in
           i := !i * 16 + n)
        s;
      !i
    end

  (*$= & ~printer:(errpp Q.Print.int) ~cmp:(erreq (=))
    (Ok 16) (parse_string U.hexa_int "0x10")
    (Ok 16) (parse_string U.hexa_int "10")
    (Error "") (parse_string U.hexa_int "x10")
    (Error "") (parse_string U.hexa_int "0xz")
  *)

  let prepend_str c s = String.make 1 c ^ s

  let word =
    map2 prepend_str (char_if is_alpha) (chars_if is_alpha_num)

  let bool =
    skip_white *>
    ((string "true" *> return true) <|> (string "false" *> return false))
  (*$= & ~printer:(errpp Q.Print.bool) ~cmp:(erreq (=))
    (Ok true) (parse_string U.bool "true")
    (Ok false) (parse_string U.bool "false")
    *)

  let pair ?(start="(") ?(stop=")") ?(sep=",") p1 p2 =
    skip_white *> string start *> skip_white *>
      p1 >>= fun x1 ->
    skip_white *> string sep *> skip_white *>
      p2 >>= fun x2 ->
    skip_white *> string stop *> return (x1,x2)

  (*$= & ~printer:Q.Print.(errpp (pair int int))
    (Ok(1,2)) U.(parse_string (pair int int) "(1 , 2 )")
  *)

  let triple ?(start="(") ?(stop=")") ?(sep=",") p1 p2 p3 =
    string start *> skip_white *>
      p1 >>= fun x1 ->
    skip_white *> string sep *> skip_white *>
      p2 >>= fun x2 ->
    skip_white *> string sep *> skip_white *>
      p3 >>= fun x3 ->
    string stop *> return (x1,x2,x3)
end
