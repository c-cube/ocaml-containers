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
    ( (try_ (char '(') *> (pure mk_node <*> self <*> self) <* char ')')
      <|>
      (U.int >|= mk_leaf) )

  let ptree' = fix_memo @@ fun self ->
    skip_space *>
    ( (try_ (char '(') *> (pure mk_node <*> self <*> self) <* char ')')
      <|>
      (U.int >|= mk_leaf) )

  let rec pptree = function
    | N (a,b) -> Printf.sprintf "N (%s, %s)" (pptree a) (pptree b)
    | L x -> Printf.sprintf "L %d" x

  let errpptree = function
    | Ok x -> "Ok " ^ pptree x
    | Error s -> "Error " ^ s
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
    | Ok l -> "Ok " ^ CCFormat.(to_string (list string)) l
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

  let parens p = P.try_ (P.char '(') *> p <* P.char ')' in
  let add = P.char '+' *> P.return (+) in
  let sub = P.char '-' *> P.return (-) in
  let mul = P.char '*' *> P.return ( * ) in
  let div = P.char '/' *> P.return ( / ) in
  let integer =
  P.chars1_if (function '0'..'9'->true|_->false) >|= int_of_string in

  let chainl1 e op =
  P.fix (fun r ->
    e >>= fun x -> P.try_ (op <*> P.return x <*> r) <|> P.return x) in

  let expr : int P.t =
  P.fix (fun expr ->
    let factor = parens expr <|> integer in
    let term = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub)) in

  assert_equal (Ok 6) (P.parse_string expr "4*1+2");
  assert_equal (Ok 12) (P.parse_string expr "4*(1+2)");
  ()
*)

type 'a or_error = ('a, string) result

type line_num = int
type col_num = int

module MemoTbl = struct
  module H = Hashtbl.Make(struct
      type t = int * int  (* id of parser, position *)
      let equal ((a,b):t)(c,d) = a=c && b=d
      let hash = Hashtbl.hash
    end)

  (* table of closures, used to implement universal type *)
  type t = (unit -> unit) H.t lazy_t

  let create n = lazy (H.create n)

  (* unique ID for each parser *)
  let id_ = ref 0

  type 'a res =
    | Fail of exn
    | Ok of 'a
end

type position = int * int * int (* pos, line, column *)

type parse_branch = (line_num * col_num * string option) list

type state = {
  str: string; (* the input *)
  mutable i: int; (* offset *)
  mutable lnum : line_num; (* Line number *)
  mutable cnum : col_num; (* Column number *)
  mutable branch: parse_branch;
  memo : MemoTbl.t; (* Memoization table, if any *)
}

exception ParseError of parse_branch * (unit -> string)

let char_equal (a : char) b = Stdlib.(=) a b
let string_equal (a : string) b = Stdlib.(=) a b

let rec string_of_branch l =
  let pp_s () = function
    | None -> ""
    | Some s -> Format.sprintf "while parsing %s, " s
  in
  match l with
    | [] -> ""
    | [l,c,s] ->
      Format.sprintf "@[%aat line %d, col %d@]" pp_s s l c
    | (l,c,s) :: tail ->
      Format.sprintf "@[%aat line %d, col %d@]@,%s" pp_s s l c (string_of_branch tail)

let () = Printexc.register_printer
    (function
      | ParseError (b,msg) ->
        Some (Format.sprintf "@[<v>%s@ %s@]" (msg()) (string_of_branch b))
      | _ -> None)

let const_ x () = x

let state_of_string str =
  let s = {
    str;
    i=0;
    lnum=1;
    cnum=1;
    branch=[];
    memo=MemoTbl.create 32;
  } in
  s

let is_done st = st.i = String.length st.str
let cur st = st.str.[st.i]

let fail_ ~err st msg =
  let b = (st.lnum, st.cnum, None) :: st.branch in
  err (ParseError (b, msg))

let next st ~ok ~err =
  if st.i = String.length st.str
  then fail_ ~err st (const_ "unexpected end of input")
  else (
    let c = st.str.[st.i] in
    st.i <- st.i + 1;
    if char_equal c '\n'
    then (st.lnum <- st.lnum + 1; st.cnum <- 1)
    else st.cnum <- st.cnum + 1;
    ok c
  )

let pos st = st.i, st.lnum, st.cnum

let backtrack st (i',l',c') =
  assert (0 <= i' && i' <= st.i);
  st.i <- i';
  st.lnum <- l';
  st.cnum <- c';
  ()

type 'a t = state -> ok:('a -> unit) -> err:(exn -> unit) -> unit

let return : 'a -> 'a t = fun x _st ~ok ~err:_ -> ok x
let pure = return
let (>|=) : 'a t -> ('a -> 'b) -> 'b t
  = fun p f st ~ok ~err -> p st ~ok:(fun x -> ok (f x)) ~err
let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  = fun p f st ~ok ~err -> p st ~ok:(fun x -> f x st ~ok ~err) ~err
let (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  = fun f x st ~ok ~err ->
    f st ~ok:(fun f' -> x st ~ok:(fun x' -> ok (f' x')) ~err) ~err
let (<* ) : 'a t -> _ t -> 'a t
  = fun x y st ~ok ~err ->
    x st ~ok:(fun res -> y st ~ok:(fun _ -> ok res) ~err) ~err
let ( *>) : _ t -> 'a t -> 'a t
  = fun x y st ~ok ~err ->
    x st ~ok:(fun _ -> y st ~ok ~err) ~err

let map f x = x >|= f
let map2 f x y = pure f <*> x <*> y
let map3 f x y z = pure f <*> x <*> y <*> z

let junk_ st = next st ~ok:ignore ~err:(fun _ -> assert false)

let eoi st ~ok ~err =
  if is_done st
  then ok ()
  else fail_ ~err st (const_ "expected EOI")

let fail msg st ~ok:_ ~err = fail_ ~err st (const_ msg)
let failf msg = Printf.ksprintf fail msg

let parsing s p st ~ok ~err =
  st.branch <- (st.lnum, st.cnum, Some s) :: st.branch;
  p st
    ~ok:(fun x -> st.branch <- List.tl st.branch; ok x)
    ~err:(fun e -> st.branch <- List.tl st.branch; err e)

let nop _ ~ok ~err:_ = ok()

let char c =
  let msg = Printf.sprintf "expected '%c'" c in
  fun st ~ok ~err ->
    next st
      ~ok:(fun c' -> if char_equal c c' then ok c else fail_ ~err st (const_ msg)) ~err

let char_if p st ~ok ~err =
  next st
    ~ok:(fun c ->
      if p c then ok c
      else fail_ ~err st (fun () -> Printf.sprintf "unexpected char '%c'" c)
    ) ~err

let chars_if p st ~ok ~err:_ =
  let i = st.i in
  let len = ref 0 in
  while not (is_done st) && p (cur st) do junk_ st; incr len done;
  ok (String.sub st.str i !len)

let chars1_if p st ~ok ~err =
  chars_if p st
    ~ok:(fun s ->
      if string_equal s ""
      then fail_ ~err st (const_ "unexpected sequence of chars")
      else ok s)
    ~err

let rec skip_chars p st ~ok ~err =
  if not (is_done st) && p (cur st) then (
    junk_ st;
    skip_chars p st ~ok ~err
  ) else ok()

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

let endline st ~ok ~err =
  next st
    ~ok:(function
      | '\n' as c -> ok c
      | _ -> fail_ ~err st (const_ "expected end-of-line"))
    ~err

let skip_space = skip_chars is_space
let skip_white = skip_chars is_white

let (<|>) : 'a t -> 'a t -> 'a t
  = fun x y st ~ok ~err ->
    let i = st.i in
    x st ~ok
      ~err:(fun e ->
        let j = st.i in
        if i=j then y st ~ok ~err (* try [y] *)
        else err e (* fail *)
      )

let try_ : 'a t -> 'a t
  = fun p st ~ok ~err ->
    let i = pos st in
    p st ~ok
      ~err:(fun e ->
        backtrack st i;
        err e)

let suspend f st ~ok ~err = f () st ~ok ~err

let (<?>) : 'a t -> string -> 'a t
  = fun x msg st ~ok ~err ->
    let i = st.i in
    x st ~ok
      ~err:(fun e ->
        if st.i = i
        then fail_ ~err st (fun () -> msg)
        else err e)

let string s st ~ok ~err =
  let rec check i =
    if i = String.length s then ok s
    else
      next st
        ~ok:(fun c ->
          if char_equal c s.[i]
          then check (i+1)
          else fail_ ~err st (fun () -> Printf.sprintf "expected \"%s\"" s))
        ~err
  in
  check 0

let rec many_rec : 'a t -> 'a list -> 'a list t = fun p acc st ~ok ~err ->
  if is_done st then ok(List.rev acc)
  else
    p st
      ~ok:(fun x ->
        let i = pos st in
        let acc = x :: acc in
        many_rec p acc st ~ok
          ~err:(fun _ ->
            backtrack st i;
            ok(List.rev acc))
      ) ~err

let many : 'a t -> 'a list t
  = fun p st ~ok ~err -> many_rec p [] st ~ok ~err

(*$R
  let p0 = skip_white *> U.int in
  let p = (skip_white *> char '(' *> many p0) <* try_ (skip_white <* char ')') in
  let printer =  CCFormat.(to_string @@ Dump.result  @@ Dump.list int) in
  assert_equal ~printer
    (Ok [1;2;3]) (parse_string p "(1 2 3)");
  assert_equal ~printer
    (Ok [1;2; -30; 4]) (parse_string p "( 1 2    -30 4 )")
  *)


let many1 : 'a t -> 'a list t =
  fun p st ~ok ~err ->
    p st ~ok:(fun x -> many_rec p [x] st ~ok ~err) ~err

let rec skip p st ~ok ~err =
  let i = pos st in
  p st
    ~ok:(fun _ -> skip p st ~ok ~err)
    ~err:(fun _ ->
      backtrack st i;
      ok()
    )

(* by (sep1 ~by p) *)
let rec sep_rec ~by p = try_ by *> sep1 ~by p

and sep1 ~by p =
  p >>= fun x ->
  (sep_rec ~by p >|= fun tl -> x::tl)
  <|> return [x]

let sep ~by p =
  (try_ p >>= fun x ->
   (sep_rec ~by p >|= fun tl -> x::tl)
   <|> return [x])
  <|> return []

let fix f =
  let rec p st ~ok ~err = f p st ~ok ~err in
  p

let memo (type a) (p:a t):a t =
  let id = !MemoTbl.id_ in
  incr MemoTbl.id_;
  let r = ref None in (* used for universal encoding *)
  fun st ~ok ~err ->
    let i = st.i in
    let (lazy tbl) = st.memo in
    try
      let f = MemoTbl.H.find tbl (i,id) in
      (* extract hidden value *)
      r := None;
      f ();
      begin match !r with
        | None -> assert false
        | Some (MemoTbl.Ok x) -> ok x
        | Some (MemoTbl.Fail e) -> err e
      end
    with Not_found ->
      (* parse, and save *)
      p st
        ~ok:(fun x ->
          MemoTbl.H.replace tbl (i,id) (fun () -> r := Some (MemoTbl.Ok x));
          ok x)
        ~err:(fun e ->
          MemoTbl.H.replace tbl (i,id) (fun () -> r := Some (MemoTbl.Fail e));
          err e)

let fix_memo f =
  let rec p =
    let p' = lazy (memo p) in
    fun st ~ok ~err -> f (Lazy.force p') st ~ok ~err
  in
  p

let get_lnum = fun st ~ok ~err:_ -> ok st.lnum
let get_cnum = fun st ~ok ~err:_ -> ok st.cnum
let get_pos = fun st ~ok ~err:_ -> ok (st.lnum, st.cnum)

let parse_exn p st =
  let res = ref None in
  p st ~ok:(fun x -> res := Some x) ~err:(fun e -> raise e);
  match !res with
    | None -> assert false
    | Some x -> x

let exn_to_err e = Error (Printexc.to_string e)

let parse p st =
  try Ok (parse_exn p st)
  with e -> exn_to_err e

let parse_string_exn p s = parse_exn p (state_of_string s)

let parse_string p s = parse p (state_of_string s)

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

let parse_file_exn p file =
  let ic = open_in file in
  let st = state_of_string (read_all_ ic) in
  try
    let res = parse_exn p st in
    close_in ic;
    res
  with e ->
    close_in_noerr ic;
    raise e

let parse_file p file =
  try Ok (parse_file_exn p file)
  with e -> exn_to_err e

module Infix = struct
  let (>|=) = (>|=)
  let (>>=) = (>>=)
  let (<*>) = (<*>)
  let (<* ) = (<* )
  let ( *>) = ( *>)
  let (<|>) = (<|>)
  let (<?>) = (<?>)
end

module U = struct
  let sep_ = sep

  let list ?(start="[") ?(stop="]") ?(sep=";") p =
    string start *> skip_white *>
      sep_ ~by:(skip_white *> string sep *> skip_white) p <*
      skip_white <* string stop

  let int =
    chars1_if (fun c -> is_num c || char_equal c '-')
    >>= fun s ->
    try return (int_of_string s)
    with Failure _ -> fail "expected an int"

  let prepend_str c s = String.make 1 c ^ s

  let word =
    map2 prepend_str (char_if is_alpha) (chars_if is_alpha_num)

  let pair ?(start="(") ?(stop=")") ?(sep=",") p1 p2 =
    string start *> skip_white *>
      p1 >>= fun x1 ->
    skip_white *> string sep *> skip_white *>
      p2 >>= fun x2 ->
    string stop *> return (x1,x2)

  let triple ?(start="(") ?(stop=")") ?(sep=",") p1 p2 p3 =
    string start *> skip_white *>
      p1 >>= fun x1 ->
    skip_white *> string sep *> skip_white *>
      p2 >>= fun x2 ->
    skip_white *> string sep *> skip_white *>
      p3 >>= fun x3 ->
    string stop *> return (x1,x2,x3)
end

include CCShimsMkLet_.Make(struct
    type nonrec 'a t = 'a t
    include Infix
    let monoid_product a1 a2 = pure (fun x y ->x,y) <*> a1 <*> a2
  end)
