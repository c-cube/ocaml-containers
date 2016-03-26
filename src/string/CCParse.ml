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

(** {1 Very Simple Parser Combinators} *)

type 'a or_error = [`Ok of 'a | `Error of string]

type line_num = int
type col_num = int

module H = Hashtbl.Make(struct
  type t = int * int  (* id of parser, position *)
  let equal ((a,b):t)(c,d) = a=c && b=d
  let hash = Hashtbl.hash
end)

type memo_ = (unit -> unit) H.t lazy_t

type input = {
  is_done : unit -> bool; (** End of input? *)
  cur : unit -> char;  (** Current char *)
  next : unit -> char; (** if not {!is_done}, move to next char *)
  pos : unit -> int;   (** Current pos *)
  lnum : unit -> line_num; (** Line number @since 0.13 *)
  cnum : unit -> col_num;  (** Column number @since 0.13 *)
  memo : memo_; (** Memoization table, if any *)
  backtrack : int -> unit;  (** Restore to previous pos *)
  sub : int -> int -> string; (** Extract slice from [pos] with [len] *)
}

exception ParseError of line_num * col_num * (unit -> string)

(*$inject
  module T = struct
    type tree = L of int | N of tree * tree
  end
  open T

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

  let errpptree = function
    | `Ok x -> "Ok " ^ pptree x
    | `Error s -> "Error " ^ s
*)

(*$= & ~printer:errpptree
  (`Ok (N (L 1, N (L 2, L 3)))) \
    (parse_string ~p:ptree "(1 (2 3))" )
  (`Ok (N (N (L 1, L 2), N (L 3, N (L 4, L 5))))) \
    (parse_string ~p:ptree "((1 2) (3 (4 5)))" )
  (`Ok (N (L 1, N (L 2, L 3)))) \
    (parse_string ~p:ptree' "(1 (2 3))" )
  (`Ok (N (N (L 1, L 2), N (L 3, N (L 4, L 5))))) \
    (parse_string ~p:ptree' "((1 2) (3 (4 5)))" )
*)

(*$R
  let p = U.list ~sep:"," U.word in
  let printer = function
    | `Ok l -> "Ok " ^ CCPrint.to_string (CCList.pp CCString.pp) l
    | `Error s -> "Error " ^ s
  in
  assert_equal ~printer
    (`Ok ["abc"; "de"; "hello"; "world"])
    (parse_string ~p "[abc , de, hello ,world  ]");
 *)

(*$R
  let test n =
    let p = CCParse.(U.list ~sep:"," U.int) in

    let l = CCList.(1 -- n) in
    let l_printed =
      CCFormat.to_string (CCList.print ~sep:"," ~start:"[" ~stop:"]" CCInt.print) l in

    let l' = CCParse.parse_string_exn ~p l_printed in

    assert_equal ~printer:Q.Print.(list int) l l'
  in
  test 100_000;
  test 400_000;

*)

(* test with a temporary file *)
(*$R
  let test n =
    let p = CCParse.(U.list ~sep:"," U.int) in

    let l = CCList.(1 -- n) in
    let l' =
      CCIO.File.with_temp ~temp_dir:"/tmp/"
       ~prefix:"containers_test" ~suffix:""
       (fun name ->
          (* write test into file *)
         CCIO.with_out name
          (fun oc ->
            let fmt = Format.formatter_of_out_channel oc in
            Format.fprintf fmt "@[%a@]@."
              (CCList.print ~sep:"," ~start:"[" ~stop:"]" CCInt.print) l);
         (* parse it back *)
         CCParse.parse_file_exn ~size:1024 ~file:name ~p)
    in
    assert_equal ~printer:Q.Print.(list int) l l'
  in
  test 100_000;
  test 400_000;
*)

let const_ x () = x

let input_of_string s =
  let i = ref 0 in
  let line = ref 1 in (* line *)
  let col = ref 1 in (* column *)
  { is_done=(fun () -> !i = String.length s);
    cur=(fun () -> s.[!i]);
    next=(fun () ->
        if !i = String.length s
        then raise (ParseError (!line, !col, const_ "unexpected EOI"))
        else (
          let c = s.[!i] in
          incr i;
          if c='\n' then (incr line; col:=1) else incr col;
          c
        )
    );
    lnum=(fun () -> !line);
    cnum=(fun () -> !col);
    memo=lazy (H.create 32);
    pos=(fun () -> !i);
    backtrack=(fun j -> assert (0 <= j && j <= !i); i := j);
    sub=(fun j len -> assert (j + len <= !i); String.sub s j len);
  }

let input_of_chan ?(size=1024) ic =
  assert (size > 0);
  let b = ref (Bytes.make size ' ') in
  let n = ref 0 in  (* length of buffer *)
  let i = ref 0 in  (* current index in buffer *)
  let line = ref 1 in
  let col = ref 1 in
  let exhausted = ref false in (* input fully read? *)
  let eoi() = raise (ParseError (!line, !col, const_ "unexpected EOI")) in
  (* read a chunk of input *)
  let read_more () =
    assert (not !exhausted);
    (* resize *)
    if Bytes.length !b - !n < size then (
      let b' = Bytes.make (Bytes.length !b + 2 * size) ' ' in
      Bytes.blit !b 0 b' 0 !n;
      b := b';
    );
    let len = input ic !b !n size in
    exhausted := len = 0;
    n := !n + len
  in
  (* read next char *)
  let next() =
    if !exhausted && !i = !n then eoi();
    let c = Bytes.get !b !i in
    incr i;
    if c='\n' then (incr line; col := 1) else incr col;
    if !i = !n then (
      read_more();
      if !exhausted then eoi();
      assert (!i < !n);
    );
    c
  and is_done () = !exhausted && !i = !n in
  (* fetch first chars *)
  read_more();
  { is_done=(fun () -> !exhausted && !i = !n);
    cur=(fun () -> assert (not (is_done())); Bytes.get !b !i);
    next;
    pos=(fun() -> !i);
    lnum=(fun () -> !line);
    cnum=(fun () -> !col);
    memo=lazy (H.create 32);
    backtrack=(fun j -> assert (0 <= j && j <= !i); i:=j);
    sub=(fun j len -> assert (j + len <= !i); Bytes.sub_string !b j len);
  }

type 'a t = input -> ok:('a -> unit) -> err:(exn -> unit) -> unit

let return : 'a -> 'a t = fun x _st ~ok ~err:_ -> ok x
let pure = return
let (>|=) : 'a t -> ('a -> 'b) -> 'b t
  = fun p f st ~ok ~err -> p st ~err ~ok:(fun x -> ok (f x))
let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  = fun p f st ~ok ~err -> p st ~err ~ok:(fun x -> f x st ~err ~ok)
let (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  = fun f x st ~ok ~err ->
      f st ~err ~ok:(fun f' -> x st ~err ~ok:(fun x' -> ok (f' x')))
let (<* ) : 'a t -> _ t -> 'a t
  = fun x y st ~ok ~err ->
    x st ~err ~ok:(fun res -> y st ~err ~ok:(fun _ -> ok res))
let ( *>) : _ t -> 'a t -> 'a t
  = fun x y st ~ok ~err ->
    x st ~err ~ok:(fun _ -> y st ~err ~ok)

let junk_ st = ignore (st.next ())
let pf = Printf.sprintf
let fail_ ~err st msg = err (ParseError (st.lnum(), st.cnum(), msg))

let eoi st ~ok ~err =
  if st.is_done()
  then ok ()
  else fail_ ~err st (const_ "expected EOI")

let fail msg st ~ok:_ ~err = fail_ ~err st (const_ msg)
let nop _ ~ok ~err:_ = ok()

let char c =
  let msg = pf "expected '%c'" c in
  fun st ~ok ~err -> if st.next () = c then ok c else fail_ ~err st (const_ msg)

let char_if p st ~ok ~err =
  let c = st.next () in
  if p c then ok c else fail_ ~err st (fun () -> pf "unexpected char '%c'" c)

let chars_if p st ~ok ~err:_ =
  let i = st.pos () in
  let len = ref 0 in
  while not (st.is_done ()) && p (st.cur ()) do junk_ st; incr len done;
  ok (st.sub i !len)

let chars1_if p st ~ok ~err =
  chars_if p st ~err
    ~ok:(fun s ->
      if s = "" then fail_ ~err st (const_ "unexpected sequence of chars");
      ok s
    )

let rec skip_chars p st ~ok ~err =
  if not (st.is_done ()) && p (st.cur ()) then (
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
let (~~~) p c = not (p c)
let (|||) p1 p2 c = p1 c || p2 c
let (&&&) p1 p2 c = p1 c && p2 c

let endline = char '\n'
let space = char_if is_space
let white = char_if is_white

let skip_space = skip_chars is_space
let skip_white = skip_chars is_white

(* XXX: combine errors? *)

let (<|>) : 'a t -> 'a t -> 'a t
  = fun x y st ~ok ~err ->
    let i = st.pos () in
    x st ~ok
      ~err:(fun _ ->
        st.backtrack i; (* restore pos *)
        y st ~ok ~err
      )

let string s st ~ok ~err =
  let rec check i =
    i = String.length s ||
    (s.[i] = st.next () && check (i+1))
  in
  if check 0 then ok s else fail_ ~err st (fun () -> pf "expected \"%s\"" s)

let rec many_rec : 'a t -> 'a list -> 'a list t = fun p acc st ~ok ~err ->
  if st.is_done () then ok(List.rev acc)
  else
    let i = st.pos () in
    p st ~err
      ~ok:(fun x ->
        many_rec p (x :: acc) st ~ok
          ~err:(fun _ ->
            st.backtrack i;
            ok(List.rev acc)
          )
      )

let many : 'a t -> 'a list t
  = fun p st ~ok ~err -> many_rec p [] st ~ok ~err

let many1 : 'a t -> 'a list t =
  fun p st ~ok ~err ->
    p st ~err ~ok:(fun x -> many_rec p [x] st ~err ~ok)

let rec skip p st ~ok ~err =
  let i = st.pos () in
  p st
    ~ok:(fun _ -> skip p st ~ok ~err)
    ~err:(fun _ ->
      st.backtrack i;
      ok()
    )

let rec sep1 ~by p =
  p >>= fun x ->
  let cont = by *> sep ~by p >|= fun tl -> x :: tl in
  cont <|> return [x]
and sep ~by p =
  sep1 ~by p <|> return []

module MemoTbl = struct
  (* table of closures, used to implement universal type *)
  type t = memo_

  let create n = lazy (H.create n)

  (* unique ID for each parser *)
  let id_ = ref 0

  type 'a res =
    | Fail of exn
    | Ok of 'a
end

let fix f =
  let rec p st ~ok ~err = f p st ~ok ~err in
  p

let memo (type a) (p:a t):a t =
  let id = !MemoTbl.id_ in
  incr MemoTbl.id_;
  let r = ref None in (* used for universal encoding *)
  fun input ~ok ~err ->
    let i = input.pos () in
    let (lazy tbl) = input.memo in
    try
      let f = H.find tbl (i, id) in
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
      p input
        ~err:(fun e ->
          H.replace tbl (i,id) (fun () -> r := Some (MemoTbl.Fail e));
          err e
        )
        ~ok:(fun x ->
          H.replace tbl (i,id) (fun () -> r := Some (MemoTbl.Ok x));
          ok x
        )

let fix_memo f =
  let rec p =
    let p' = lazy (memo p) in
    fun st ~ok ~err -> f (Lazy.force p') st ~ok ~err
  in
  p

let parse_exn ~input ~p =
  let res = ref None in
  p input ~ok:(fun x -> res := Some x) ~err:(fun e -> raise e);
  match !res with
    | None -> failwith "no input returned by parser"
    | Some x -> x

let parse ~input ~p =
  try `Ok (parse_exn ~input ~p)
  with ParseError (lnum, cnum, msg) ->
    `Error (Printf.sprintf "at line %d, column %d: error, %s" lnum cnum (msg ()))

let parse_string s ~p = parse ~input:(input_of_string s) ~p
let parse_string_exn s ~p = parse_exn ~input:(input_of_string s) ~p

let parse_file_exn ?size ~file ~p =
  let ic = open_in file in
  let input = input_of_chan ?size ic in
  try
    let res = parse_exn ~input ~p in
    close_in ic;
    res
  with e ->
    close_in ic;
    raise e

let parse_file ?size ~file ~p =
  try
    `Ok (parse_file_exn ?size ~file ~p)
  with
  | ParseError (lnum, cnum, msg) ->
    `Error (Printf.sprintf "at line %d, column %d: error, %s" lnum cnum (msg ()))
  | Sys_error s ->
    `Error (Printf.sprintf "error while reading %s: %s" file s)

module U = struct
  let sep_ = sep

  let list ?(start="[") ?(stop="]") ?(sep=";") p =
    string start *> skip_white *>
    sep_ ~by:(skip_white *> string sep *> skip_white) p <*
    skip_white <* string stop

  let int =
    chars1_if (is_num ||| (=) '-')
    >>= fun s ->
    try return (int_of_string s)
    with Failure _ -> fail "expected an int"

  let map f x = x >|= f
  let map2 f x y = pure f <*> x <*> y
  let map3 f x y z = pure f <*> x <*> y <*> z

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
