(* This file is free software. See file "license" for more details. *)

(** {1 Very Simple Parser Combinators} *)

open CCShims_

module Memo_tbl = Hashtbl.Make (struct
  type t = int * int (* id of parser, position *)

  let equal ((a, b) : t) (c, d) = a = c && b = d
  let hash = Hashtbl.hash
end)

module Memo_state = struct
  (* table of closures, used to implement universal type *)
  type t = (unit -> unit) Memo_tbl.t

  (* unique ID for each parser *)
  let id_ = ref 0
end

(* state common to all parser instances *)
type common_state = {
  str: string;
  mutable line_offsets: int array option;
  mutable memo: Memo_state.t option;
}

type position = {
  pos_cs: common_state;
  pos_offset: int;
  mutable pos_lc: (int * int) option;
}

module Position = struct
  type t = position

  let compute_line_offsets_ (s : string) : int array =
    let lines = CCVector.create () in
    let i = ref 0 in
    CCVector.push lines 0;
    while !i < String.length s do
      match String.index_from s !i '\n' with
      | exception Not_found -> i := String.length s
      | j ->
        CCVector.push lines j;
        i := j + 1
    done;
    CCVector.to_array lines

  let line_offsets_ cs =
    match cs.line_offsets with
    | Some lines -> lines
    | None ->
      let lines = compute_line_offsets_ cs.str in
      cs.line_offsets <- Some lines;
      lines

  let int_cmp_ : int -> int -> int = compare

  (* TODO: use pos_cs.line_offsets *)
  (* actually re-compute line and column from the buffer *)
  let compute_line_and_col_ (cs : common_state) (off : int) : int * int =
    let offsets = line_offsets_ cs in
    assert (offsets.(0) = 0);
    match CCArray.bsearch ~cmp:int_cmp_ off offsets with
    | `At 0 -> 0, 0
    | `At n -> n - 1, off - offsets.(n - 1) - 1
    | `Just_after n -> n, off - offsets.(n)
    | `Empty -> assert false
    | `All_bigger -> assert false (* off >= 0, and offsets[0] == 0 *)
    | `All_lower ->
      let n = Array.length offsets - 1 in
      n, off - offsets.(n)

  let line_and_column self =
    match self.pos_lc with
    | Some tup -> tup
    | None ->
      let tup = compute_line_and_col_ self.pos_cs self.pos_offset in
      self.pos_lc <- Some tup;
      (* save *)
      tup

  let line self = fst (line_and_column self)
  let column self = snd (line_and_column self)

  let pp out self =
    let l, c = line_and_column self in
    Format.fprintf out "at line %d, column %d" l c
end

module Error = struct
  type t = { msg: unit -> string; pos: position }

  let position self = self.pos
  let line_and_column self = Position.line_and_column self.pos
  let msg self = self.msg ()

  let to_string self =
    let line, col = line_and_column self in
    Printf.sprintf "at line %d, char %d: %s" line col (self.msg ())

  let pp out self =
    let line, col = line_and_column self in
    Format.fprintf out "@[<hv>at line %d, char %d:@ %s@]" line col (self.msg ())
end

type +'a or_error = ('a, Error.t) result

type state = {
  cs: common_state;
  i: int; (* offset in [str] *)
  j: int; (* end pointer in [str], excluded. [len = j-i] *)
}
(** Purely functional state passed around *)
(* FIXME: replace memo with:
   [global : global_st ref]

   where:
   [type global = {
     mutable memo: Memo_state.t option;
     line_offsets: int CCVector.vector;
   }

   with line_offsets used to cache the offset where each line begins,
   and is computed lazily, to make {!Position.line_and_column}
   faster if called many times.
*)

let[@inline] char_equal (a : char) b = Stdlib.( = ) a b
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
  let s =
    {
      cs = { str; memo = None; line_offsets = None };
      i = 0;
      j = String.length str;
    }
  in
  s

let[@inline] is_done st = st.i >= st.j
let[@inline] cur st = st.cs.str.[st.i]

let pos_of_st_ st : position =
  { pos_cs = st.cs; pos_offset = st.i; pos_lc = None }

let mk_error_ st msg : Error.t = { Error.msg; pos = pos_of_st_ st }

(* consume one char, passing it to [ok]. *)
let consume_ st ~ok ~err =
  if is_done st then (
    let msg = const_str_ "unexpected end of input" in
    err (mk_error_ st msg)
  ) else (
    let c = st.cs.str.[st.i] in
    ok { st with i = st.i + 1 } c
  )

type 'a t = {
  run: 'b. state -> ok:(state -> 'a -> 'b) -> err:(Error.t -> 'b) -> 'b;
}
[@@unboxed]
(** Takes the input and two continuations:
    {ul
      {- [ok] to call with the result and new state when it's done}
      {- [err] to call when the parser met an error}
    }
*)

let return x : _ t = { run = (fun st ~ok ~err:_ -> ok st x) }
let pure = return

let map f (p : 'a t) : _ t =
  { run = (fun st ~ok ~err -> p.run st ~ok:(fun st x -> ok st (f x)) ~err) }

let bind f (p : 'a t) : _ t =
  {
    run =
      (fun st ~ok ~err ->
        p.run st
          ~ok:(fun st x ->
            let p2 = f x in
            p2.run st ~ok ~err)
          ~err);
  }

let ap (f : _ t) (a : _ t) : _ t =
  {
    run =
      (fun st ~ok ~err ->
        f.run st
          ~ok:(fun st f -> a.run st ~ok:(fun st x -> ok st (f x)) ~err)
          ~err);
  }

let ap_left (a : _ t) (b : _ t) : _ t =
  {
    run =
      (fun st ~ok ~err ->
        a.run st ~ok:(fun st x -> b.run st ~ok:(fun st _ -> ok st x) ~err) ~err);
  }

let ap_right (a : _ t) (b : _ t) : _ t =
  {
    run =
      (fun st ~ok ~err ->
        a.run st ~ok:(fun st _ -> b.run st ~ok:(fun st x -> ok st x) ~err) ~err);
  }

let or_ (p1 : 'a t) (p2 : 'a t) : _ t =
  {
    run = (fun st ~ok ~err -> p1.run st ~ok ~err:(fun _e -> p2.run st ~ok ~err));
  }

let both a b =
  {
    run =
      (fun st ~ok ~err ->
        a.run st
          ~ok:(fun st xa -> b.run st ~ok:(fun st xb -> ok st (xa, xb)) ~err)
          ~err);
  }

let set_error_message msg (p : 'a t) : _ t =
  {
    run =
      (fun st ~ok ~err ->
        p.run st ~ok ~err:(fun _e -> err (mk_error_ st (const_str_ msg))));
  }

module Infix = struct
  let[@inline] ( >|= ) p f = map f p
  let[@inline] ( >>= ) p f = bind f p
  let ( <*> ) = ap
  let ( <* ) = ap_left
  let ( *> ) = ap_right
  let ( <|> ) = or_
  let ( ||| ) = both
  let[@inline] ( <?> ) p msg = set_error_message msg p

  [@@@ifge 4.8]

  let ( let+ ) = ( >|= )
  let ( let* ) = ( >>= )
  let ( and+ ) = both
  let ( and* ) = ( and+ )

  [@@@endif]
end

include Infix

let map2 f x y = pure f <*> x <*> y
let map3 f x y z = pure f <*> x <*> y <*> z

let junk_ (st : state) : state =
  assert (st.i < st.j);
  { st with i = st.i + 1 }

let eoi =
  {
    run =
      (fun st ~ok ~err ->
        if is_done st then
          ok st ()
        else
          err (mk_error_ st (const_str_ "expected end of input")));
  }

let with_pos p : _ t =
  {
    run =
      (fun st ~ok ~err ->
        p.run st ~ok:(fun st' x -> ok st' (x, pos_of_st_ st)) ~err);
  }

let pos : _ t = { run = (fun st ~ok ~err:_ -> ok st (pos_of_st_ st)) }

(* a slice is just a state, which makes {!recurse} quite easy. *)
type slice = state

module Slice = struct
  type t = slice

  let length sl = sl.j - sl.i
  let is_empty sl = sl.i = sl.j
  let to_string sl = String.sub sl.cs.str sl.i (length sl)
end

let recurse slice p : _ t =
  {
    run =
      (fun _st ~ok ~err ->
        (* make sure these states are related. all slices share the
           same reference as the initial state they derive from. *)
        assert (CCShims_.Stdlib.(_st.cs == slice.cs));
        p.run slice ~ok ~err);
  }

let all =
  {
    run =
      (fun st ~ok ~err:_ ->
        if is_done st then
          ok st st
        else (
          let st_done = { st with i = st.j } in
          ok st_done st
        ));
  }

let all_str = all >|= Slice.to_string

let fail msg : _ t =
  { run = (fun st ~ok:_ ~err -> err (mk_error_ st (const_str_ msg))) }

let failf msg = Printf.ksprintf fail msg
let fail_lazy msg = { run = (fun st ~ok:_ ~err -> err (mk_error_ st msg)) }

let parsing what p =
  {
    run =
      (fun st ~ok ~err ->
        p.run st ~ok ~err:(fun e ->
            let msg () =
              Printf.sprintf "while parsing %s:\n%s" what (e.Error.msg ())
            in
            err { e with Error.msg }));
  }

let empty = { run = (fun st ~ok ~err:_ -> ok st ()) }
let nop = empty
let any_char = { run = (fun st ~ok ~err -> consume_ st ~ok ~err) }

let char c : _ t =
  {
    run =
      (fun st ~ok ~err ->
        consume_ st
          ~ok:(fun st c2 ->
            if char_equal c c2 then
              ok st c
            else (
              let msg () = Printf.sprintf "expected '%c', got '%c'" c c2 in
              err (mk_error_ st msg)
            ))
          ~err);
  }

let char_if ?descr p =
  {
    run =
      (fun st ~ok ~err ->
        consume_ st
          ~ok:(fun st c ->
            if p c then
              ok st c
            else (
              let msg () =
                let rest =
                  match descr with
                  | None -> ""
                  | Some d -> Printf.sprintf ", expected %s" d
                in
                Printf.sprintf "unexpected char '%c'%s" c rest
              in
              err (mk_error_ st msg)
            ))
          ~err);
  }

let take_if p : slice t =
  {
    run =
      (fun st ~ok ~err:_ ->
        let i = ref st.i in
        while
          let st = { st with i = !i } in
          (not (is_done st)) && p (cur st)
        do
          incr i
        done;
        ok { st with i = !i } { st with j = !i });
  }

let take1_if ?descr p =
  take_if p >>= fun sl ->
  if Slice.is_empty sl then (
    let msg () =
      let what =
        match descr with
        | None -> ""
        | Some d -> Printf.sprintf " for %s" d
      in
      Printf.sprintf "expected non-empty sequence of chars%s" what
    in
    fail_lazy msg
  ) else
    return sl

let chars_if p = take_if p >|= Slice.to_string

let chars1_if ?descr p =
  {
    run =
      (fun st ~ok ~err ->
        (chars_if p).run st
          ~ok:(fun st s ->
            if string_equal s "" then (
              let msg () =
                let what =
                  match descr with
                  | None -> ""
                  | Some d -> Printf.sprintf " for %s" d
                in
                Printf.sprintf "expected non-empty sequence of chars%s" what
              in
              err (mk_error_ st msg)
            ) else
              ok st s)
          ~err);
  }

exception Fold_fail of state * string

let chars_fold ~f acc0 =
  {
    run =
      (fun st ~ok ~err ->
        let i0 = st.i in
        let i = ref i0 in
        let acc = ref acc0 in
        let continue = ref true in
        try
          while !continue do
            let st = { st with i = !i } in
            if is_done st then
              continue := false
            else (
              let c = cur st in
              match f !acc c with
              | `Continue acc' ->
                incr i;
                acc := acc'
              | `Stop a ->
                acc := a;
                continue := false
              | `Consume_and_stop a ->
                acc := a;
                incr i;
                continue := false
              | `Fail msg -> raise (Fold_fail (st, msg))
            )
          done;
          ok { st with i = !i } (!acc, { st with j = !i })
        with Fold_fail (st, msg) -> err (mk_error_ st (const_str_ msg)));
  }

let chars_fold_transduce ~f acc0 =
  {
    run =
      (fun st ~ok ~err ->
        let i0 = st.i in
        let i = ref i0 in
        let acc = ref acc0 in
        let continue = ref true in
        let buf = Buffer.create 16 in
        try
          while !continue do
            let st = { st with i = !i } in
            if is_done st then
              continue := false
            else (
              let c = cur st in
              match f !acc c with
              | `Continue acc' ->
                incr i;
                acc := acc'
              | `Yield (acc', c') ->
                incr i;
                acc := acc';
                Buffer.add_char buf c'
              | `Stop -> continue := false
              | `Consume_and_stop ->
                incr i;
                continue := false
              | `Fail msg -> raise (Fold_fail (st, msg))
            )
          done;
          ok { st with i = !i } (!acc, Buffer.contents buf)
        with Fold_fail (st, msg) -> err (mk_error_ st (const_str_ msg)));
  }

let skip_chars p : _ t =
  let rec self =
    {
      run =
        (fun st ~ok ~err ->
          if (not (is_done st)) && p (cur st) then (
            let st = junk_ st in
            self.run st ~ok ~err
          ) else
            ok st ());
    }
  in
  self

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_num = function
  | '0' .. '9' -> true
  | _ -> false

let is_alpha_num = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false

let is_white = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false

let space = char_if is_space
let white = char_if is_white

let endline =
  char_if ~descr:"end-of-line ('\\n')" (function
    | '\n' -> true
    | _ -> false)

let skip_space = skip_chars is_space
let skip_white = skip_chars is_white

let try_or p1 ~f ~else_:p2 =
  {
    run =
      (fun st ~ok ~err ->
        p1.run st
          ~ok:(fun st x -> (f x).run st ~ok ~err)
          ~err:(fun _ -> p2.run st ~ok ~err));
  }

let try_or_l ?(msg = "try_or_l ran out of options") ?else_ l : _ t =
  {
    run =
      (fun st ~ok ~err ->
        let rec loop = function
          | (test, p) :: tl ->
            test.run st
              ~ok:(fun _ _ -> p.run st ~ok ~err) (* commit *)
              ~err:(fun _ -> loop tl)
          | [] ->
            (match else_ with
            | None -> err (mk_error_ st (const_str_ msg))
            | Some p -> p.run st ~ok ~err)
        in
        loop l);
  }

let suspend f =
  {
    run =
      (fun st ~ok ~err ->
        let p = f () in
        p.run st ~ok ~err);
  }

(* read [len] chars at once *)
let take len : slice t =
  {
    run =
      (fun st ~ok ~err ->
        if st.i + len <= st.j then (
          let slice = { st with j = st.i + len } in
          let st = { st with i = st.i + len } in
          ok st slice
        ) else (
          let msg () =
            Printf.sprintf "expected to be able to consume %d chars" len
          in
          err (mk_error_ st msg)
        ));
  }

let any_char_n len : _ t = take len >|= Slice.to_string

let exact s =
  {
    run =
      (fun st ~ok ~err ->
        (* parse a string of length [String.length s] and compare with [s] *)
        (any_char_n (String.length s)).run st
          ~ok:(fun st s2 ->
            if string_equal s s2 then
              ok st s
            else (
              let msg () = Printf.sprintf "expected %S, got %S" s s2 in
              err (mk_error_ st msg)
            ))
          ~err);
  }

let string = exact

let fix f =
  let rec self =
    { run = (fun st ~ok ~err -> (Lazy.force f_self).run st ~ok ~err) }
  and f_self = lazy (f self) in
  self

let try_ p = p

let try_opt p : _ t =
  {
    run =
      (fun st ~ok ~err:_ ->
        p.run st ~ok:(fun st x -> ok st (Some x)) ~err:(fun _ -> ok st None));
  }

let optional p : _ t =
  {
    run =
      (fun st ~ok ~err:_ ->
        p.run st ~ok:(fun st _x -> ok st ()) ~err:(fun _ -> ok st ()));
  }

let many_until ~until p : _ t =
  fix (fun self ->
      try_or until
        ~f:(fun _ -> pure [])
        ~else_:
          ( p >>= fun x ->
            self >|= fun l -> x :: l ))

let many p : _ t =
  fix (fun self ->
      try_or p ~f:(fun x -> self >|= fun tl -> x :: tl) ~else_:(pure []))

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

let many1 p =
  p >>= fun x ->
  many p >|= fun l -> x :: l

(* skip can be made efficient by not allocating intermediate parsers *)
let skip p : _ t =
  let rec self =
    {
      run =
        (fun st ~ok ~err ->
          p.run st
            ~ok:(fun st _ -> self.run st ~ok ~err)
            ~err:(fun _ -> ok st ()));
    }
  in
  self

let sep_until ~until ~by p =
  let rec read_p =
    lazy
      ( p >>= fun x ->
        until *> pure [ x ] <|> by *> (Lazy.force read_p >|= fun tl -> x :: tl)
      )
  in
  until *> pure [] <|> Lazy.force read_p

let sep ~by p =
  let rec read_p =
    lazy
      (try_or p
         ~f:(fun x ->
           eoi *> pure [ x ]
           <|> try_or by
                 ~f:(fun _ -> Lazy.force read_p >|= fun tl -> x :: tl)
                 ~else_:(pure [ x ]))
         ~else_:(pure []))
  in
  Lazy.force read_p

let sep1 ~by p =
  p >>= fun x ->
  sep ~by p >|= fun tl -> x :: tl

let lookahead p : _ t =
  {
    run =
      (fun st ~ok ~err ->
        p.run st ~ok:(fun _st x -> ok st x) (* discard p's new state *) ~err);
  }

let lookahead_ignore p : _ t =
  { run = (fun st ~ok ~err -> p.run st ~ok:(fun _st _x -> ok st ()) ~err) }

let set_current_slice sl : _ t =
  {
    run =
      (fun _st ~ok ~err:_ ->
        assert (CCShims_.Stdlib.(_st.cs == sl.cs));
        ok sl ())
      (* jump to slice *);
  }

let split_1 ~on_char : _ t =
  {
    run =
      (fun st ~ok ~err:_ ->
        if st.i >= st.j then
          ok st (st, None)
        else (
          match String.index_from st.cs.str st.i on_char with
          | j ->
            let x = { st with j } in
            let y = { st with i = min st.j (j + 1) } in
            let st_done = { st with i = st.j } in
            (* empty *)
            ok st_done (x, Some y)
          | exception Not_found ->
            let st_done = { st with i = st.j } in
            (* empty *)
            ok st_done (st, None)
        ));
  }

let split_list_at_most ~on_char n : slice list t =
  let rec loop acc n =
    if n <= 0 then
      (* add the rest to [acc] *)
      all >|= fun rest ->
      let acc = rest :: acc in
      List.rev acc
    else
      try_or eoi ~f:(fun _ -> return (List.rev acc)) ~else_:(parse_1 acc n)
  and parse_1 acc n =
    split_1 ~on_char >>= fun (sl1, rest) ->
    let acc = sl1 :: acc in
    match rest with
    | None -> return (List.rev acc)
    | Some rest -> recurse rest (loop acc (n - 1))
  in
  loop [] n

let split_list ~on_char : _ t = split_list_at_most ~on_char max_int

let split_2 ~on_char : _ t =
  split_list_at_most ~on_char 3 >>= function
  | [ a; b ] -> return (a, b)
  | _ -> fail "split_2: expected 2 fields exactly"

let split_3 ~on_char : _ t =
  split_list_at_most ~on_char 4 >>= function
  | [ a; b; c ] -> return (a, b, c)
  | _ -> fail "split_3: expected 3 fields exactly"

let split_4 ~on_char : _ t =
  split_list_at_most ~on_char 5 >>= function
  | [ a; b; c; d ] -> return (a, b, c, d)
  | _ -> fail "split_4: expected 4 fields exactly"

let split_list ~on_char : slice list t =
  let rec loop acc =
    try_or eoi ~f:(fun _ -> return (List.rev acc)) ~else_:(parse_1 acc)
  and parse_1 acc =
    split_1 ~on_char >>= fun (sl1, rest) ->
    let acc = sl1 :: acc in
    match rest with
    | None -> return (List.rev acc)
    | Some rest -> recurse rest (loop acc)
  in
  loop []

let each_split ~on_char p : 'a list t =
  let rec loop acc =
    split_1 ~on_char >>= fun (sl1, rest) ->
    (* parse [sl1] with [p] *)
    recurse sl1 p >>= fun x ->
    let acc = x :: acc in
    match rest with
    | None -> return (List.rev acc)
    | Some rest -> recurse rest (loop acc)
  in
  loop []

let line : slice t =
  split_1 ~on_char:'\n' >>= fun (sl, rest) ->
  match rest with
  | None -> return sl
  | Some rest -> set_current_slice rest >|= fun () -> sl

let line_str = line >|= Slice.to_string
let each_line p : _ t = each_split ~on_char:'\n' p

let memo (type a) (p : a t) : a t =
  let id = !Memo_state.id_ in
  incr Memo_state.id_;
  let r = ref None in

  (* used for universal encoding *)
  {
    run =
      (fun st ~ok ~err ->
        let tbl =
          match st.cs.memo with
          | Some t -> t
          | None ->
            let tbl = Memo_tbl.create 32 in
            st.cs.memo <- Some tbl;
            tbl
        in

        match
          r := None;
          let f = Memo_tbl.find tbl (st.i, id) in
          f ();
          !r
        with
        | None -> assert false
        | Some (Ok (st, x)) -> ok st x
        | Some (Error e) -> err e
        | exception Not_found ->
          (* parse, and save *)
          p.run st
            ~ok:(fun st' x ->
              Memo_tbl.replace tbl (st.i, id) (fun () ->
                  r := Some (Ok (st', x)));
              ok st' x)
            ~err:(fun e ->
              Memo_tbl.replace tbl (st.i, id) (fun () -> r := Some (Error e));
              err e));
  }

let fix_memo f =
  let rec p = { run = (fun st ~ok ~err -> (Lazy.force p').run st ~ok ~err) }
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
  p.run (state_of_string s) ~ok:(fun _st x -> Ok x) ~err:(fun e -> Error e)

let parse_string p s = parse_string_e p s |> stringify_result

let read_all_ ic =
  let buf = Buffer.create 1024 in
  (try
     while true do
       let line = input_line ic in
       Buffer.add_string buf line;
       Buffer.add_char buf '\n'
     done;
     assert false
   with End_of_file -> ());
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

  let list ?(start = "[") ?(stop = "]") ?(sep = ";") p =
    string start *> skip_white
    *> sep_until
         ~until:(skip_white <* string stop)
         ~by:(skip_white *> string sep *> skip_white)
         p

  let int =
    skip_white
    *> chars1_if ~descr:"integer" (fun c -> is_num c || char_equal c '-')
    >>= fun s ->
    try return (int_of_string s) with Failure _ -> fail "expected an int"

  let in_paren (p : 'a t) : 'a t =
    skip_white *> (char '(' *> skip_white *> p <* skip_white <* char ')')

  let in_parens_opt (p : 'a t) : 'a t =
    fix (fun self ->
        skip_white
        *> try_or (char '(')
             ~f:(fun _ -> skip_white *> self <* skip_white <* char ')')
             ~else_:p)

  let option p =
    skip_white
    *> try_or (string "Some")
         ~f:(fun _ -> skip_white *> p >|= fun x -> Some x)
         ~else_:(string "None" *> return None)

  let hexa_int =
    (exact "0x" <|> return "")
    *> ( chars1_if (function
           | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
           | _ -> false)
       >|= fun s ->
         let i = ref 0 in
         String.iter
           (fun c ->
             let n =
               match c with
               | '0' .. '9' -> Char.code c - Char.code '0'
               | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
               | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
               | _ -> assert false
             in
             i := (!i * 16) + n)
           s;
         !i )

  let prepend_str c s = String.make 1 c ^ s
  let word = map2 prepend_str (char_if is_alpha) (chars_if is_alpha_num)

  let bool =
    skip_white
    *> (string "true" *> return true <|> string "false" *> return false)

  let pair ?(start = "(") ?(stop = ")") ?(sep = ",") p1 p2 =
    skip_white *> string start *> skip_white *> p1 >>= fun x1 ->
    skip_white *> string sep *> skip_white *> p2 >>= fun x2 ->
    skip_white *> string stop *> return (x1, x2)

  let triple ?(start = "(") ?(stop = ")") ?(sep = ",") p1 p2 p3 =
    string start *> skip_white *> p1 >>= fun x1 ->
    skip_white *> string sep *> skip_white *> p2 >>= fun x2 ->
    skip_white *> string sep *> skip_white *> p3 >>= fun x3 ->
    string stop *> return (x1, x2, x3)
end

module Debug_ = struct
  let trace_fail name p =
    {
      run =
        (fun st ~ok ~err ->
          p.run st ~ok ~err:(fun e ->
              Printf.eprintf "trace %s: fail with %s\n%!" name
                (Error.to_string e);
              err e));
    }

  let trace_ ~both name ~print p =
    {
      run =
        (fun st ~ok ~err ->
          p.run st
            ~ok:(fun st x ->
              Printf.eprintf "trace %s: parsed %s\n%!" name (print x);
              ok st x)
            ~err:(fun e ->
              if both then
                Printf.eprintf "trace %s: fail with %s\n%!" name
                  (Error.to_string e);
              err e));
    }

  let trace_success name ~print p = trace_ ~both:false name ~print p
  let trace_success_or_fail name ~print p = trace_ ~both:true name ~print p
end
