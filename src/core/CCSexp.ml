(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Simple S-expression parsing/printing} *)

open CCShims_

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

module type SEXP = CCSexp_intf.SEXP
module type BASIC_SEXP = CCSexp_intf.BASIC_SEXP
module type S = CCSexp_intf.S
module type S0 = CCSexp_intf.S0

let equal_string (a : string) b = Stdlib.(=) a b
let compare_string (a : string) b = Stdlib.compare a b

module MakeBasic(Sexp : BASIC_SEXP) = struct
  type t = Sexp.t
  type sexp = t

  let atom = Sexp.atom
  let list = Sexp.list
  let of_int x = Sexp.atom (string_of_int x)
  let of_float x = Sexp.atom (string_of_float x)
  let of_bool x = Sexp.atom (string_of_bool x)
  let of_unit = Sexp.list []
  let of_list l = Sexp.list l
  let of_rev_list l = Sexp.list (List.rev l)
  let of_pair (x,y) = Sexp.list [x;y]
  let of_triple (x,y,z) = Sexp.list [x;y;z]
  let of_quad (x,y,z,u) = Sexp.list [x;y;z;u]

  let of_variant name args = Sexp.list (Sexp.atom name :: args)
  let of_field name t = Sexp.list [Sexp.atom name; t]
  let of_record l =
    Sexp.list (List.map (fun (n,x) -> of_field n x) l)
end

let _with_in filename f =
  let ic = open_in filename in
  try
    let x = f ic in
    close_in ic;
    x
  with e ->
    close_in ic;
    Error (Printexc.to_string e)

let _with_out filename f =
  let oc = open_out filename in
  try
    let x = f oc in
    close_out oc;
    x
  with e ->
    close_out oc;
    raise e

module Make(Sexp : SEXP) = struct
  include MakeBasic(Sexp)

  (** {3 Printing} *)

  (* shall we escape the string because of one of its chars? *)
  let _must_escape s =
    try
      for i = 0 to String.length s - 1 do
        let c = String.unsafe_get s i in
        match c with
          | ' ' | ')' | '(' | '"' | ';' | '\\' | '\n' | '\t' | '\r' -> raise Exit
          | _ when Char.code c > 127 -> raise Exit  (* non-ascii *)
          | _ -> ()
      done;
      false
    with Exit -> true

  (* empty atoms must be escaped *)
  let _must_escape s = String.length s = 0 || _must_escape s

  let rec to_buf b t =
    Sexp.match_ t
      ~atom:(fun s ->
          if _must_escape s then Printf.bprintf b "\"%s\"" (String.escaped s)
          else Buffer.add_string b s)
      ~list:(function
          | [] -> Buffer.add_string b "()"
          | [x] -> Printf.bprintf b "(%a)" to_buf x
          | l ->
            Buffer.add_char b '(';
            List.iteri
              (fun i t' -> (if i > 0 then Buffer.add_char b ' '; to_buf b t'))
              l;
            Buffer.add_char b ')')

  let to_string t =
    let b = Buffer.create 128 in
    to_buf b t;
    Buffer.contents b

  let rec pp fmt t =
    Sexp.match_ t
      ~atom:(fun s ->
          if _must_escape s then Format.fprintf fmt "\"%s\"" (String.escaped s)
          else Format.pp_print_string fmt s)
      ~list:(function
          | [] -> Format.pp_print_string fmt "()"
          | [x] -> Format.fprintf fmt "@[<hov2>(%a)@]" pp x
          | l ->
            Format.fprintf fmt "@[<hov1>(";
            List.iteri
              (fun i t' -> (if i > 0 then Format.fprintf fmt "@ "; pp fmt t'))
              l;
            Format.fprintf fmt ")@]")

  let rec pp_noindent fmt t =
    Sexp.match_ t
      ~atom:(fun s ->
    if _must_escape s then Format.fprintf fmt "\"%s\"" (String.escaped s)
    else Format.pp_print_string fmt s)
      ~list:(function
          | [] -> Format.pp_print_string fmt "()"
          | [x] -> Format.fprintf fmt "(%a)" pp_noindent x
          | l ->
            Format.pp_print_char fmt '(';
            List.iteri
              (fun i t' -> (if i > 0 then Format.pp_print_char fmt ' '; pp_noindent fmt t'))
              l;
            Format.pp_print_char fmt ')')

  let to_chan oc t =
    let fmt = Format.formatter_of_out_channel oc in
    pp fmt t;
    Format.pp_print_flush fmt ()

  let to_file_iter filename seq =
    _with_out filename
      (fun oc ->
         seq (fun t -> to_chan oc t; output_char oc '\n')
      )

  let to_file filename t = to_file_iter filename (fun k -> k t)

  (** {2 Parsing} *)

  (** A parser of ['a] can return [Yield x] when it parsed a value,
      or [Fail e] when a parse error was encountered, or
      [End] if the input was empty *)
  type 'a parse_result =
    | Yield of 'a
    | Fail of string
    | End

  module Decoder = struct
    module L = CCSexp_lex

    type t = {
      buf: Lexing.lexbuf;
      mutable cur_tok: L.token option; (* current token *)
    }

    let cur (t:t): L.token = match t.cur_tok with
      | Some L.EOI -> assert false
      | Some t -> t
      | None ->
        (* fetch token *)
        let tok = L.token t.buf in
        t.cur_tok <- Some tok;
        tok

    let junk t = t.cur_tok <- None

    let of_lexbuf buf = {
      buf;
      cur_tok=None;
    }

    exception E_end
    exception E_error of int * int * string

    let pair_of_pos_ p =
      let open Lexing in
      p.pos_lnum, p.pos_cnum - p.pos_bol

    let error_ lexbuf msg =
      let start = Lexing.lexeme_start_p lexbuf in
      let line, col = pair_of_pos_ start in
      raise (E_error (line,col,msg))

    let next (t:t) =
      let open Lexing in
      let rec expr () = match cur t with
        | L.EOI -> raise E_end
        | L.SEXP_COMMENT ->
          junk t;
          let _u = expr() in (* discard next sexp *)
          expr()
        | L.ATOM s ->
          junk t;
          begin match Sexp.make_loc with
            | None -> Sexp.atom s
            | Some f ->
              (* build a position for this token *)
              let loc =
                f (pair_of_pos_ t.buf.lex_start_p) (pair_of_pos_ t.buf.lex_curr_p)
                  t.buf.lex_curr_p.pos_fname in
              Sexp.atom_with_loc ~loc s
          end
        | L.LIST_OPEN ->
          let pos_start = t.buf.lex_curr_p in
          junk t;
          let l = lst [] in
          begin match cur t with
            | L.LIST_CLOSE ->
              junk t;
              begin match Sexp.make_loc with
                | None -> Sexp.list l
                | Some f ->
                  let loc =
                    f (pair_of_pos_ pos_start)
                      (pair_of_pos_ t.buf.lex_curr_p)
                      t.buf.lex_curr_p.pos_fname in
                  Sexp.list_with_loc ~loc l
              end
            | _ -> error_ t.buf "expected ')'"
          end
        | L.LIST_CLOSE -> error_ t.buf "expected expression"
      and lst acc = match cur t with
        | L.LIST_CLOSE -> List.rev acc
        | L.LIST_OPEN | L.ATOM _ | L.SEXP_COMMENT ->
          let sub = expr () in
          lst (sub::acc)
        | L.EOI -> error_ t.buf "unexpected EOI"
      in
      try Yield (expr ())
      with
        | E_end -> End
        | E_error (line,col,msg)
        | CCSexp_lex.Error (line,col,msg) ->
          Fail (Printf.sprintf "parse error at %d:%d: %s" line col msg)

    let to_list (d:t) : _ or_error =
      let rec iter acc = match next d with
        | End -> Ok (List.rev acc)
        | Yield x -> iter (x::acc)
        | Fail e -> Error e
      in
      try iter []
      with e -> Error (Printexc.to_string e)
  end

  let dec_next_ (d:Decoder.t) : _ or_error =
    match Decoder.next d with
      | End -> Error "unexpected end of file"
      | Yield x -> Ok x
      | Fail s -> Error s

  let parse_string s : t or_error =
    let buf = Lexing.from_string s in
    let d = Decoder.of_lexbuf buf in
    dec_next_ d

  let parse_string_list s : t list or_error =
    let buf = Lexing.from_string s in
    let d = Decoder.of_lexbuf buf in
    Decoder.to_list d

  let set_file_ ?file buf =
    let open Lexing in
    match file with
    | Some s -> buf.lex_start_p <- {buf.lex_start_p with pos_fname=s}
    | None -> ()

  let parse_chan_ ?file ic : sexp or_error =
    let buf = Lexing.from_channel ic in
    set_file_ ?file buf;
    let d = Decoder.of_lexbuf buf in
    dec_next_ d

  let parse_chan_list_ ?file ic =
    let buf = Lexing.from_channel ic in
    set_file_ ?file buf;
    let d = Decoder.of_lexbuf buf in
    Decoder.to_list d

  let parse_chan ic = parse_chan_ ic
  let parse_chan_list ic = parse_chan_list_ ic

  let parse_chan_gen ic =
    let buf = Lexing.from_channel ic in
    let d = Decoder.of_lexbuf buf in
    fun () -> match Decoder.next d with
      | End -> None
      | Fail e -> Some (Error e)
      | Yield x -> Some (Ok x)

  let parse_file filename = _with_in filename (parse_chan_ ~file:filename)

  let parse_file_list filename = _with_in filename (parse_chan_list_ ~file:filename)
end

module MakeCanonical(Sexp : BASIC_SEXP) = struct
  include MakeBasic(Sexp)

  (** {3 Printing} *)

  let rec to_buf b t =
    Sexp.match_ t
      ~atom:(fun s -> Printf.bprintf b "%d:%s" (String.length s) s)
      ~list:(function
          | [] -> Buffer.add_string b "()"
          | [x] -> Printf.bprintf b "(%a)" to_buf x
          | l ->
            Buffer.add_char b '(';
            List.iter (to_buf b) l;
            Buffer.add_char b ')')

  let to_string t =
    let b = Buffer.create 128 in
    to_buf b t;
    Buffer.contents b

  let rec pp_noindent fmt t =
    Sexp.match_ t
      ~atom:(fun s -> Format.fprintf fmt "%d:%s" (String.length s) s)
      ~list:(function
          | [] -> Format.pp_print_string fmt "()"
          | [x] -> Format.fprintf fmt "(%a)" pp_noindent x
          | l ->
            Format.fprintf fmt "(";
            List.iter (pp_noindent fmt) l;
            Format.fprintf fmt ")")

  let pp = pp_noindent

  let rec to_chan oc t =
    Sexp.match_ t
      ~atom:(fun s -> Printf.fprintf oc "%d:%s" (String.length s) s)
      ~list:(function
          | [] -> output_string oc "()"
          | [x] -> Printf.fprintf oc "(%a)" to_chan x
          | l ->
            output_char oc '(';
            List.iter (to_chan oc) l;
            output_char oc ')')

  let to_file_iter filename iter =
    _with_out filename
      (fun oc -> iter (fun t -> to_chan oc t))

  let to_file filename t = to_file_iter filename (fun k -> k t)

  (** {3 Parsing} *)

  module type INPUT = sig
    exception EOF
    val read_char : unit -> char
    val read_string : int -> string
  end

  module Decoder(I:INPUT) = struct
    let[@inline] is_num_ c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
    let[@inline] as_num_ c = Char.code c - Char.code '0'

    let next_ () : sexp or_error * bool =
      let rec read_string_len n =
        match I.read_char () with
        | c when is_num_ c -> read_string_len (n * 10 + as_num_ c)
        | ':' ->
          let s = I.read_string n in
          atom s
        | _ -> failwith "expected string length"

      and eat_colon () =
        match I.read_char () with
        | ':' -> ()
        | _ -> failwith "expected ':'"

      and read_in_paren acc =
        match I.read_char () with
        | ')' -> list (List.rev acc)
        | c when is_num_ c ->
          let sexp = read_string_len (as_num_ c) in
          read_in_paren (sexp::acc)
        | '(' ->
          let sexp = read_in_paren [] in
          read_in_paren (sexp::acc)
        | _ -> failwith "expected list of sexprs"
      in
      (* read a S-expr *)
      try
        begin match I.read_char () with
          | exception I.EOF -> Error "unexpected EOF", true
          | '(' -> Ok (read_in_paren []), false
          | '0' -> eat_colon (); Ok (atom ""), false
          | c when is_num_ c -> Ok (read_string_len (as_num_ c)), false
          | _ -> Error "unexpected char, expected toplevel sexpr", false
        end
      with Failure e -> Error e, false

    let to_list () : _ or_error =
      let rec iter acc =
        match next_ () with
        | Error _, true -> Ok (List.rev acc)
        | Ok x, _ -> iter (x::acc)
        | Error _ as res, _ -> res
      in
      try iter []
      with e -> Error (Printexc.to_string e)

    let[@inline] next_or_error () : _ or_error = fst (next_ ())
  end[@@inline]

  module Decoder_str(X : sig val s : string end) =
    Decoder(struct
      exception EOF
      let i = ref 0
      let n = String.length X.s
      let read_char () =
        if !i >= n then raise_notrace EOF;
        let c = String.unsafe_get X.s !i in
        incr i;
        c
      let read_string len =
        if !i + len > n then raise_notrace EOF;
        let res = String.sub X.s !i len in
        i := !i + len;
        res
    end)[@@inline]

  let parse_string s : t or_error =
    let module D = Decoder_str(struct let s=s end) in
    D.next_or_error ()

  let parse_string_list s : t list or_error =
    let module D = Decoder_str(struct let s=s end) in
    D.to_list ()

  module Decoder_ic(X : sig val ic : in_channel end) =
    Decoder(struct
      exception EOF = End_of_file
      let[@inline] read_char () = input_char X.ic
      let read_string n =
        match n with
        | 0 -> ""
        | 1 -> String.make 1 (read_char ())
        | _ ->
          let buf = Bytes.make n '\000' in
          let i = ref 0 in
          while !i < n do
            let len = input X.ic buf !i (n - !i) in
            i := !i + len;
          done;
          Bytes.unsafe_to_string buf
    end)[@@inline]

  let parse_chan_ ?file ic : sexp or_error =
    let module D = Decoder_ic(struct let ic=ic end) in
    match D.next_or_error(), file with
    | Error s, Some file -> Error (Printf.sprintf "%s in '%s'" s file)
    | r, _ -> r

  let parse_chan_list_ ?file ic =
    let module D = Decoder_ic(struct let ic=ic end) in
    match D.to_list (), file with
    | Error s, Some file -> Error (Printf.sprintf "%s in '%s'" s file)
    | r, _ -> r

  let parse_chan ic = parse_chan_ ic
  let parse_chan_list ic = parse_chan_list_ ic

  let parse_chan_gen ic =
    let module D = Decoder_ic(struct let ic=ic end) in
    fun () ->
      match D.next_ () with
      | _, true -> None
      | Error e, _ -> Some (Error e)
      | Ok x, _ -> Some (Ok x)

  let parse_file filename = _with_in filename (parse_chan_ ~file:filename)

  let parse_file_list filename = _with_in filename (parse_chan_list_ ~file:filename)
end

type t = [
  | `Atom of string
  | `List of t list
]

let rec equal a b = match a, b with
  | `Atom s1, `Atom s2 ->
    equal_string s1 s2
  | `List l1, `List l2 ->
    begin try List.for_all2 equal l1 l2 with Invalid_argument _ -> false end
  | `Atom _, _ | `List _, _ -> false

let rec compare_list a b = match a, b with
  | [], [] -> 0
  | [], _::_ -> -1
  | _::_, [] -> 1
  | x::xs, y::ys ->
    begin match compare x y with
      | 0 -> compare_list xs ys
      | c -> c
    end

and compare a b = match a, b with
  | `Atom s1, `Atom s2 -> compare_string s1 s2
  | `List l1, `List l2 -> compare_list l1 l2
  | `Atom _, _ -> -1
  | `List _, _ -> 1

module Basic_ = struct
  type nonrec t = t
  type loc = unit
  let make_loc = None
  let atom x = `Atom x
  let list x = `List x
  let atom_with_loc ~loc:_ s = atom s
  let list_with_loc ~loc:_ l = list l

  let match_ x ~atom ~list = match x with
    | `Atom x -> atom x
    | `List l -> list l
end

include (Make(Basic_) : S with type t := t)
module Canonical = MakeCanonical(Basic_)

(*$T
  CCResult.to_opt (parse_string "(abc d/e/f \"hello \\\" () world\" )") <> None
  CCResult.to_opt (parse_string "(abc ( d e ffff   ) \"hello/world\")") <> None
  CCResult.to_opt (parse_string "\"\123\bcoucou\"") <> None
*)

(*$= & ~printer:(function Ok x -> to_string x | Error e -> "error " ^ e)
  (parse_string "(a b)") (Ok (`List [`Atom "a"; `Atom "b"]))
  (parse_string "(a\n ;coucou\n b)") (Ok (`List [`Atom "a"; `Atom "b"]))
  (parse_string "(a #; (foo bar\n (1 2 3)) b)") (Ok (`List [`Atom "a"; `Atom "b"]))
  (parse_string "#; (a b) (c d)") (Ok (`List [`Atom "c"; `Atom "d"]))
  (parse_string "#; (a b) 1") (Ok (`Atom "1"))
*)

(*$= & ~printer:(function Ok x -> String.concat ";" @@ List.map to_string x | Error e -> "error " ^ e)
  (parse_string_list "(a b)(c)") (Ok [`List [`Atom "a"; `Atom "b"]; `List [`Atom "c"]])
  (parse_string_list "  ") (Ok [])
  (parse_string_list "(a\n ;coucou\n b)") (Ok [`List [`Atom "a"; `Atom "b"]])
  (parse_string_list "#; (a b) (c d) e ") (Ok [`List [`Atom "c"; `Atom "d"]; `Atom "e"])
  (parse_string_list "#; (a b) 1") (Ok [`Atom "1"])
*)


(*$inject
  let sexp_bijective s = to_string s |> parse_string = Ok s
  let csexp_bijective s = Canonical.to_string s |> Canonical.parse_string = Ok s
*)

(*$= & ~printer:CCFormat.(to_string (Dump.result Canonical.pp))
  (Ok (`List [`Atom ""])) (parse_string "(\"\")")
  (Ok (`List [`Atom ""])) (Canonical.parse_string {|(0:)|})
  (Ok (`List [`Atom "a"; `Atom "b "])) (Canonical.parse_string {|(1:a2:b )|})
*)

(*$T
  sexp_bijective (`List [`Atom ""])
  csexp_bijective (`List [`Atom ""])
*)

(*$inject
  let sexp_gen =
    let mkatom a = `Atom a and mklist l = `List l in
    let atom = Q.Gen.(map mkatom (string_size ~gen:printable (1 -- 30))) in
    let gen = Q.Gen.(
      sized (fix
        (fun self n st -> match n with
        | 0 -> atom st
        | _ ->
          frequency
            [ 1, atom
            ; 2, map mklist (list_size (0 -- 10) (self (n/10)))
            ] st
        )
    )) in
    let rec small = function
      | `Atom s -> String.length s
      |  `List l -> List.fold_left (fun n x->n+small x) 0 l
    and print = function
      | `Atom s -> Printf.sprintf "`Atom \"%s\"" s
      | `List l -> "`List " ^ Q.Print.list print l
    and shrink = function
      | `Atom s -> Q.Iter.map mkatom (Q.Shrink.string s)
      | `List l -> Q.Iter.map mklist (Q.Shrink.list ~shrink l)
    in
    Q.make ~print ~small ~shrink gen
*)

(*$Q & ~count:100
    sexp_gen sexp_bijective
    sexp_gen csexp_bijective
*)

(*$R
  let s1 =
    `List (CCList.init 100_000
      (fun i -> `List [`Atom "-"; `Atom (string_of_int i); `Atom ")(\n]"])) in
  let str = Canonical.to_string s1 in
  match Canonical.parse_string str with
    | Ok s2 -> assert_equal s1 s2
    | Error e -> assert_failure e
*)

let atom s : t = `Atom s

(* regression for #338 *)
(*$R
  Printexc.record_backtrace true;
  let cases = [
    "\"\\256\"";
    "\"\\722\02622222\\\\\n\r<\\\\\\\\\"\\222222222\\\\\"\"\2032!2222\\\\\"\"";
    "\"\n\r<\\t\023\n\203\\622222222\\\\\"\"\2032!2222\\\\\"\"";
    "\"\n\r<@t\023\n\203\\2222D2\n\r22222\01622222222222222222222222\203\\292242\222 2\\\\\">K2";
    "\"\n\r<\\t\023\n\203\\272222222\\\\\"\"\2032\0042222\\\\\"\"";
    "\"\023\n\203\\5222\n\r<\\t\023\n\203\\52222222\\\\\"2\\\216\216\216\216\216\\\\\"\216\216\216\216\216\216\216\216\216222222222222222\147";
    "\"\\722\02622222\\\\\n\r<\\\\\\\\\"\\222222222\\\\\"\"\2032!2222\\\\\"\"";
  ] in
  cases
  |> List.iter (fun s ->
    try ignore (parse_string s);
    with e ->
      let st = Printexc.get_backtrace() in
      print_endline @@ Printexc.to_string e ^ "\n" ^ st;
      assert false)
*)
