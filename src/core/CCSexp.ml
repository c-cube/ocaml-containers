(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Simple S-expression parsing/printing} *)

open CCShims_

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

module type SEXP = CCSexp_intf.SEXP
module type S = CCSexp_intf.S

let equal_string (a : string) b = Stdlib.(=) a b
let compare_string (a : string) b = Stdlib.compare a b

module Make(Sexp : SEXP) = struct
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

  (** {2 Printing} *)

  let _with_out filename f =
    let oc = open_out filename in
    try
      let x = f oc in
      close_out oc;
      x
    with e ->
      close_out oc;
      raise e

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

  let _with_in filename f =
    let ic = open_in filename in
    try
      let x = f ic in
      close_in ic;
      x
    with e ->
      close_in ic;
      Error (Printexc.to_string e)

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

include (Make(struct
    type t_ = t
    type t = t_
    type loc = unit
    let make_loc = None
    let atom x = `Atom x
    let list x = `List x
    let atom_with_loc ~loc:_ s = atom s
    let list_with_loc ~loc:_ l = list l

    let match_ x ~atom ~list = match x with
      | `Atom x -> atom x
      | `List l -> list l
  end) : S with type t := t)

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
*)

(*$T
  (Ok (`List [`Atom ""]) = (parse_string "(\"\")"))
  sexp_bijective (`List [`Atom ""])
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
*)

let atom s : t = `Atom s
