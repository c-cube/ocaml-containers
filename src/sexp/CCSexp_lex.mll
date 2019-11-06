{
  open CCShims_
  type token =
    | ATOM of string
    | LIST_OPEN
    | LIST_CLOSE
    | SEXP_COMMENT
    | EOI

  (* location + message *)
  exception Error of int * int * string

  let error lexbuf msg =
    let start = Lexing.lexeme_start_p lexbuf in
    let line = start.Lexing.pos_lnum in
    let col = start.Lexing.pos_cnum - start.Lexing.pos_bol in
    raise (Error (line,col,msg))

  type unescape_state =
    | Not_escaped
    | Escaped
    | Escaped_int_1 of int
    | Escaped_int_2 of int

  let char_equal (a : char) b = Stdlib.(=) a b

  (* remove quotes + unescape *)
  let remove_quotes lexbuf s =
    assert (char_equal s.[0] '"' && char_equal s.[String.length s - 1] '"');
    let buf = Buffer.create (String.length s) in
    let st = ref Not_escaped in
    for i = 1 to String.length s-2 do
      match !st, s.[i] with
      | Escaped, '\\' -> Buffer.add_char buf '\\'; st := Not_escaped
      | Not_escaped, '\\' -> st := Escaped
      | Escaped, 'n' -> Buffer.add_char buf '\n'; st := Not_escaped
      | Escaped, 'r' -> Buffer.add_char buf '\r'; st := Not_escaped
      | Escaped, 't' -> Buffer.add_char buf '\t'; st := Not_escaped
      | Escaped, 'b' -> Buffer.add_char buf '\b'; st := Not_escaped
      | Escaped, '"' -> Buffer.add_char buf '"'; st := Not_escaped
      | Escaped, ('0'..'9' as c) ->
          st := Escaped_int_1 (Char.code c - Char.code '0')
      | Escaped_int_1 i, ('0'..'9' as c) ->
          st := Escaped_int_2 (10*i+Char.code c - Char.code '0')
      | Escaped_int_2 i, ('0'..'9' as c) ->
          Buffer.add_char buf (Char.chr (10*i+Char.code c - Char.code '0'));
          st := Not_escaped
      | (Escaped | Escaped_int_1 _ | Escaped_int_2 _), c ->
          error lexbuf (Printf.sprintf "wrong escape `%c`" c)
      | Not_escaped, c -> Buffer.add_char buf c;
    done;
    Buffer.contents buf

}

let newline = '\n' | "\r\n"
let white = [' ' '\r' '\t'] | newline

let comment_line = ';' [^ '\n']*
let printable_char = [^ '\n']

let id = [^ ')' '(' '"' ' ' '\t' '\r' '\n']+
let num = ['0'-'9']
let string_item =
  ([^ '"' '\\'] | "\\\"" | "\\\\" | "\\b" | "\\n" | "\\t" | "\\r" | '\\' num num num )
let string = '"' string_item* '"'

rule token = parse
  | "#;" { SEXP_COMMENT }
  | comment_line { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | white { token lexbuf }
  | eof { EOI }
  | '(' { LIST_OPEN }
  | ')' { LIST_CLOSE }
  | id { ATOM (Lexing.lexeme lexbuf) }
  | string { ATOM (remove_quotes lexbuf (Lexing.lexeme lexbuf)) }
  | _ as c
    { error lexbuf (Printf.sprintf "lexing failed on char `%c`" c) }
