{
  type token =
    | ATOM of string
    | LIST_OPEN
    | LIST_CLOSE
    | EOI

  (* location + message *)
  exception Error of int * int * string

  let error lexbuf msg =
    let start = Lexing.lexeme_start_p lexbuf in
    let line = start.Lexing.pos_lnum in
    let col = start.Lexing.pos_cnum - start.Lexing.pos_bol in
    raise (Error (line,col,msg))

  (* remove quotes + unescape *)
  let remove_quotes lexbuf s =
    assert (s.[0] = '"' && s.[String.length s - 1] = '"');
    let buf = Buffer.create (String.length s) in
    let escaped = ref false in
    for i = 1 to String.length s-2 do
      match s.[i] with
      | '\\' when !escaped -> Buffer.add_char buf '\\'; escaped := false
      | '\\' -> escaped := true
      | 'n' when !escaped -> Buffer.add_char buf '\n'; escaped := false
      | 'r' when !escaped -> Buffer.add_char buf '\r'; escaped := false
      | 't' when !escaped -> Buffer.add_char buf '\t'; escaped := false
      | '"' when !escaped -> Buffer.add_char buf '"'; escaped := false
      | c when !escaped -> error lexbuf (Printf.sprintf "wrong escape `%c`" c)
      | c -> Buffer.add_char buf c;
    done;
    Buffer.contents buf
}

let newline = '\n' | "\r\n"
let white = [' ' '\r' '\t'] | newline

let comment_line = ';' [^ '\n']*
let printable_char = [^ '\n']

let id = [^ ')' '(' '"' ' ' '\t' '\r' '\n']+
let string = '"' ([^ '"' '\\'] | "\\\"" | "\\\\" | "\\n" | "\\t" | "\\r")* '"'

rule token = parse
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

