open CCParse

type sexp = Atom of string | List of sexp list

let rec pp_sexpr out (s:sexp) : unit = match s with
  | Atom s -> Format.fprintf out "%S" s
  | List l ->
    Format.fprintf out "(@[";
    List.iteri (fun i s -> if i>0 then Format.fprintf out "@ "; pp_sexpr out s) l;
    Format.fprintf out "@])"

let str_of_sexp = CCFormat.to_string pp_sexpr

let skip_white_and_comments =
  fix @@ fun self ->
  skip_white *>
  ( try_or (char ';')
      ~f:(fun _ -> skip_chars (function '\n' -> false | _ -> true) *> self)
      ~else_:(return ())
  )

let atom =
  chars_fold_transduce `Start
    ~f:(fun acc c ->
        match acc, c with
          | `Start, '"' -> `Continue `In_quote
          | `Start, (' ' | '\t' | '\n' | '(' | ')' | ';') -> `Fail "atom"
          | `Normal, (' ' | '\t' | '\n' | '(' | ')' | ';') -> `Stop
          | `Done, _ -> `Stop
          | `In_quote, '"' -> `Continue `Done (* consume *)
          | `In_quote, '\\' -> `Continue `Escape
          | `In_quote, c -> `Yield (`In_quote, c)
          | `Escape, 'n' -> `Yield (`In_quote, '\n')
          | `Escape, 't' -> `Yield (`In_quote, '\t')
          | `Escape, '"' -> `Yield (`In_quote, '"')
          | `Escape, '\\' -> `Yield (`In_quote, '\\')
          | `Escape, c -> `Fail (Printf.sprintf "unknown escape code \\%c" c)
          | (`Start | `Normal), c -> `Yield (`Normal, c)
          | _ -> `Fail "invalid atom"
      )
  >>= function
  | `In_quote, _ -> fail "unclosed \""
  | `Escape, _ -> fail "unfinished escape sequence"
  | _, "" -> fail "expected non-empty atom"
  | _, s -> return (Atom s)

let psexp =
  fix @@ fun self ->
  skip_white_and_comments *>
  try_or (char '(')
    ~f:(fun _ ->
      (sep ~by:skip_white_and_comments self
      <* skip_white_and_comments <* char ')') >|= fun l -> List l)
    ~else_:atom

let psexp_l =
  many_until ~until:(skip_white_and_comments *> eoi) psexp

let () =
  let s = CCIO.File.read_exn Sys.argv.(1) in
  match parse_string psexp_l s with
    | Ok l ->
      Format.printf "parsed:@.";
      List.iter (Format.printf "%a@." pp_sexpr) l
    | Error e ->
      Format.printf "parse error: %s@." e; exit 1
