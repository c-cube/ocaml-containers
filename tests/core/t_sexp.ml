
open CCSexp

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;


t @@ fun () -> CCResult.to_opt (parse_string "(abc d/e/f \"hello \\\" () world\" )") <> None;;
t @@ fun () -> CCResult.to_opt (parse_string "(abc ( d e ffff   ) \"hello/world\")") <> None;;
t @@ fun () -> CCResult.to_opt (parse_string "\"\123\bcoucou\"") <> None;;

let eq' = eq ~printer:(function Ok x -> to_string x | Error e -> "error " ^ e);;
eq'  (parse_string "(a b)") (Ok (`List [`Atom "a"; `Atom "b"]));;
eq'  (parse_string "(a\n ;coucou\n b)") (Ok (`List [`Atom "a"; `Atom "b"]));;
eq'  (parse_string "(a #; (foo bar\n (1 2 3)) b)") (Ok (`List [`Atom "a"; `Atom "b"]));;
eq'  (parse_string "#; (a b) (c d)") (Ok (`List [`Atom "c"; `Atom "d"]));;
eq'  (parse_string "#; (a b) 1") (Ok (`Atom "1"));;

let eq' = eq ~printer:(function Ok x -> String.concat ";" @@ List.map to_string x | Error e -> "error " ^ e) ;;
eq'  (parse_string_list "(a b)(c)") (Ok [`List [`Atom "a"; `Atom "b"]; `List [`Atom "c"]]);;
eq'  (parse_string_list "  ") (Ok []);;
eq'  (parse_string_list "(a\n ;coucou\n b)") (Ok [`List [`Atom "a"; `Atom "b"]]);;
eq'  (parse_string_list "#; (a b) (c d) e ") (Ok [`List [`Atom "c"; `Atom "d"]; `Atom "e"]);;
eq'  (parse_string_list "#; (a b) 1") (Ok [`Atom "1"]);;

let sexp_bijective s = to_string s |> parse_string = Ok s;;

eq ~printer:CCFormat.(to_string (Dump.result pp))
  (Ok (`List [`Atom ""])) (parse_string "(\"\")");;

t @@ fun () -> sexp_bijective (`List [`Atom ""]);;

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
  Q.make ~print ~small ~shrink gen;;

q ~count:100 sexp_gen sexp_bijective;;

(* regression for #338 *)
t @@ fun () ->
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
      assert false);
  true;;
