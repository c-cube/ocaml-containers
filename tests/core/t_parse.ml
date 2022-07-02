
module Tst = (val Containers_testlib.make ~__FILE__ ())
include Tst;;

open CCParse

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

let errpp pp = function
  | Ok x -> "Ok " ^ pp x
  | Error s -> "Error " ^ s

let errpptree = errpp pptree

let erreq eq x y = match x, y with
  | Ok x, Ok y -> eq x y
  | Error _ , Error _ -> true
  | _ -> false ;;

(* ### start tests ### *)

eq ~printer:errpptree  (Ok (N (L 1, N (L 2, L 3))))
    (parse_string ptree "(1 (2 3))" );;
eq ~printer:errpptree  (Ok (N (N (L 1, L 2), N (L 3, N (L 4, L 5)))))
    (parse_string ptree "((1 2) (3 (4 5)))" );;
eq ~printer:errpptree  (Ok (N (L 1, N (L 2, L 3))))
    (parse_string ptree' "(1 (2 3))" );;
eq ~printer:errpptree  (Ok (N (N (L 1, L 2), N (L 3, N (L 4, L 5)))))
    (parse_string ptree' "((1 2) (3 (4 5)))" );;

t @@ fun () ->
  let p = U.list ~sep:"," U.word in
  let printer = function
    | Ok l -> "Ok " ^ CCFormat.(to_string (Dump.list string_quoted)) l
    | Error s -> "Error " ^ s
  in
  assert_equal ~printer
    (Ok ["abc"; "de"; "hello"; "world"])
    (parse_string p "[abc , de, hello ,world  ]");
    true;;

t @@ fun () ->
  let test n =
    let p = CCParse.(U.list ~sep:"," U.int) in

    let l = CCList.(1 -- n) in
    let l_printed =
      CCFormat.(to_string (within "[" "]" (list ~sep:(return ",") int))) l in

    let l' = CCParse.parse_string_exn p l_printed in

    assert_equal ~printer:Q.Print.(list int) l l'
  in
  test 300_000;
  true ;;

t @@ fun () ->
  let open CCParse.Infix in
  let module P = CCParse in

  let parens p = P.char '(' *> p <* P.char ')' in
  let add = P.char '+' *> P.return (+) in
  let sub = P.char '-' *> P.return (-) in
  let mul = P.char '*' *> P.return ( * ) in
  let div = P.char '/' *> P.return ( / ) in
  let integer =
  P.chars1_if (function '0'..'9'->true|_->false) >|= int_of_string in

  let chainr1 e op =
  P.fix (fun r ->
    e >>= fun x -> (op <*> P.return x <*> r) <|> P.return x) in

  let expr : int P.t =
  P.fix (fun expr ->
    let factor = parens expr <|> integer in
    let term = chainr1 factor (mul <|> div) in
    chainr1 term (add <|> sub)) in

  assert_equal (Ok 6) (P.parse_string expr "4*1+2");
  assert_equal (Ok 12) (P.parse_string expr "4*(1+2)");
  true;;

let eq' = eq ~printer:(errpp Q.Print.bool) ~cmp:(erreq (=)) ;;
eq'  (Ok true) (parse_string (U.bool <* eoi) "true");;
eq'  (Error "") (parse_string (U.bool <* eoi) "true ");;
eq'  (Ok true) (parse_string (U.bool <* skip_white <* eoi) "true");;

let eq' = eq ~printer:Q.Print.(pair int int);;
eq' (0,5) (let p = any_char_n 5 *> pos in
  match parse_string p "abcde   " with
    | Ok p -> Position.line_and_column p
    | Error _ -> assert false);;

eq ~printer:Q.Print.(list @@ pair int int)
  [(0,2); (1,3); (2,1); (3,0); (4,0); (5,2)]
   (let p = each_line (skip_space *> pos) in
    match parse_string p "  a\n  b\nc\n\n\n a" with
      | Ok ps -> List.map Position.line_and_column ps
      | Error _ -> assert false);;

let eq' = eq ~printer:(errpp Q.Print.string) ~cmp:(erreq (=));;
eq'  (Ok "abcd") (parse_string all_str "abcd");;
eq'  (Ok "cd") (parse_string (string "ab" *> all_str) "abcd");;
eq'  (Ok "") (parse_string (string "ab" *> all_str) "ab");;

eq ~printer:(errpp Q.Print.(pair string string)) ~cmp:(erreq (=))
  (Ok ("foobar", "")) (parse_string (both all_str all_str) "foobar");;

q Q.(printable_string) (fun s ->
      let pred = (function 'a'..'z' | 'A' .. 'Z' | '{' | '}' -> true | _ -> false) in
      let p1 = chars1_if pred in
      let p2 = take1_if pred >|= Slice.to_string in
      parse_string p1 s = parse_string p2 s);;

t @@ fun () ->
  let pred = (function 'a'..'z' | 'A' .. 'Z' | '{' | '}' -> true | _ -> false) in
  parse_string (chars_if pred) "coucou{lol} 123" = Ok "coucou{lol}" ;;

t @@ fun () ->
  let p0 = skip_white *> U.int in
  let p = (skip_white *> char '(' *> many p0) <* (skip_white <* char ')') in
  let printer =  CCFormat.(to_string @@ Dump.result  @@ Dump.list int) in
  assert_equal ~printer
    (Ok [1;2;3]) (parse_string p "(1 2 3)");
  assert_equal ~printer
    (Ok [1;2; -30; 4]) (parse_string p "( 1 2    -30 4 )");
  true;;

let aword = chars1_if (function 'a'..'z'|'A'..'Z'->true|_ -> false);;

eq ~printer:(errpp Q.Print.(list string))
  (Ok ["a";"b";"c"])
  (parse_string (optional (char '/') *> sep ~by:(char '/') aword) "/a/b/c");;
eq ~printer:(errpp Q.Print.(list string))
  (Ok ["a";"b";"c"])
  (parse_string (optional (char '/') *> sep ~by:(char '/') aword) "a/b/c");;

eq ~printer:(errpp Q.Print.(string))
  (Ok "abc") (parse_string (lookahead (string "ab") *> (string "abc")) "abcd");;

eq ~printer:(errpp Q.Print.(string))
  (Ok "1234") (parse_string line_str "1234\nyolo");;

eq ~printer:(errpp Q.Print.(pair String.escaped String.escaped))
  (Ok ("1234", "yolo")) (parse_string (line_str ||| line_str) "1234\nyolo\nswag");;

eq ~printer:(errpp Q.Print.(list string)) ~cmp:(erreq (=))
  (Ok ["a";"b";"c";"d,e,f"])
    (parse_string (split_list_at_most ~on_char:',' 3 >|= List.map Slice.to_string) "a,b,c,d,e,f");;
eq ~printer:(errpp Q.Print.(list string)) ~cmp:(erreq (=))
  (Ok ["a";"bc"])
    (parse_string (split_list_at_most ~on_char:',' 3 >|= List.map Slice.to_string) "a,bc");;

eq ~printer:(errpp Q.Print.(list @@ list int))
  (Ok ([[1;1];[2;2];[3;3];[]]))
    (parse_string (each_line (sep ~by:skip_space U.int)) "1 1\n2 2\n3   3\n");;

let eq' = eq ~printer:(errpp Q.Print.int) ~cmp:(erreq (=)) ;;
eq'  (Ok 42) (parse_string U.int " 42");;
eq'  (Ok 2) (parse_string U.int "2");;
eq'  (Error "") (parse_string U.int "abc");;
eq'  (Error "") (parse_string U.int "");;

let eq' = eq ~printer:(errpp Q.Print.int) ~cmp:(erreq (=));;
eq'  (Ok 15) (parse_string (U.in_paren (U.in_paren U.int)) "( ( 15) )");;
eq'  (Ok 2) (parse_string (U.in_paren U.int) "(2)");;
eq'  (Error "") (parse_string (U.in_paren U.int) "2");;
eq'  (Error "") (parse_string (U.in_paren U.int) "");;
eq'  (Ok 2) (parse_string (U.in_parens_opt U.int) "((((2))))");;
eq'  (Ok 2) (parse_string (U.in_parens_opt U.int) "2");;
eq'  (Ok 200) (parse_string (U.in_parens_opt U.int) "( (  200 ) )");;

let eq' = eq ~printer:(errpp Q.Print.(option int)) ~cmp:(erreq (=));;
eq'  (Ok (Some 12)) (parse_string U.(option int) " Some 12");;
eq'  (Ok None) (parse_string U.(option int) "  None");;
eq'  (Ok (Some 0)) (parse_string U.(option int) "Some 0");;
eq'  (Ok (Some 0)) (parse_string U.(in_parens_opt @@ option int) "(( Some 0) )");;

let eq' = eq ~printer:(errpp Q.Print.int) ~cmp:(erreq (=)) ;;
eq'  (Ok 16) (parse_string U.hexa_int "0x10");;
eq'  (Ok 16) (parse_string U.hexa_int "10");;
eq'  (Error "") (parse_string U.hexa_int "x10");;
eq'  (Error "") (parse_string U.hexa_int "0xz");;

eq ~printer:(errpp Q.Print.bool) ~cmp:(erreq (=))
  (Ok true) (parse_string U.bool "true");;
eq ~printer:(errpp Q.Print.bool) ~cmp:(erreq (=))
  (Ok false) (parse_string U.bool "false");;

eq ~printer:Q.Print.(errpp (pair int int))
  (Ok(1,2)) U.(parse_string (pair int int) "(1 , 2 )");;

