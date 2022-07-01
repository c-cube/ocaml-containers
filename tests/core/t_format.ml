
open CCFormat

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

let to_string_test s = CCFormat.sprintf_no_color "@[<h>%a@]%!" s ();;

eq ~printer:(fun s->CCFormat.sprintf "%S" s) "a b" (to_string_test (return "a@ b"));;
eq ~printer:(fun s->CCFormat.sprintf "%S" s) ", " (to_string_test (return ",@ "));;
eq ~printer:(fun s->CCFormat.sprintf "%S" s) "and then" (to_string_test (return "@{<Red>and then@}@,"));;
eq ~printer:(fun s->CCFormat.sprintf "%S" s) "a b" (to_string_test (return "@[<h>a@ b@]"));;


eq ~printer:(fun s->CCFormat.sprintf "%S" s) "a\nb\nc"
  (sprintf_no_color "@[<v>%a@]%!" text "a b c");;
eq ~printer:(fun s->CCFormat.sprintf "%S" s) "a b\nc"
  (sprintf_no_color "@[<h>%a@]%!" text "a b\nc");;


eq ~printer:(fun s->CCFormat.sprintf "%S" s)
  "(a\n b\n c)" (sprintf_no_color "(@[<v>%a@])" string_lines "a\nb\nc");;


eq ~printer:(fun s -> CCFormat.sprintf "%S" s) "foobar"
  (to_string_test (append (return "foo") (return "bar")));;
eq ~printer:(fun s -> CCFormat.sprintf "%S" s) "bar"
  (to_string_test (append (return "") (return "bar")));;
eq ~printer:(fun s -> CCFormat.sprintf "%S" s) "foo"
  (to_string_test (append (return "foo") (return "")));;


eq ~printer:(fun s -> CCFormat.sprintf "%S" s) "" (to_string_test @@ append_l []);;
eq ~printer:(fun s -> CCFormat.sprintf "%S" s) "foobarbaz" (to_string_test @@ append_l (List.map return ["foo"; "bar"; "baz"]));;
eq ~printer:(fun s -> CCFormat.sprintf "%S" s) "3141" (to_string_test @@ append_l (List.map (const int) [3; 14; 1]));;

t @@ fun () ->
  let buf1 = Buffer.create 42 in
  let buf2 = Buffer.create 42 in
  let f1 = Format.formatter_of_buffer buf1 in
  let f2 = Format.formatter_of_buffer buf2 in
  let fmt = tee f1 f2 in
  Format.fprintf fmt "coucou@.";
  assert_equal ~printer:CCFun.id "coucou\n" (Buffer.contents buf1);
  assert_equal ~printer:CCFun.id "coucou\n" (Buffer.contents buf2);
  true;;

t @@ fun () ->
  set_color_default true;
  let s = sprintf
    "what is your %a? %a! No, %a! Ahhhhhhh@."
    (styling [`FG `White; `Bold] string) "favorite color"
    (styling [`FG `Blue] string) "blue"
    (styling [`FG `Red] string) "red"
  in
  assert_equal ~printer:CCFun.id
    "what is your \027[37;1mfavorite color\027[0m? \027[34mblue\027[0m! No, \027[31mred\027[0m! Ahhhhhhh\n"
    s;
    true
;;

t @@ fun () ->
  set_color_default true;
  let s = sprintf
    "what is your @{<White>favorite color@}? @{<blue>blue@}! No, @{<red>red@}! Ahhhhhhh@."
  in
  assert_equal ~printer:CCFun.id
    "what is your \027[37;1mfavorite color\027[0m? \027[34mblue\027[0m! No, \027[31mred\027[0m! Ahhhhhhh\n"
    s;
    true;;

t @@ fun () -> sprintf "yolo %s %d" "a b" 42 = "yolo a b 42";;
t @@ fun () -> sprintf "%d " 0 = "0 ";;
t @@ fun () -> sprintf_no_color "%d " 0 = "0 ";;

t @@ fun () ->
  set_color_default true;
  assert_equal "\027[31myolo\027[0m" (sprintf "@{<red>yolo@}");
  assert_equal "yolo" (sprintf_no_color "@{<red>yolo@}");
  true;;

eq ~printer:CCFormat.(to_string (opt string))
  (Some "hello world")
    (ksprintf ~f:(fun s -> Some s) "hello %a" CCFormat.string "world");;


eq ~printer:(fun s->s) "[1;2;3]" (to_string Dump.(list int) [1;2;3]);;
eq ~printer:(fun s->s) "Some 1" (to_string Dump.(option int) (Some 1));;
eq ~printer:(fun s->s) "[None;Some \"a b\"]" (to_string Dump.(list (option string)) [None; Some "a b"]);;
eq ~printer:(fun s->s) "[(Ok \"a b c\");(Error \"nope\")]"
    (to_string Dump.(list (result string)) [Ok "a b c"; Error "nope"]);;

eq  ANSI_codes.reset "\x1b[0m";;
