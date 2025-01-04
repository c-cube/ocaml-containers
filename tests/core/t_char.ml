open CCChar
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq (Some 'a') (of_int (to_int 'a'));;
eq None (of_int 257);;

q
  (Q.string_of_size (Q.Gen.return 1))
  (fun s -> Stdlib.( = ) (to_string s.[0]) s)
;;

q (Q.int_range 65 90 |> Q.map Char.chr) CCChar.is_uppercase_ascii;;

q
  (Q.int_range 0 64 |> Q.map Char.chr)
  (fun c -> not @@ CCChar.is_uppercase_ascii c)
;;

q
  (Q.int_range 91 127 |> Q.map Char.chr)
  (fun c -> not @@ CCChar.is_uppercase_ascii c)
;;

q (Q.int_range 97 122 |> Q.map Char.chr) CCChar.is_lowercase_ascii;;

q
  (Q.int_range 0 96 |> Q.map Char.chr)
  (fun c -> not @@ CCChar.is_lowercase_ascii c)
;;

q
  (Q.int_range 123 127 |> Q.map Char.chr)
  (fun c -> not @@ CCChar.is_lowercase_ascii c)
;;

q (Q.int_range 48 57 |> Q.map Char.chr) CCChar.is_digit_ascii;;
q (Q.int_range 0 47 |> Q.map Char.chr) (fun c -> not @@ CCChar.is_digit_ascii c)
;;

q
  (Q.int_range 58 127 |> Q.map Char.chr)
  (fun c -> not @@ CCChar.is_digit_ascii c)
;;

eq true
  (Stdlib.List.for_all CCChar.is_whitespace_ascii
     [ '\n'; '\t'; ' '; '\010'; '\011'; '\012'; '\013' ])
;;

eq false
  (Stdlib.List.exists CCChar.is_whitespace_ascii
     [
       'H';
       'e';
       'l';
       'l';
       'o';
       '!';
       '-';
       '-';
       'N';
       'O';
       't';
       'h';
       'i';
       'n';
       'a';
       '\055';
       'k';
       'a';
       'g';
       '$';
       '$';
       '$';
       '%';
       '^';
       'b';
       'c';
       'h';
       '\008';
       'h';
     ])
