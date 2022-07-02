
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

open CCString;;

open CCShims_.Stdlib;;

q Q.printable_string (fun s -> s = rev (rev s));;
q Q.printable_string (fun s -> length s = length (rev s));;
q Q.printable_string (fun s -> rev s = (to_list s |> List.rev |> of_list));;


eq  "abc" (rev "cba");;
eq  "" (rev "");;
eq  " " (rev " ");;

let eq' = eq ~printer:string_of_int;;
eq'  1 (find ~sub:"bc" "abcd");;
eq'  ~-1 (find ~sub:"bc" "abd");;
eq'  1 (find ~sub:"a" "_a_a_a_");;
eq'  6 (find ~start:5 ~sub:"a" "a1a234a");;

q ~count:10_000
  Q.(pair printable_string printable_string) (fun (s1,s2) ->
    let i = find ~sub:s2 s1 in
    i < 0 || String.sub s1 i (length s2) = s2);;

let eq' = eq ~printer:Q.Print.(list int);;
eq'  [1; 6] (find_all_l ~sub:"bc" "abc aabc  aab");;
eq'  [] (find_all_l ~sub:"bc" "abd");;
eq'  [76] (find_all_l ~sub:"aaaaaa"
    "aabbaabbaaaaabbbbabababababbbbabbbabbaaababbbaaabaabbaabbaaaabbababaaaabbaabaaaaaabbbaaaabababaabaaabbaabaaaabbababbaabbaaabaabbabababbbaabababaaabaaababbbaaaabbbaabaaababbabaababbaabbaaaaabababbabaababbbaaabbabbabababaaaabaaababaaaaabbabbaabbabbbbbbbbbbbbbbaabbabbbbbabbaaabbabbbbabaaaaabbababbbaaaa");;

t @@ fun () -> mem ~sub:"bc" "abcd";;
t @@ fun () -> not (mem ~sub:"a b" "abcd");;

let eq' = eq ~printer:string_of_int ;;
eq'  1 (rfind ~sub:"bc" "abcd");;
eq'  ~-1 (rfind ~sub:"bc" "abd");;
eq'  5 (rfind ~sub:"a" "_a_a_a_");;
eq'  4 (rfind ~sub:"bc" "abcdbcd");;
eq'  6 (rfind ~sub:"a" "a1a234a");;

q ~count:10_000
  Q.(pair printable_string printable_string) (fun (s1,s2) ->
    let i = rfind ~sub:s2 s1 in
    i < 0 || String.sub s1 i (length s2) = s2);;


eq ~printer:CCFun.id (replace ~which:`All ~sub:"a" ~by:"b" "abcdabcd") "bbcdbbcd";;
eq ~printer:CCFun.id (replace ~which:`Left ~sub:"a" ~by:"b" "abcdabcd") "bbcdabcd";;
eq ~printer:CCFun.id (replace ~which:`Right ~sub:"a" ~by:"b" "abcdabcd") "abcdbbcd";;
eq ~printer:CCFun.id (replace ~which:`All ~sub:"ab" ~by:"hello" "  abab cdabb a")
    "  hellohello cdhellob a";;
eq ~printer:CCFun.id (replace ~which:`Left ~sub:"ab" ~by:"nope" " a b c d ") " a b c d ";;
eq ~printer:CCFun.id (replace ~sub:"a" ~by:"b" "1aa234a") "1bb234b";;

t @@ fun () -> Split.list_cpy ~by:"," "aa,bb,cc" = ["aa"; "bb"; "cc"];;
t @@ fun () -> Split.list_cpy ~by:"--" "a--b----c--" = ["a"; "b"; ""; "c"; ""];;
t @@ fun () -> Split.list_cpy ~by:" " "hello  world aie" = ["hello"; ""; "world"; "aie"];;
t @@ fun () -> Split.left ~by:" " "ab cde f g " = Some ("ab", "cde f g ");;
t @@ fun () -> Split.left ~by:"__" "a__c__e_f" = Some ("a", "c__e_f");;
t @@ fun () -> Split.left ~by:"_" "abcde" = None;;
t @@ fun () -> Split.left ~by:"bb" "abbc" = Some ("a", "c");;
t @@ fun () -> Split.left ~by:"a_" "abcde" = None;;
t @@ fun () -> Split.right ~by:" " "ab cde f g" = Some ("ab cde f", "g");;
t @@ fun () -> Split.right ~by:"__" "a__c__e_f" = Some ("a__c", "e_f");;
t @@ fun () -> Split.right ~by:"_" "abcde" = None;;
t @@ fun () -> Split.right ~by:"a_" "abcde" = None;;

eq ~printer:Q.Print.(list string)
  ["a"; "few"; "words"; "from"; "our"; "sponsors"]
  (split_on_char ' ' "a few words from our sponsors");;

q Q.(printable_string) (fun s ->
    let s = split_on_char ' ' s |> String.concat " " in
    s = (split_on_char ' ' s |> String.concat " "));;

t @@ fun () -> compare_versions "0.1.3" "0.1" > 0;;
t @@ fun () -> compare_versions "10.1" "2.0" > 0;;
t @@ fun () -> compare_versions "0.1.alpha" "0.1" > 0;;
t @@ fun () -> compare_versions "0.3.dev" "0.4" < 0;;
t @@ fun () -> compare_versions "0.foo" "0.0" < 0;;
t @@ fun () -> compare_versions "1.2.3.4" "01.2.4.3" < 0;;

q Q.(pair printable_string printable_string) (fun (a,b) ->
  CCOrd.equiv (compare_versions a b) (CCOrd.opp compare_versions b a));;

t @@ fun () -> compare_natural "foo1" "foo2" < 0;;
t @@ fun () -> compare_natural "foo11" "foo2" > 0;;
t @@ fun () -> compare_natural "foo11" "foo11" = 0;;
t @@ fun () -> compare_natural "foo011" "foo11" = 0;;
t @@ fun () -> compare_natural "foo1a" "foo1b" < 0;;
t @@ fun () -> compare_natural "foo1a1" "foo1a2" < 0;;
t @@ fun () -> compare_natural "foo1a17" "foo1a2" > 0;;

q Q.(pair printable_string printable_string) (fun (a,b) ->
  CCOrd.opp compare_natural a b = compare_natural b a);;
q Q.(printable_string) (fun a -> compare_natural a a = 0);;
q Q.(triple printable_string printable_string printable_string) (fun (a,b,c) ->
    if compare_natural a b < 0 && compare_natural b c < 0
    then compare_natural a c < 0 else Q.assume_fail());;

q Q.(string_of_size Gen.(0 -- 30)) (fun s ->
    edit_distance s s = 0);;
q Q.(let p = string_of_size Gen.(0 -- 20) in pair p p) (fun (s1,s2) ->
    edit_distance s1 s2 = edit_distance s2 s1);;
q Q.(let p = string_of_size Gen.(0 -- 20) in pair p p) (fun (s1,s2) ->
    let e = edit_distance s1 s2 in
    let e' = edit_distance ~cutoff:3 s1 s2 in
    (if e' < 3 then e=e' else e >= 3) &&
    (if e <= 3 then e=e' else true));;

eq ~printer:string_of_int 2 (edit_distance "hello" "helo!");;
eq ~printer:string_of_int 5 (edit_distance "abcde" "tuvwx");;
eq ~printer:string_of_int 2 (edit_distance ~cutoff:2 "abcde" "tuvwx");;
eq ~printer:string_of_int 1
  (edit_distance ("a" ^ String.make 100 '_') ("b"^String.make 100 '_'));;
eq ~printer:string_of_int 1
  (edit_distance ~cutoff:4 ("a" ^ String.make 1000 '_') ("b"^String.make 1000 '_'));;
eq ~printer:string_of_int 2
  (edit_distance ~cutoff:3 ("a" ^ String.make 1000 '_' ^ "c")
       ("b" ^ String.make 1000 '_' ^ "d"));;

(* test that building a from s, and mutating one char of s, yields
   a string s' that is accepted by a.

   --> generate triples (s, i, c) where c is a char, s a non empty string
   and i a valid index in s.
*)

q (
    let gen = Q.Gen.(
      3 -- 10 >>= fun len ->
      0 -- (len-1) >>= fun i ->
      string_size (return len) >>= fun s ->
      char >|= fun c -> (s,i,c)
    ) in
    let small (s,_,_) = String.length s in
    Q.make ~small gen
  )
  (fun (s,i,c) ->
    let s' = Bytes.of_string s in
    Bytes.set s' i c;
    edit_distance s (Bytes.to_string s') <= 1)
;;

t @@ fun () -> prefix ~pre:"aab" "aabcd";;
t @@ fun () -> not (prefix ~pre:"ab" "aabcd");;
t @@ fun () -> not (prefix ~pre:"abcd" "abc");;
t @@ fun () -> prefix ~pre:"abc" "abcde";;
t @@ fun () -> prefix ~pre:"" "";;
t @@ fun () -> prefix ~pre:"" "abc";;
t @@ fun () -> prefix ~pre:"abc" "abc";;
t @@ fun () -> suffix ~suf:"cd" "abcd";;
t @@ fun () -> suffix ~suf:"" "";;
t @@ fun () -> suffix ~suf:"" "abc";;
t @@ fun () -> not (suffix ~suf:"cd" "abcde");;
t @@ fun () -> not (suffix ~suf:"abcd" "cd");;

eq  ("ab", "cd") (take_drop 2 "abcd");;
eq  ("abc", "") (take_drop 3 "abc");;
eq  ("abc", "") (take_drop 5 "abc");;

let eq' = eq ~printer:Q.Print.(option string);;
eq'  (Some "ab") (chop_suffix ~suf:"cd" "abcd");;
eq'  None (chop_suffix ~suf:"cd" "abcde");;
eq'  None (chop_suffix ~suf:"abcd" "cd");;
eq'  (Some "cd") (chop_prefix ~pre:"aab" "aabcd");;
eq'  None (chop_prefix ~pre:"ab" "aabcd");;
eq'  None (chop_prefix ~pre:"abcd" "abc");;

let eq' = eq ~printer:Q.Print.string;;
eq'  "  42" (pad 4 "42");;
eq'  "0042" (pad ~c:'0' 4 "42");;
eq'  "4200" (pad ~side:`Right ~c:'0' 4 "42");;
eq'  "hello" (pad 4 "hello");;
eq'  "aaa" (pad ~c:'a' 3 "");;
eq'  "aaa" (pad ~side:`Right ~c:'a' 3 "");;

t @@ fun () -> of_list ['a'; 'b'; 'c'] = "abc";;
t @@ fun () -> of_list [] = "";;

let eq' = eq ~printer:Q.Print.(list @@ Printf.sprintf "%S");;
eq'  ["ab"; "c"] (lines "ab\nc");;
eq'  ["ab"; "c"] (lines "ab\nc\n");;
eq'  [] (lines "");;
eq'  [""] (lines "\n");;
eq'  [""; "a"] (lines "\na");;

q Q.(printable_string) (fun s ->
  lines s = (lines_gen s |> Gen.to_list));;
q Q.(printable_string) (fun s ->
  lines s = (lines_iter s |> Iter.to_list));;

q Q.(small_list printable_string) (fun l ->
  concat_iter ~sep:"\n" (Iter.of_list l) = concat "\n" l);;
q Q.(small_list printable_string) (fun l ->
  concat_gen ~sep:"\n" (Gen.of_list l) = concat "\n" l);;
q Q.(small_list printable_string) (fun l ->
  concat_seq ~sep:"\n" (CCSeq.of_list l) = concat "\n" l);;

eq ~printer:CCFun.id "" (unlines []);;
eq ~printer:CCFun.id   "ab\nc\n" (unlines ["ab"; "c"]);;

q Q.printable_string (fun s -> trim (unlines (lines s)) = trim s);;
q Q.printable_string (fun s -> trim (unlines_gen (lines_gen s)) = trim s);;

q Q.(small_list small_string) (fun l ->
    let l = unlines l |> lines in
    l = (unlines l |> lines));;

t @@ fun () -> set "abcd" 1 '_' = "a_cd";;
t @@ fun () -> set "abcd" 0 '-' = "-bcd";;
t @@ fun () -> (try ignore (set "abc" 5 '_'); false with Invalid_argument _ -> true);;

eq ~printer:Q.Print.string
  "bcef" (filter_map
     (function 'c' -> None | c -> Some (Char.chr (Char.code c + 1))) "abcde");;

eq ~printer:Q.Print.string
  "abde" (filter (function 'c' -> false | _ -> true) "abcdec");;

q Q.printable_string (fun s -> filter (fun _ -> true) s = s);;

eq ~printer:Q.Print.string
  "abcde" (uniq CCShims_.Stdlib.(=) "abbccdeeeee");;

eq ~printer:CCFun.id "abc " (ltrim " abc ");;
eq ~printer:CCFun.id   " abc" (rtrim " abc ");;

q Q.(printable_string) (fun s ->
    String.trim s = (s |> ltrim |> rtrim));;
q Q.(printable_string) (fun s -> ltrim s = ltrim (ltrim s));;
q Q.(printable_string) (fun s -> rtrim s = rtrim (rtrim s));;
q Q.(printable_string) (fun s ->
    let s' = ltrim s in
    if s'="" then Q.assume_fail() else s'.[0] <> ' ');;
q Q.(printable_string) (fun s ->
    let s' = rtrim s in
    if s'="" then Q.assume_fail() else s'.[String.length s'-1] <> ' ');;

t @@ fun () -> equal_caseless "foo" "FoO";;
t @@ fun () -> equal_caseless "helLo" "HEllO";;

q Q.(pair printable_string printable_string) (fun (s1,s2) ->
    equal_caseless s1 s2 = (lowercase_ascii s1=lowercase_ascii s2));;
q Q.(printable_string) (fun s -> equal_caseless s s);;
q Q.(printable_string) (fun s -> equal_caseless (uppercase_ascii s) s);;

let eq' = eq ~printer:(Printf.sprintf "%S");;
eq'  "0068656c6c6f20776f726c64" (to_hex "\000hello world");;
eq'  "" (to_hex "");;
eq'  "\000hello world" (of_hex_exn "0068656c6c6f20776f726c64");;
eq'  "hello world" (of_hex_exn "68656C6C6F20776F726C64");;

q Q.(string) (fun s ->
  of_hex_exn (to_hex s) = s);;
q Q.(string) (fun s ->
    CCString.for_all (function 'A'..'F'|'a'..'f'|'0'..'9' -> true |  _ -> false) @@ to_hex s);;

t @@ fun () -> "ab" < "abc";;
t @@ fun () -> "123" < "14";;
