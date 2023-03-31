include (val Containers_testlib.make ~__FILE__ ())
open Containers_pp

let spf = Printf.sprintf

let () =
  eq "hello world" (Flatten.to_string @@ text "hello" ^ newline ^ text "world")

let () =
  eq ~name:"l1" ~printer:(spf "%S") "[0; 1; 2; 3;\n 4; 5; 6; 7;\n 8; 9]"
    (let d = Dump.list (List.init 10 int) in
     Pretty.to_string ~width:10 d)

let () =
  eq ~name:"l2" ~printer:(spf "%S")
    "[[0; 1; 2; 3;\n\
    \  4; 5];\n\
    \ [1; 2; 3; 4;\n\
    \  5; 6];\n\
    \ [2; 3; 4; 5;\n\
    \  6; 7];\n\
    \ [3; 4; 5; 6;\n\
    \  7; 8];\n\
    \ [4; 5; 6; 7;\n\
    \  8; 9];\n\
    \ [5; 6; 7; 8;\n\
    \  9; 10]]"
    (let d =
       Dump.list
         (List.init 6 (fun i ->
              Dump.list (List.init 6 (fun j -> int @@ (i + j)))))
     in
     Pretty.to_string ~width:10 d)

let () =
  eq ~name:"s1" ~printer:(spf "%S") "(foo\n bar\n baaz\n (g 42 10))"
    (let d =
       sexp_apply "foo"
         [ text "bar"; text "baaz"; sexp_apply "g" [ int 42; int 10 ] ]
     in
     Pretty.to_string ~width:10 d)

let ext_coucou =
  {
    Ext.pre = (fun out () -> out.string "<coucou>");
    post = (fun out () -> out.string "</coucou>");
  }

let () =
  eq ~name:"wrap1" ~printer:(spf "%S")
    "(foo\n bar\n <coucou>(g 42 10)</coucou>)"
    (let d =
       sexp_apply "foo"
         [ text "bar"; wrap ext_coucou () (sexp_apply "g" [ int 42; int 10 ]) ]
     in
     Pretty.to_string ~width:10 d)
