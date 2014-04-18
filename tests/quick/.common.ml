#use "topfind";;
#directory "_build/";;

#require "unix";;

let ok () =
  print_endline "... OK";
  exit 0;;

let fail msg =
  print_endline ("... FAILURE " ^ msg);
  exit 1;;

(* vim:syntax=ocaml
*)
