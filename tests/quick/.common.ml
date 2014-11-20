#use "topfind";;
#directory "_build/core/";;
#directory "_build/string";;
#directory "_build/misc";;
#directory "_build/lwt";;

#require "unix";;

let ok () =
  print_endline "... OK";
  exit 0;;

let fail msg =
  print_endline ("... FAILURE " ^ msg);
  exit 1;;

(* vim:syntax=ocaml
*)
