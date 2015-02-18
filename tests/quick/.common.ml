#use "topfind";;
#directory "_build/src/core/";;
#directory "_build/src/string";;
#directory "_build/src/misc";;
#directory "_build/src/io";;
#directory "_build/src/lwt";;

#require "unix";;

let ok () =
  print_endline "... OK";
  exit 0;;

let fail msg =
  print_endline ("... FAILURE " ^ msg);
  exit 1;;

(* vim:syntax=ocaml
*)
