#!/usr/bin/env ocaml

(* note: this requires to generate documentation first, so that
   .odoc files are generated *)

#use "topfind";;
#require "containers";;
#require "containers.io";;
#require "gen";;
#require "unix";;

let odoc_files =
  CCIO.File.walk "_build"
  |> Gen.filter_map
    (function
      | `File, f when CCString.suffix ~suf:".odoc" f -> Some f
      | _ -> None
    )
  |> Gen.flat_map
    (fun f -> Gen.of_list ["-load"; f])
  |> Gen.to_list
;;

let out = "deps.dot";;

let cmd =
  "ocamldoc -dot -o " ^ out ^ " " ^ String.concat " " odoc_files
;;

print_endline ("run: " ^ cmd);;
Unix.system cmd;;
print_endline ("output in " ^ out);;
