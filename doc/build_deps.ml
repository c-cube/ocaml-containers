#!/usr/bin/env ocaml

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

let cmd =
  "ocamldoc -dot -o deps.dot " ^ String.concat " " odoc_files
;;

print_endline ("run: " ^ cmd);;
Unix.system cmd;;
