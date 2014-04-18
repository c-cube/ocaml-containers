#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;
#load "containers.cma";;
open Containers;;

#require "batteries";;
open Batteries;;

let words = File.with_file_in "/usr/share/dict/cracklib-small"
  (fun i -> IO.read_all i |> String.nsplit ~by:"\\n");;

let idx = List.fold_left
  (fun idx s -> Levenshtein.StrIndex.add_string idx s s)
  Levenshtein.StrIndex.empty words;;

Levenshtein.StrIndex.retrieve_string ~limit:1 idx "hell"
  |> Levenshtein.klist_to_list
  |> List.iter print_endline;;
