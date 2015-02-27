#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;
#load "containers.cma";;
#load "containers_string.cma";;
#load "containers_io.cma";;

open Containers_string

let words =
  CCIO.with_in "/usr/share/dict/words" CCIO.read_lines_l

let idx = List.fold_left
  (fun idx s -> Levenshtein.Index.add idx s s)
  Levenshtein.Index.empty words;;

Levenshtein.Index.retrieve ~limit:1 idx "hell"
  |> Levenshtein.klist_to_list
  |> List.iter print_endline;;
