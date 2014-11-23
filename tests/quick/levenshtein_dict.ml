#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;
#load "containers.cma";;
#load "containers_string.cma";;

open Containers_string

let words = CCIO.(
  (with_in "/usr/share/dict/cracklib-small" >>>= read_lines)
  |> run_exn
  )

let idx = List.fold_left
  (fun idx s -> Levenshtein.Index.add idx s s)
  Levenshtein.Index.empty words;;

Levenshtein.Index.retrieve ~limit:1 idx "hell"
  |> Levenshtein.klist_to_list
  |> List.iter print_endline;;
