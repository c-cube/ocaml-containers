(* quickcheck for Levenshtein *)

module Levenshtein = Containers_string.Levenshtein

(* test that automaton accepts its string *)
let test_automaton =
  let gen = QCheck.Arbitrary.(map string (fun s -> s, Levenshtein.of_string ~limit:1 s)) in
  let test (s,a) =
    Levenshtein.match_with a s
  in
  let pp (s,_) = s in
  let name = "string accepted by its own automaton" in
  QCheck.mk_test ~name ~pp ~size:(fun (s,_)->String.length s) gen test

(* test that building a from s, and mutating one char of s, yields
   a string s' that is accepted by a *)
let test_mutation =
  (* generate triples (s, i, c) where c is a char, s a non empty string
     and i a valid index in s *)
  let gen = QCheck.Arbitrary.(
    int_range ~start:3 ~stop:10 >>= fun len ->
    int (len-1) >>= fun i ->
    string_len (return len) >>= fun s ->
    char >>= fun c ->
    return (s,i,c)
  ) in
  let test (s,i,c) =
    let s' = String.copy s in
    s'.[i] <- c;
    let a = Levenshtein.of_string ~limit:1 s in
    Levenshtein.match_with a s'
  in
  let name = "mutating s.[i] into s' still accepted by automaton(s)" in
  QCheck.mk_test ~name ~size:(fun (s,_,_)->String.length s) gen test

(* test that, for an index, all retrieved strings are at a distance to
   the key that is not too high *)
let test_index =
  let gen = QCheck.Arbitrary.(
    list string >>= fun l ->
    let l = List.map (fun s->s,s) l in
    return (List.map fst l, Levenshtein.Index.of_list l)
  ) in
  let test (l, idx) =
    List.for_all
      (fun s ->
        let retrieved = Levenshtein.Index.retrieve ~limit:2 idx s
          |> Levenshtein.klist_to_list in
        List.for_all
          (fun s' -> Levenshtein.edit_distance s s' <= 2) retrieved
      ) l
  in
  let name = "strings retrieved from automaton with limit:n are at distance <= n" in
  QCheck.mk_test ~name gen test

let suite =
  [ test_automaton
  ; test_mutation
  ; test_index
  ]

let () =
  if not (QCheck.run_tests suite)
    then exit 1;
  ()
