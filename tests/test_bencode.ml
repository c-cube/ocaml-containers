
open OUnit

module B = Bencode

let test1 () =
  let s = "li42ei0ei-200ee" in
  match B.parse_string s with
  | B.ParseError msg ->
    OUnit.assert_failure (Printf.sprintf "should parse, got %s" msg)
  | B.ParsePartial ->
    OUnit.assert_failure "should parse, got partial"
  | B.ParseOk b ->
    OUnit.assert_equal (B.L [B.I 42; B.I 0; B.I ~-200]) b

let test2 () =
  let b =
    B.dict_of_list [
      "foo", B.I 42;
      "bar", B.L [B.I 0; B.S "caramba si"];
      "", B.S "";
    ]
  in
  let s = B.to_string b in
  (* Printf.printf "serialized to %s\n" s; *)
  let b' = B.of_string s in
  OUnit.assert_equal ~cmp:B.eq ~printer:B.to_string b b'

let test3 () =
  let b = B.dict_of_list [
    "a", B.I 1;
    "b", B.S "bbbb";
    "l", B.L [B.I 0; B.I 0; B.S "zero\n\t \x00"];
    "d", B.dict_of_list ["foo", B.S "bar"];
  ] in
  let s = B.to_string b in
  (* Printf.printf "serialized to %s\n" s; *)
  let b' = B.of_string s in
  OUnit.assert_equal ~cmp:B.eq ~printer:B.to_string b b'

let suite =
  "test_bencode" >:::
    [ "test1" >:: test1;
      "test2" >:: test2;
      "test3" >:: test3;
    ]
