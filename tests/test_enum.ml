
open OUnit
open Enum.Infix

let pint i = string_of_int i

let test_singleton () =
  let e = Enum.singleton 42 in
  let gen = Enum.start e in
  OUnit.assert_equal 42 (Enum.next gen);
  OUnit.assert_raises Enum.EOG (fun () -> Enum.next gen);
  OUnit.assert_equal 1 (Enum.length e);
  ()

let test_iter () =
  let e = 1 -- 10 in
  OUnit.assert_equal ~printer:pint 10 (Enum.length e);
  (* TODO *)
  ()

let suite =
  "test_enum" >:::
    [ "test_singleton" >:: test_singleton;
      "test_iter" >:: test_iter;
    ]
