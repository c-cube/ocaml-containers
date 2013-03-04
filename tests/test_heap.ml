(** Test heaps *)

open OUnit

let test_empty () =
  let h = Heap.empty ~compare:(fun x y -> x - y) in
  OUnit.assert_bool "is_empty empty" (Heap.is_empty h)

let suite =
  "test_heaps" >:::
    [ "test_empty" >:: test_empty;
    ]
