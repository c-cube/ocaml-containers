
(* test leftistheap *)

open OUnit

module Sequence = CCSequence

module H = CCLeftistheap.Make(struct type t = int let leq x y =x<=y end)

let empty = H.empty

let test1 () =
  let h = H.of_list [5;3;4;1;42;0] in
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 0 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 1 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 3 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 4 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 5 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 42 x;
  OUnit.assert_raises H.Empty (fun () -> H.take_exn h);
  ()

let rec is_sorted l = match l with
  | [_]
  | [] -> true
  | x::((y::_) as l') -> x <= y && is_sorted l'

(* extract the content of the heap into a list *)
let extract_list heap =
  let rec recurse acc h =
    if H.is_empty h
      then List.rev acc
      else
        let h', x = H.take_exn h in
        recurse (x::acc) h'
  in
  recurse [] heap

(* heap sort on a random list *)
let test_sort () =
  let n = 100_000 in
  let l = Sequence.to_rev_list (Sequence.take n (Sequence.random_int n)) in
  (* put elements into a heap *)
  let h = H.of_seq empty (Sequence.of_list l) in
  OUnit.assert_equal n (H.size h);
  let l' = extract_list h in
  OUnit.assert_bool "sorted" (is_sorted l');
  ()

let suite =
  "test_leftistheap" >:::
    [ "test1" >:: test1;
      "test_sort" >:: test_sort;
      "test_sort2" >:: test_sort;  (* random! *)
    ]
