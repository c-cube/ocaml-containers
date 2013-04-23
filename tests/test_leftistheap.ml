
(* test leftistheap *)

open OUnit

let empty = Leftistheap.empty ~leq:(fun i j -> i <= j)

let test1 () =
  let h = Leftistheap.of_seq empty (Sequence.of_list [5;3;4;1;42;0]) in
  let h, x = Leftistheap.extract_min h in
  OUnit.assert_equal ~printer:string_of_int 0 x;
  let h, x = Leftistheap.extract_min h in
  OUnit.assert_equal ~printer:string_of_int 1 x;
  let h, x = Leftistheap.extract_min h in
  OUnit.assert_equal ~printer:string_of_int 3 x;
  let h, x = Leftistheap.extract_min h in
  OUnit.assert_equal ~printer:string_of_int 4 x;
  let h, x = Leftistheap.extract_min h in
  OUnit.assert_equal ~printer:string_of_int 5 x;
  let h, x = Leftistheap.extract_min h in
  OUnit.assert_equal ~printer:string_of_int 42 x;
  OUnit.assert_raises Not_found (fun () -> Leftistheap.extract_min h);
  ()

let rec is_sorted l = match l with
  | [_]
  | [] -> true
  | x::((y::_) as l') -> x <= y && is_sorted l'

let extract_list heap =
  let rec recurse acc h =
    if Leftistheap.is_empty h
      then List.rev acc
      else
        let h', x = Leftistheap.extract_min h in
        recurse (x::acc) h'
  in
  recurse [] heap

(* heap sort on a random list *)
let test_sort () =
  let n = 100_000 in
  let l = Sequence.to_rev_list (Sequence.take n (Sequence.random_int n)) in
  (* put elements into a heap *)
  let h = Leftistheap.of_seq empty (Sequence.of_list l) in
  let l' = extract_list h in
  OUnit.assert_bool "sorted" (is_sorted l');
  ()

let suite =
  "test_leftistheap" >:::
    [ "test1" >:: test1;
      "test_sort" >:: test_sort;
      "test_sort2" >:: test_sort;  (* random! *)
    ]
