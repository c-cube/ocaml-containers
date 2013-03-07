
open OUnit

module IFHashtbl = FHashtbl.Make(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

let test_add () =
  let h = IFHashtbl.empty 32 in
  let h = IFHashtbl.replace h 42 "foo" in
  OUnit.assert_equal (IFHashtbl.find h 42) "foo"

let my_list = 
  [ 1, "a";
    2, "b";
    3, "c";
    4, "d";
  ]

let my_seq = Sequence.of_list my_list

let test_of_seq () =
  let h = IFHashtbl.of_seq my_seq in
  OUnit.assert_equal "b" (IFHashtbl.find h 2);
  OUnit.assert_equal "a" (IFHashtbl.find h 1);
  OUnit.assert_raises Not_found (fun () -> IFHashtbl.find h 42);
  ()

let test_to_seq () =
  let h = IFHashtbl.of_seq my_seq in
  let l = Sequence.to_list (IFHashtbl.to_seq h) in
  OUnit.assert_equal my_list (List.sort compare l)

let test_resize () =
  let h = IFHashtbl.of_seq
    (Sequence.map (fun i -> i, string_of_int i)
      (Sequence.int_range ~start:0 ~stop:200)) in
  OUnit.assert_bool "must have been resized" (IFHashtbl.depth h > 0);
  ()

let test_persistent () =
  let h = IFHashtbl.of_seq my_seq in
  OUnit.assert_equal "a" (IFHashtbl.find h 1);
  OUnit.assert_raises Not_found (fun () -> IFHashtbl.find h 5);
  let h' = IFHashtbl.replace h 5 "e" in
  OUnit.assert_equal "a" (IFHashtbl.find h' 1);
  OUnit.assert_equal "e" (IFHashtbl.find h' 5);
  OUnit.assert_equal "a" (IFHashtbl.find h 1);
  OUnit.assert_raises Not_found (fun () -> IFHashtbl.find h 5);
  ()

let test_big () =
  let n = 10000 in
  let seq = Sequence.map (fun i -> i, string_of_int i)
    (Sequence.int_range ~start:0 ~stop:n) in
  let h = IFHashtbl.of_seq seq in
  (*
  Format.printf "@[<v2>table:%a@]@." (Sequence.pp_seq
    (fun formatter (k,v) -> Format.fprintf formatter "%d -> \"%s\"" k v))
    (IFHashtbl.to_seq h);
  *)
  Sequence.iter
    (fun (k,v) ->
      (*
      Format.printf "lookup %d@." k;
      *)
      OUnit.assert_equal ~printer:(fun x -> x) v (IFHashtbl.find h k))
    seq;
  OUnit.assert_raises Not_found (fun () -> IFHashtbl.find h (n+1));
  ()

(*
let test_remove () =
  let h = IHashtbl.create 3 in
  IHashtbl.of_seq h my_seq;
  OUnit.assert_equal (IHashtbl.find h 2) "b";
  OUnit.assert_equal (IHashtbl.find h 3) "c";
  OUnit.assert_equal (IHashtbl.find h 4) "d";
  OUnit.assert_equal (IHashtbl.length h) 4;
  IHashtbl.remove h 2;
  OUnit.assert_equal (IHashtbl.find h 3) "c";
  OUnit.assert_equal (IHashtbl.length h) 3;
  (* test that 2 has been removed *)
  OUnit.assert_raises Not_found (fun () -> IHashtbl.find h 2)
*)

let suite =
  "test_pHashtbl" >:::
    [ "test_add" >:: test_add;
      "test_of_seq" >:: test_of_seq;
      "test_to_seq" >:: test_to_seq;
      "test_resize" >:: test_resize;
      "test_persistent" >:: test_persistent;
      "test_big" >:: test_big;
      (*
      "test_remove" >:: test_remove;
      *)
    ]


