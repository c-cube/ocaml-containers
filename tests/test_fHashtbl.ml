
open OUnit
open Containers_misc



module Test(SomeHashtbl : FHashtbl.S with type key = int) = struct
  let test_add () =
    let h = SomeHashtbl.empty 32 in
    let h = SomeHashtbl.replace h 42 "foo" in
    OUnit.assert_equal (SomeHashtbl.find h 42) "foo"

  let my_list = 
    [ 1, "a";
      2, "b";
      3, "c";
      4, "d";
    ]

  let my_seq = Sequence.of_list my_list

  let test_of_seq () =
    let h = SomeHashtbl.of_seq my_seq in
    OUnit.assert_equal "b" (SomeHashtbl.find h 2);
    OUnit.assert_equal "a" (SomeHashtbl.find h 1);
    OUnit.assert_raises Not_found (fun () -> SomeHashtbl.find h 42);
    ()

  let test_to_seq () =
    let h = SomeHashtbl.of_seq my_seq in
    let l = Sequence.to_list (SomeHashtbl.to_seq h) in
    OUnit.assert_equal my_list (List.sort compare l)

  let test_resize () =
    let h = SomeHashtbl.of_seq
      (Sequence.map (fun i -> i, string_of_int i)
        (Sequence.int_range ~start:0 ~stop:200)) in
    OUnit.assert_equal 201 (SomeHashtbl.size h);
    ()

  let test_persistent () =
    let h = SomeHashtbl.of_seq my_seq in
    OUnit.assert_equal "a" (SomeHashtbl.find h 1);
    OUnit.assert_raises Not_found (fun () -> SomeHashtbl.find h 5);
    let h' = SomeHashtbl.replace h 5 "e" in
    OUnit.assert_equal "a" (SomeHashtbl.find h' 1);
    OUnit.assert_equal "e" (SomeHashtbl.find h' 5);
    OUnit.assert_equal "a" (SomeHashtbl.find h 1);
    OUnit.assert_raises Not_found (fun () -> SomeHashtbl.find h 5);
    ()

  let test_big () =
    let n = 10000 in
    let seq = Sequence.map (fun i -> i, string_of_int i)
      (Sequence.int_range ~start:0 ~stop:n) in
    let h = SomeHashtbl.of_seq seq in
    (*
    Format.printf "@[<v2>table:%a@]@." (Sequence.pp_seq
      (fun formatter (k,v) -> Format.fprintf formatter "%d -> \"%s\"" k v))
      (SomeHashtbl.to_seq h);
    *)
    Sequence.iter
      (fun (k,v) ->
        (*
        Format.printf "lookup %d@." k;
        *)
        OUnit.assert_equal ~printer:(fun x -> x) v (SomeHashtbl.find h k))
      seq;
    OUnit.assert_raises Not_found (fun () -> SomeHashtbl.find h (n+1));
    ()

  let test_remove () =
    let h = SomeHashtbl.of_seq my_seq in
    OUnit.assert_equal (SomeHashtbl.find h 2) "b";
    OUnit.assert_equal (SomeHashtbl.find h 3) "c";
    OUnit.assert_equal (SomeHashtbl.find h 4) "d";
    OUnit.assert_equal (SomeHashtbl.size h) 4;
    let h = SomeHashtbl.remove h 2 in
    OUnit.assert_equal (SomeHashtbl.find h 3) "c";
    OUnit.assert_equal (SomeHashtbl.size h) 3;
    (* test that 2 has been removed *)
    OUnit.assert_raises Not_found (fun () -> SomeHashtbl.find h 2)

  let test_size () =
    let open Sequence.Infix in
    let n = 10000 in
    let seq = Sequence.map (fun i -> i, string_of_int i) (0 -- n) in
    let h = SomeHashtbl.of_seq seq in
    OUnit.assert_equal (n+1) (SomeHashtbl.size h);
    let h = Sequence.fold (fun h i -> SomeHashtbl.remove h i) h (0 -- 500) in
    OUnit.assert_equal (n-500) (SomeHashtbl.size h);
    OUnit.assert_bool "is_empty" (SomeHashtbl.is_empty (SomeHashtbl.empty 16));
    ()

  let suite =
    "test_FHashtbl" >:::
      [ "test_add" >:: test_add;
        "test_of_seq" >:: test_of_seq;
        "test_to_seq" >:: test_to_seq;
        "test_resize" >:: test_resize;
        "test_persistent" >:: test_persistent;
        "test_big" >:: test_big;
        "test_remove" >:: test_remove;
        "test_size" >:: test_size;
      ]
end

module ITreeHashtbl = FHashtbl.Tree(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module IFlatHashtbl = FHashtbl.Flat(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module TestTree = Test(ITreeHashtbl)
module TestFlat = Test(IFlatHashtbl)

let suite =
  OUnit.TestList ["tree" >: TestTree.suite; "flat" >: TestFlat.suite]
