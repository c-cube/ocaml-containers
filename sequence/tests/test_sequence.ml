
open OUnit

module S = Sequence
open Sequence.Infix

let pp_ilist l =
  let b = Buffer.create 15 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "@[<h>%a@]" (S.pp_seq Format.pp_print_int) (S.of_list l);
  Buffer.contents b

let test_empty () =
  let seq = S.empty in
  OUnit.assert_bool "empty" (S.is_empty seq);
  OUnit.assert_bool "empty"
    (try S.iter (fun _ -> raise Exit) seq; true with Exit -> false);
  ()

let test_repeat () =
  let seq = S.repeat "hello" in
  OUnit.assert_equal ["hello"; "hello"; "hello"]
    (seq |> S.take 3 |> S.to_list);
  ()

let test_concat () =
  let s1 = (1 -- 5) in
  let s2 = (6 -- 10) in
  let l = [1;2;3;4;5;6;7;8;9;10] in
  OUnit.assert_equal l (S.to_list (S.append s1 s2));
  ()

let test_fold () =
  let n = (1 -- 10)
    |> S.fold (+) 0 in
  OUnit.assert_equal 55 n;
  ()

let test_foldi () =
  let l = ["hello"; "world"]
    |> S.of_list
    |> S.foldi (fun acc i x -> (i,x) :: acc) [] in
  OUnit.assert_equal [1, "world"; 0, "hello"] l;
  ()

let test_exists () =
  (1 -- 100)
    |> S.exists (fun x -> x = 59)
    |> OUnit.assert_bool "exists";
  (1 -- 100)
    |> S.exists (fun x -> x < 0)
    |> (fun x -> not x)
    |> OUnit.assert_bool "not exists";
  ()

let test_length () =
  (1 -- 1000) |> S.length |> OUnit.assert_equal 1000

let test_concat () =
  1 -- 1000
    |> S.map (fun i -> (i -- (i+1)))
    |> S.concat
    |> S.length
    |> OUnit.assert_equal 2000

let test_flatMap () =
  1 -- 1000
    |> S.flatMap (fun i -> (i -- (i+1)))
    |> S.length
    |> OUnit.assert_equal 2000

let test_intersperse () =
  1 -- 100
    |> (fun seq -> S.intersperse 0 seq)
    |> S.take 10
    |> S.to_list
    |> OUnit.assert_equal [1;0;2;0;3;0;4;0;5;0]

let test_not_persistent () =
  let printer = pp_ilist in
  let stream = Stream.from (fun i -> if i < 5 then Some i else None) in
  let seq = S.of_stream stream in
  OUnit.assert_equal ~printer [0;1;2;3;4] (seq |> S.to_list);
  OUnit.assert_equal ~printer [] (seq |> S.to_list);
  ()

let test_persistent () =
  let printer = pp_ilist in
  let stream = Stream.from (fun i -> if i < 5 then Some i else None) in
  let seq = S.of_stream stream in
  (* consume seq into a persistent version of itself *)
  let seq' = S.persistent seq in
  OUnit.assert_equal ~printer [] (seq |> S.to_list);
  OUnit.assert_equal ~printer [0;1;2;3;4] (seq' |> S.to_list);
  OUnit.assert_equal ~printer [0;1;2;3;4] (seq' |> S.to_list);
  OUnit.assert_equal ~printer [0;1;2;3;4] (seq' |> S.to_stream |> S.of_stream |> S.to_list);
  ()

let test_big_persistent () =
  let printer = pp_ilist in
  let seq = 0 -- 10_000 in
  let seq' = S.persistent seq in
  OUnit.assert_equal 10_001 (S.length seq');
  OUnit.assert_equal 10_001 (S.length seq');
  OUnit.assert_equal ~printer [0;1;2;3] (seq' |> S.take 4 |> S.to_list);
  ()

let test_sort () =
  1 -- 100
    |> S.sort ~cmp:(fun i j -> j - i)
    |> S.take 4
    |> S.to_list
    |> OUnit.assert_equal [100;99;98;97]

let test_sort_uniq () =
  [42;1;2;3;4;5;4;3;2;1]
    |> S.of_list
    |> S.sort_uniq
    |> S.to_list
    |> OUnit.assert_equal [1;2;3;4;5;42]

let test_group () =
  [1;2;3;3;2;2;3;4]
    |> S.of_list |> S.group |> S.to_list
    |> OUnit.assert_equal [[1];[2];[3;3];[2;2];[3];[4]]

let test_uniq () =
  [1;2;2;3;4;4;4;3;3]
    |> S.of_list |> S.uniq |> S.to_list
    |> OUnit.assert_equal [1;2;3;4;3]

let test_product () =
  let stream = Stream.from (fun i -> if i < 3 then Some i else None) in
  let a = S.of_stream stream in
  let b = S.of_list ["a";"b";"c"] in
  let s = S.product a b |> S.map (fun (x,y) -> y,x)
    |> S.to_list |> List.sort compare in
  OUnit.assert_equal ["a",0; "a", 1; "a", 2;
                      "b",0; "b", 1; "b", 2;
                      "c",0; "c", 1; "c", 2;] s

let test_join () =
  let s1 = (1 -- 3) in
  let s2 = S.of_list ["1"; "2"] in
  let join_row i j =
    if string_of_int i = j then Some (string_of_int i ^ " = " ^ j) else None
  in
  let s = S.join ~join_row s1 s2 in
  OUnit.assert_equal ["1 = 1"; "2 = 2"] (S.to_list s);
  ()

let test_scan () =
  1 -- 5 
    |> S.scan (+) 0
    |> S.to_list
    |> OUnit.assert_equal ~printer:pp_ilist [0;1;3;6;10;15]

let test_drop () =
  1 -- 5 |> S.drop 2 |> S.to_list |> OUnit.assert_equal [3;4;5]

let test_rev () =
  1 -- 5 |> S.rev |> S.to_list |> OUnit.assert_equal [5;4;3;2;1]

let test_unfoldr () =
  let f x = if x < 5 then Some (string_of_int x,x+1) else None in
  S.unfoldr f 0
    |> S.to_list
    |> OUnit.assert_equal ["0"; "1"; "2"; "3"; "4"]

let test_hashtbl () =
  let h = 1 -- 5
    |> S.zip_i
    |> S.to_hashtbl2 in
  0 -- 4
    |> S.iter (fun i -> OUnit.assert_equal (i+1) (Hashtbl.find h i));
  OUnit.assert_equal [0;1;2;3;4] (S.hashtbl_keys h |> S.sort |> S.to_list);
  ()

let test_buff () =
  let b = Buffer.create 4 in
  "hello world"
    |> S.of_str |> S.rev |> S.map Char.uppercase
    |> (fun seq -> S.to_buffer seq b);
  OUnit.assert_equal "DLROW OLLEH" (Buffer.contents b);
  ()

let test_int_range () =
  OUnit.assert_equal ~printer:pp_ilist [1;2;3;4] S.(to_list (1--4));
  OUnit.assert_equal ~printer:pp_ilist [10;9;8;7;6] S.(to_list (10 --^ 6));
  OUnit.assert_equal ~printer:pp_ilist [] S.(to_list (10--4));
  OUnit.assert_equal ~printer:pp_ilist [] S.(to_list (10 --^ 60));
  ()

let test_take () =
  let l = S.(to_list (take 0 (of_list [1]))) in
  OUnit.assert_equal ~printer:pp_ilist [] l;
  let l = S.(to_list (take 5 (of_list [1;2;3;4;5;6;7;8;9;10]))) in
  OUnit.assert_equal ~printer:pp_ilist [1;2;3;4;5] l;
  ()

let test_regression1 () =
  let s = S.(take 10 (repeat 1)) in
  OUnit.assert_bool "not empty" (not (S.is_empty s));
  ()

let suite =
  "test_sequence" >:::
    [ "test_empty" >:: test_empty;
      "test_repeat" >:: test_repeat;
      "test_concat" >:: test_concat;
      "test_fold" >:: test_fold;
      "test_foldi" >:: test_foldi;
      "test_exists" >:: test_exists;
      "test_length" >:: test_length;
      "test_concat" >:: test_concat;
      "test_flatMap" >:: test_flatMap;
      "test_intersperse" >:: test_intersperse;
      "test_not_persistent" >:: test_not_persistent;
      "test_persistent" >:: test_persistent;
      "test_big_persistent" >:: test_big_persistent;
      "test_sort" >:: test_sort;
      "test_sort_uniq" >:: test_sort;
      "test_group" >:: test_group;
      "test_uniq" >:: test_uniq;
      "test_product" >:: test_product;
      "test_join" >:: test_join;
      "test_scan" >:: test_scan;
      "test_drop" >:: test_drop;
      "test_rev" >:: test_rev;
      "test_unfoldr" >:: test_unfoldr;
      "test_hashtbl" >:: test_hashtbl;
      "test_int_range" >:: test_int_range;
      "test_take" >:: test_take;
      "test_regression1" >:: test_regression1
    ]
