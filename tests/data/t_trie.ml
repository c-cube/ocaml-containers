module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCTrie
module T = MakeList (CCInt)
module S = String

let l1 =
  [ [ 1; 2 ], "12"; [ 1 ], "1"; [ 2; 1 ], "21"; [ 1; 2; 3 ], "123"; [], "[]" ]

let t1 = T.of_list l1
let small_l l = List.fold_left (fun acc (k, _) -> List.length k + acc) 0 l
let s1 = String.of_list [ "cat", 1; "catogan", 2; "foo", 3 ];;

t @@ fun () -> String.of_list [ "a", 1; "b", 2 ] |> String.size = 2;;
t @@ fun () -> String.of_list [ "a", 1; "b", 2; "a", 3 ] |> String.size = 2;;
t @@ fun () -> String.of_list [ "a", 1; "b", 2 ] |> String.find_exn "a" = 1;;
t @@ fun () -> String.of_list [ "a", 1; "b", 2 ] |> String.find_exn "b" = 2;;
t @@ fun () -> String.of_list [ "a", 1; "b", 2 ] |> String.find "c" = None;;
t @@ fun () -> s1 |> String.find_exn "cat" = 1;;
t @@ fun () -> s1 |> String.find_exn "catogan" = 2;;
t @@ fun () -> s1 |> String.find_exn "foo" = 3;;
t @@ fun () -> s1 |> String.find "cato" = None;;
t @@ fun () -> T.add [ 3 ] "3" t1 |> T.find_exn [ 3 ] = "3";;
t @@ fun () -> T.add [ 3 ] "3" t1 |> T.find_exn [ 1; 2 ] = "12";;
t @@ fun () -> T.remove [ 1; 2 ] t1 |> T.find [ 1; 2 ] = None;;
t @@ fun () -> T.remove [ 1; 2 ] t1 |> T.find [ 1 ] = Some "1";;
t @@ fun () -> T.remove [ 1; 2 ] t1 |> T.find [] = Some "[]";;
eq ~printer:CCFun.id "ca" (String.longest_prefix "carte" s1);;
eq ~printer:CCFun.id "" (String.longest_prefix "yolo" s1);;
eq ~printer:CCFun.id "cat" (String.longest_prefix "cat" s1);;
eq ~printer:CCFun.id "catogan" (String.longest_prefix "catogan" s1);;

q
  Q.(
    pair
      (list (pair (string_size_of Gen.(0 -- 30) Gen.char_printable) int))
      string_printable)
  (fun (l, s) ->
    let m = String.of_list l in
    let s' = String.longest_prefix s m in
    CCString.prefix ~pre:s' s)
;;

t @@ fun () ->
T.fold (fun acc k v -> (k, v) :: acc) [] t1
|> List.sort Stdlib.compare
= List.sort Stdlib.compare l1
;;

eq
  ~printer:Q.Print.(list (pair (list int) string))
  (List.map (fun (k, v) -> k, v ^ "!") l1 |> List.sort Stdlib.compare)
  (T.mapi (fun _ v -> v ^ "!") t1 |> T.to_list |> List.sort Stdlib.compare)
;;

eq
  ~printer:Q.Print.(list (pair (list int) string))
  (List.map (fun (k, v) -> k, v ^ "!") l1 |> List.sort Stdlib.compare)
  (T.map (fun v -> v ^ "!") t1 |> T.to_list |> List.sort Stdlib.compare)
;;

q ~count:30
  Q.(
    let p = list_size Gen.(0 -- 100) (pair string_printable nat_small) in
    pair p p)
  (fun (l1, l2) ->
    let t1 = S.of_list l1 and t2 = S.of_list l2 in
    let t = S.merge (fun a _ -> Some a) t1 t2 in
    S.to_iter t
    |> Iter.for_all (fun (k, v) -> S.find k t1 = Some v || S.find k t2 = Some v)
    && S.to_iter t1 |> Iter.for_all (fun (k, _) -> S.find k t <> None)
    && S.to_iter t2 |> Iter.for_all (fun (k, _) -> S.find k t <> None))
;;

t @@ fun () -> T.size t1 = List.length l1

let eq' = eq ~printer:CCFormat.(to_string (list (pair (list int) string)));;

eq'
  [ [ 1 ], "1"; [ 1; 2 ], "12"; [ 1; 2; 3 ], "123"; [ 2; 1 ], "21" ]
  (T.above [ 1 ] t1 |> Iter.to_list)
;;

eq'
  [ [ 1; 2 ], "12"; [ 1; 2; 3 ], "123"; [ 2; 1 ], "21" ]
  (T.above [ 1; 1 ] t1 |> Iter.to_list)
;;

eq'
  [ [ 1; 2 ], "12"; [ 1 ], "1"; [], "[]" ]
  (T.below [ 1; 2 ] t1 |> Iter.to_list)
;;

eq' [ [ 1 ], "1"; [], "[]" ] (T.below [ 1; 1 ] t1 |> Iter.to_list);;

(* NOTE: Regression test. See #158 *)
t @@ fun () ->
let module TPoly = Make (struct
  type t = (unit -> char) list
  type char_ = char

  let compare = compare
  let to_iter a k = List.iter (fun c -> k (c ())) a
  let of_list l = List.map (fun c () -> c) l
end) in
let trie = TPoly.of_list [ [ (fun () -> 'a') ], 1; [ (fun () -> 'b') ], 2 ] in
ignore (TPoly.below [ (fun () -> 'a') ] trie |> Iter.to_list);
true
;;

q ~count:30
  Q.(list_size Gen.(0 -- 100) (pair string_printable nat_small))
  (fun l ->
    let t = S.of_list l in
    S.check_invariants t)

let rec sorted ~rev = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as tl) ->
    (if rev then
       x >= y
     else
       x <= y)
    && sorted ~rev tl

let gen_str = Q.(string_size_of Gen.nat_small Gen.char_printable);;

q ~count:200
  Q.(list_size Gen.(1 -- 20) (pair gen_str nat_small))
  (fun l ->
    let t = String.of_list l in
    List.for_all
      (fun (k, _) -> String.above k t |> Iter.for_all (fun (k', _) -> k' >= k))
      l)
;;

q ~count:200
  Q.(list_size Gen.(1 -- 20) (pair gen_str nat_small))
  (fun l ->
    let t = String.of_list l in
    List.for_all
      (fun (k, _) -> String.below k t |> Iter.for_all (fun (k', _) -> k' <= k))
      l)
;;

q ~count:200
  Q.(list_size Gen.(1 -- 20) (pair gen_str nat_small))
  (fun l ->
    let t = String.of_list l in
    List.for_all
      (fun (k, _) -> String.above k t |> Iter.to_list |> sorted ~rev:false)
      l)
;;

q ~count:200
  Q.(list_size Gen.(1 -- 20) (pair gen_str nat_small))
  (fun l ->
    let t = String.of_list l in
    List.for_all
      (fun (k, _) -> String.below k t |> Iter.to_list |> sorted ~rev:true)
      l)
