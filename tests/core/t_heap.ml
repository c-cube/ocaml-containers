open CCHeap
module T = (val Containers_testlib.make ~__FILE__ ())
include T

module H = CCHeap.Make (struct
  type t = int

  let leq x y = x <= y
end)

let rec is_sorted l =
  match l with
  | [ _ ] | [] -> true
  | x :: (y :: _ as l') -> x <= y && is_sorted l'

let extract_list = H.to_list_sorted;;

t @@ fun () ->
let h = H.of_list [ 5; 3; 4; 1; 42; 0 ] in
let h, x = H.take_exn h in
assert_equal ~printer:string_of_int 0 x;
let h, x = H.take_exn h in
assert_equal ~printer:string_of_int 1 x;
let h, x = H.take_exn h in
assert_equal ~printer:string_of_int 3 x;
let h, x = H.take_exn h in
assert_equal ~printer:string_of_int 4 x;
let h, x = H.take_exn h in
assert_equal ~printer:string_of_int 5 x;
let h, x = H.take_exn h in
assert_equal ~printer:string_of_int 42 x;
assert_raises
  (function
    | H.Empty -> true
    | _ -> false)
  (fun () -> H.take_exn h);
true
;;

q ~count:30
  Q.(list_of_size Gen.(return 1_000) int)
  (fun l ->
    (* put elements into a heap *)
    let h = H.of_iter (Iter.of_list l) in
    assert_equal 1_000 (H.size h);
    let l' = extract_list h in
    is_sorted l')
;;

(* test filter *)
q ~count:30
  Q.(list_of_size Gen.(return 1_000) int)
  (fun l ->
    (* put elements into a heap *)
    let h = H.of_iter (Iter.of_list l) in
    let h = H.filter (fun x -> x mod 2 = 0) h in
    assert (H.to_iter h |> Iter.for_all (fun x -> x mod 2 = 0));
    let l' = extract_list h in
    is_sorted l')
;;

q
  Q.(list_of_size Gen.(return 1_000) int)
  (fun l ->
    (* put elements into a heap *)
    let h = H.of_iter (Iter.of_list l) in
    let l' = H.to_iter_sorted h |> Iter.to_list in
    is_sorted l')
;;

q
  Q.(list int)
  (fun l ->
    extract_list (H.of_list l) = extract_list (H.of_gen (CCList.to_gen l)))
;;

q
  Q.(list int)
  (fun l ->
    let h = H.of_list l in
    H.to_gen h |> CCList.of_gen |> List.sort Stdlib.compare
    = (H.to_list h |> List.sort Stdlib.compare))
;;

q
  Q.(list int)
  (fun l ->
    let h = H.of_list l in
    H.to_string string_of_int h
    = (List.sort Stdlib.compare l |> List.map string_of_int |> String.concat ","))
;;

q
  Q.(list int)
  (fun l ->
    let h = H.of_list l in
    H.to_string ~sep:" " string_of_int h
    = (List.sort Stdlib.compare l |> List.map string_of_int |> String.concat " "))
;;

q
  Q.(list_of_size Gen.(return 1_000) int)
  (fun l ->
    let module H' = Make_from_compare (CCInt) in
    let h = H'.of_list l in
    let l' = H'.to_list_sorted h in
    is_sorted l')
