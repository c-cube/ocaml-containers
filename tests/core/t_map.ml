module T = (val Containers_testlib.make ~__FILE__ ())
include T
open CCMap
module M = CCMap.Make (String)

let eq' = eq ~printer:CCFormat.(to_string @@ Dump.(list (pair string int)));;

eq'
  [ "a", 1; "b", 20 ]
  (M.of_list [ "b", 2; "c", 3 ]
  |> M.update "a" (function _ -> Some 1)
  |> M.update "c" (fun _ -> None)
  |> M.update "b" (CCOption.map (fun x -> x * 10))
  |> M.to_list |> List.sort CCOrd.poly)

module M2 = Make (CCInt);;

q
  Q.(list (pair small_int small_int))
  M2.(
    fun l -> to_list (of_list l) = to_list (of_list_with ~f:(fun _ v _ -> v) l))
;;

q
  Q.(list (pair small_int small_int))
  M2.(
    fun l ->
      to_list (of_iter @@ Iter.of_list l)
      = to_list (of_iter_with ~f:(fun _ v _ -> v) @@ Iter.of_list l))
;;

q
  Q.(list (pair small_int small_int))
  M2.(
    fun l ->
      to_list (of_seq @@ CCSeq.of_list l)
      = to_list (of_seq_with ~f:(fun _ v _ -> v) @@ CCSeq.of_list l))
