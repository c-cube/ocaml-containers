module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCLazy_list;;

q Q.(list int) (fun l -> length (of_list l) = List.length l);;

eq [ 2; 4; 6 ]
  (of_list [ 1; 2; 3; 4; 5; 6; 7 ]
  |> filter ~f:(fun x -> x mod 2 = 0)
  |> to_list)
;;

eq [ 2; 4; 6 ]
  (of_gen Gen.(1 -- max_int)
  |> filter ~f:(fun x -> x mod 2 = 0)
  |> take 3 |> to_list)
;;

q
  Q.(pair (list int) (list int))
  (fun (l1, l2) ->
    length (append (of_list l1) (of_list l2)) = List.length l1 + List.length l2)
;;

eq [ 1 ] (default ~default:(return 1) empty |> to_list);;
q Q.(list int) (fun l -> l = (Gen.of_list l |> of_gen |> to_list));;
q Q.(list int) (fun l -> l = to_list (of_list l));;
q Q.(list int) (fun l -> l = (of_list l |> to_gen |> Gen.to_list))
