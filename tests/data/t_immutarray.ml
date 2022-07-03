

module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCImmutArray;;

let print_array f a = to_list a |> Array.of_list |> Q.Print.(array f);;

eq ~printer:(print_array Q.Print.int)
  (of_list [0]) (set (of_list [5]) 0 0);;
eq ~printer:(print_array Q.Print.int)
  (of_list [1; 3; 4; 5]) (set (of_list [1; 2; 4; 5]) 1 3);;

let eq' = eq ~printer:(print_array Q.Print.int) ;;
eq'  empty (append empty empty);;
eq'  (of_list [1; 2; 3]) (append empty (of_list [1; 2; 3]));;
eq'  (of_list [1; 2; 3]) (append (of_list [1; 2; 3]) empty);;
eq'  (of_list [3; 1; 4; 1; 5]) (append (of_list [3; 1]) (of_list [4; 1; 5]));;

eq ~printer:Q.Print.(list (pair int string))
  ([2, "baz"; 1, "bar"; 0, "foo"])
  (foldi (fun l i a -> (i, a) :: l) [] (of_list ["foo"; "bar"; "baz"]));;

let eq' = eq ~printer:Q.Print.bool;;
eq'  true (for_all (fun _ -> false) empty);;
eq'  false (for_all (fun _ -> false) (singleton 3));;
eq'  true (for_all (fun n -> n mod 2 = 0) (of_list [2; 4; 8]));;
eq'  false (for_all (fun n -> n mod 2 = 0) (of_list [2; 4; 5; 8]));;

eq'  false (exists (fun _ -> true) empty);;
eq'  true (exists (fun _ -> true) (singleton 3));;
eq'  false (exists (fun _ -> false) (singleton 3));;
eq'  false (exists (fun n -> n mod 2 = 1) (of_list [2; 4; 8]));;
eq'  true (exists (fun n -> n mod 2 = 1) (of_list [2; 4; 5; 8]));;

q Q.(list bool) (fun l -> let a = of_list l in not @@ exists (fun b -> b) a = for_all not a);;
q Q.(list bool) (fun l -> let a = of_list l in not @@ for_all (fun b -> b) a = exists not a);;

q Q.(list bool) (fun l -> exists (fun b -> b) (of_list l) = List.fold_left (||) false l);;
q Q.(list bool) (fun l -> for_all (fun b -> b) (of_list l) = List.fold_left (&&) true l);;

q Q.(list int) (fun l ->
    let g = Iter.of_list l in
    of_iter g |> to_iter |> Iter.to_list = l);;

q Q.(list int) (fun l ->
    let g = Gen.of_list l in
    of_gen g |> to_gen |> Gen.to_list = l);;
