

module Test = (val Containers_testlib.make ~__FILE__())
open Test
open CCSimple_queue;;

q Q.(list small_int) (fun l ->
    let q = of_list l in
    equal CCInt.equal (Gen.unfold pop q |> of_gen) q);;

q Q.(list small_int) (fun l ->
    equal CCInt.equal (of_list l |> rev) (of_list (List.rev l)));;
q Q.(list small_int) (fun l ->
    let q = of_list l in
    equal CCInt.equal q (q |> rev |> rev));;

q Q.(list small_int)(fun l ->
    length (of_list l) = List.length l);;

q Q.(list small_int)(fun l ->
    equal CCInt.equal (of_list l) (List.fold_left snoc empty l));;

q Q.(list small_int) (fun l ->
    equal CCInt.equal
      (of_iter (Iter.of_list l))
      (of_list l));;
q Q.(list small_int) (fun l ->
    l = (of_list l |> to_iter |> Iter.to_list));;

q Q.(pair (list small_int)(list small_int)) (fun (l1,l2) ->
    equal CCInt.equal (of_list l1)(of_list l2) = (l1=l2));;

q Q.(pair (list small_int)(list small_int)) (fun (l1,l2) ->
    equal CCInt.equal
      (append (of_list l1)(of_list l2))
      (of_list (List.append l1 l2)));;
