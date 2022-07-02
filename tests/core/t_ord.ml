
open CCOrd

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> equiv 1 2;;
t @@ fun () -> equiv ~-1 ~-10;;
t @@ fun () -> equiv 0 0;;
t @@ fun () -> equiv ~-1 ~-1;;
t @@ fun () -> not (equiv 0 1);;
t @@ fun () -> not (equiv 1 ~-1);;
t @@ fun () -> not (equiv 1 0);;

q Q.(pair int int) (fun (x,y) ->
    (equiv x y) = (equiv y x));;
q Q.(triple int int int) (fun (x,y,z) ->
    if (equiv x y && equiv y z) then (equiv x z) else true);;

t @@ fun () -> bool true false > 0;;
t @@ fun () -> bool false true < 0;;
t @@ fun () -> bool true true = 0;;
t @@ fun () -> bool false false = 0;;

q Q.(option int) (fun o -> option int None o <= 0);;

t @@ fun () -> pair int string (1, "b") (2, "a") < 0;;
t @@ fun () -> pair int string (1, "b") (0, "a") > 0;;
t @@ fun () -> pair int string (1, "b") (1, "b") = 0;;
t @@ fun () -> list int [1;2;3] [1;2;3;4] < 0;;
t @@ fun () -> list int [1;2;3;4] [1;2;3] > 0;;
t @@ fun () -> list int [1;2;3;4] [1;3;4] < 0;;

q Q.(pair (list int)(list int)) CCOrd.(fun (l1,l2) ->
  equiv (list int l1 l2) (Stdlib.compare l1 l2));;

t @@ fun () -> array int [|1;2;3|] [|1;2;3;4|] < 0;;
t @@ fun () -> array int [|1;2;3;4|] [|1;2;3|] > 0;;
t @@ fun () -> array int [|1;2;3;4|] [|1;3;4|] < 0;;

q
  Q.(pair (array int)(array int)) CCOrd.(fun (a1,a2) ->
    equiv (array int a1 a2) (list int (Array.to_list a1) (Array.to_list a2)));;
