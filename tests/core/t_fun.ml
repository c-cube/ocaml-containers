[@@@ocaml.warning "-33"]

open CCFun
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq ~printer:Q.Print.int 10 (iterate 0 succ 10);;
eq ~printer:Q.Print.int 11 (iterate 1 succ 10);;
eq ~printer:Q.Print.int 12 (iterate 2 succ 10);;
eq ~printer:Q.Print.int 15 (iterate 5 succ 10);;

t @@ fun () ->
assert_raises
  (function
    | Invalid_argument _ -> true
    | _ -> false)
  (fun () -> iterate (-1) succ 10);
true
;;

t @@ fun () -> CCFun.((succ %> string_of_int) 2 = "3");;
t @@ fun () -> CCFun.((( * ) 3 % succ) 5 = 18);;
t @@ fun () -> CCFun.(succ @@ ( * ) 2 @@ pred @@ 3 = 5)

let find_array arr x =
  let@ return = with_return in
  for i = 0 to Array.length arr - 1 do
    if arr.(i) = x then return i
  done;
  -1
;;

eq 1 @@ find_array [| "a"; "b"; "c" |] "b";;
eq 2 @@ find_array [| "a"; "b"; "c" |] "c";;
eq (-1) @@ find_array [| "a"; "b"; "c" |] "hello"
