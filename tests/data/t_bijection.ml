module Test = (val Containers_testlib.make ~__FILE__ ())
open Test
open CCBijection
module M = Make (CCInt) (String);;

eq 2 (M.of_list [ 1, "1"; 2, "2" ] |> M.cardinal);;
eq "1" (M.of_list [ 1, "1"; 2, "2" ] |> M.find_left 1);;
eq "2" (M.of_list [ 1, "1"; 2, "2" ] |> M.find_left 2);;
eq 1 (M.of_list [ 1, "1"; 2, "2" ] |> M.find_right "1");;
eq 2 (M.of_list [ 1, "1"; 2, "2" ] |> M.find_right "2")
