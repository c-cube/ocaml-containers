open CCChar
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq (Some 'a') (of_int (to_int 'a'));;
eq None (of_int 257);;

q
  (Q.string_of_size (Q.Gen.return 1))
  (fun s -> Stdlib.( = ) (to_string s.[0]) s)
