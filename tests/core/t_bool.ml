open CCBool
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq 1 (to_int true);;
eq 0 (to_int false);;
eq true (of_int 1);;
eq false (of_int 0);;
eq true (of_int 42);;
eq true (of_int max_int);;
eq true (of_int min_int);;
eq (Some "true") (if_then (Fun.const "true") true);;
eq None (if_then (Fun.const "true") false);;
eq "true" (if_then_else (Fun.const "true") (Fun.const "false") true);;
eq "false" (if_then_else (Fun.const "true") (Fun.const "false") false)
