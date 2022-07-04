open CCBool
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq 1 (to_int true);;
eq 0 (to_int false);;
eq true (of_int 1);;
eq false (of_int 0);;
eq true (of_int 42);;
eq true (of_int max_int);;
eq true (of_int min_int)
