
open CCEither

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq (is_left (Left 1)) (true);;
eq (is_left (Right 1)) (false);;
eq (is_left (Left 1)) (true);;
eq (is_left (Right 1)) (false);;
eq (is_right (Left 1)) (false);;
eq (is_right (Right 1)) (true);;
eq (find_left (Left 1)) (Some 1);;
eq (find_left (Right 1)) (None);;
eq (find_right (Left 1)) (None);;
eq (find_right (Right 1)) (Some 1);;
