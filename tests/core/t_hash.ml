open CCHash
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> int 42 >= 0;;
t @@ fun () -> int max_int >= 0;;
t @@ fun () -> int max_int = int max_int;;
t @@ fun () -> int min_int >= 0;;
t @@ fun () -> int 0 >= 0;;
t @@ fun () -> char 'c' >= 0;;
t @@ fun () -> int 152352 = int 152352;;
t @@ fun () -> list_comm int [ 1; 2 ] = list_comm int [ 2; 1 ];;
t @@ fun () -> list_comm int [ 1; 2 ] <> list_comm int [ 2; 3 ];;
t @@ fun () -> string "abcd" >= 0;;
t @@ fun () -> string "abc" <> string "abcd";;

q Q.int (fun i ->
    Q.assume (i >= 0);
    int i = int64 (Int64.of_int i))
