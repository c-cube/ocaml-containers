open CCHash
module T = (val Containers_testlib.make ~__FILE__ ())
include T

open struct
  let hash_ocaml64 (n : int64) : int =
    let offset_basis = 0xcbf29ce484222325L in
    let prime = 0x100000001b3L in
    let h = ref offset_basis in
    for k = 0 to 7 do
      (h := Int64.(logxor !h (Int64.logand (Int64.shift_left n (k * 8)) 0xffL)));
      h := Int64.(mul !h prime)
    done;
    Int64.to_int !h land max_int
end
;;

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
;;

q Q.int64
  Q.(fun i -> Int64.compare i 0L >= 0 ==> (CCInt64.hash i = hash_ocaml64 i))
