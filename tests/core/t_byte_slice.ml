module T = (val Containers_testlib.make ~__FILE__ ())
include T
open CCByte_slice;;

t @@ fun () ->
  let bs = Bytes.of_string "hello" in
  let sl = create bs in
  len sl = 5
;;

t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create ~off:6 bs in
  len sl = 5
;;

t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create ~off:6 ~len:3 bs in
  len sl = 3
;;

(* Test unsafe_of_string *)
t @@ fun () ->
  let sl = unsafe_of_string "hello" in
  len sl = 5
;;

t @@ fun () ->
  let sl = unsafe_of_string ~off:2 ~len:3 "hello" in
  len sl = 3
;;

(* Test len *)
eq 5 (len (create (Bytes.of_string "hello")));;
eq 0 (len (create ~len:0 (Bytes.of_string "hello")));;
eq 3 (len (create ~off:2 ~len:3 (Bytes.of_string "hello")));;

(* Test get *)
t @@ fun () ->
  let sl = create (Bytes.of_string "hello") in
  get sl 0 = 'h' && get sl 4 = 'o'
;;

t @@ fun () ->
  let sl = create ~off:2 ~len:3 (Bytes.of_string "hello") in
  get sl 0 = 'l' && get sl 2 = 'o'
;;

t @@ fun () ->
  let sl = unsafe_of_string "world" in
  get sl 0 = 'w' && get sl 4 = 'd'
;;

(* Test get out of bounds *)
t @@ fun () ->
  let sl = create (Bytes.of_string "hi") in
  try
    ignore (get sl 2);
    false
  with Invalid_argument _ -> true
;;

t @@ fun () ->
  let sl = create (Bytes.of_string "hi") in
  try
    ignore (get sl (-1));
    false
  with Invalid_argument _ -> true
;;

(* Test set *)
t @@ fun () ->
  let bs = Bytes.of_string "hello" in
  let sl = create bs in
  set sl 0 'H';
  get sl 0 = 'H' && Bytes.get bs 0 = 'H'
;;

t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create ~off:6 ~len:5 bs in
  set sl 0 'W';
  get sl 0 = 'W' && Bytes.get bs 6 = 'W'
;;

(* Test set out of bounds *)
t @@ fun () ->
  let sl = create (Bytes.of_string "hi") in
  try
    set sl 2 'x';
    false
  with Invalid_argument _ -> true
;;

(* Test consume *)
t @@ fun () ->
  let bs = Bytes.of_string "hello" in
  let sl = create bs in
  consume sl 2;
  len sl = 3 && get sl 0 = 'l'
;;

t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create ~off:0 ~len:5 bs in
  consume sl 2;
  len sl = 3 && get sl 0 = 'l' && sl.off = 2
;;

t @@ fun () ->
  let bs = Bytes.of_string "test" in
  let sl = create bs in
  consume sl 4;
  len sl = 0
;;

(* Test contents *)
eq "hello" (contents (create (Bytes.of_string "hello")));;
eq "world" (contents (create ~off:6 (Bytes.of_string "hello world")));;
eq "ell" (contents (create ~off:1 ~len:3 (Bytes.of_string "hello")));;

t @@ fun () ->
  let bs = Bytes.of_string "hello" in
  let sl = create bs in
  let c = contents sl in
  (* Modifying the slice should not affect the returned string *)
  set sl 0 'H';
  c = "hello"
;;

t @@ fun () ->
  let sl = create (Bytes.of_string "test") in
  consume sl 2;
  contents sl = "st"
;;

(* Test sub *)
t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create bs in
  let sub_sl = sub sl 0 5 in
  len sub_sl = 5 && get sub_sl 0 = 'h'
;;

t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create bs in
  let sub_sl = sub sl 6 5 in
  len sub_sl = 5 && get sub_sl 0 = 'w'
;;

t @@ fun () ->
  let bs = Bytes.of_string "hello world" in
  let sl = create ~off:6 ~len:5 bs in
  let sub_sl = sub sl 0 3 in
  len sub_sl = 3 && get sub_sl 0 = 'w' && contents sub_sl = "wor"
;;

(* Test that sub shares the underlying bytes *)
t @@ fun () ->
  let bs = Bytes.of_string "hello" in
  let sl = create bs in
  let sub_sl = sub sl 1 3 in
  set sub_sl 0 'E';
  get sl 1 = 'E'
;;

(* Property-based tests *)
q Q.(string_of_size (Gen.int_range 1 100)) (fun s ->
    let bs = Bytes.of_string s in
    let sl = create bs in
    contents sl = s
);;

q Q.(string_of_size (Gen.int_range 1 100)) (fun s ->
    let bs = Bytes.of_string s in
    let sl = create bs in
    len sl = String.length s
);;

q Q.(pair (string_of_size (Gen.int_range 5 100)) small_nat) (fun (s, n) ->
    let bs = Bytes.of_string s in
    let sl = create bs in
    let n = min n (len sl) in
    consume sl n;
    len sl = String.length s - n
);;

q Q.(string_of_size (Gen.int_range 10 100)) (fun s ->
    let bs = Bytes.of_string s in
    let sl = create bs in
    let mid = String.length s / 2 in
    let sub1 = sub sl 0 mid in
    let sub2 = sub sl mid (String.length s - mid) in
    contents sub1 ^ contents sub2 = s
);;
