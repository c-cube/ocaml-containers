open CCPair
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> make 1 2 = (1, 2);;
t @@ fun () -> fst (make 'a' 'b') = 'a';;
t @@ fun () -> snd (make 'a' 'b') = 'b';;

(* Test map_fst *)
eq (2, "hello") (map_fst (( + ) 1) (1, "hello"));;
eq ('B', 5) (map_fst Char.uppercase_ascii ('b', 5));;
t @@ fun () -> map_fst (fun x -> x * 2) (3, "x") = (6, "x");;

(* Test map_snd *)
eq (1, "HELLO") (map_snd String.uppercase_ascii (1, "hello"));;
eq (5, 'B') (map_snd Char.uppercase_ascii (5, 'b'));;
t @@ fun () -> map_snd (fun x -> x * 2) ("x", 3) = ("x", 6);;

(* Test map *)
eq (2, "HELLO") (map (( + ) 1) String.uppercase_ascii (1, "hello"));;
t @@ fun () -> map (fun x -> x + 1) (fun y -> y * 2) (5, 10) = (6, 20);;

(* Test map_same *)
eq (2, 4) (map_same (fun x -> x * 2) (1, 2));;
eq (6, 8) (map_same (( + ) 1) (5, 7));;

(* Test map2 *)
eq (7, 11)
  (map2 ( + ) ( * ) (2, 3) (5, 4))
;;
t @@ fun () -> map2 ( + ) ( - ) (1, 10) (2, 5) = (3, 5);;

(* Test map_same2 *)
eq (3, 12) (map_same2 ( + ) (1, 2) (2, 10));;
eq (5, 7) (map_same2 ( * ) (1, 1) (5, 7));;

(* Test fst_map and snd_map *)
eq 2 (fst_map (( + ) 1) (1, "hello"));;
eq "HELLO" (snd_map String.uppercase_ascii (1, "hello"));;
t @@ fun () -> fst_map (fun x -> x * 2) (5, true) = 10;;
t @@ fun () -> snd_map (fun x -> x * 2) (true, 5) = 10;;

(* Test iter *)
t @@ fun () ->
  let r = ref 0 in
  iter (fun a b -> r := a + b) (3, 7);
  !r = 10
;;

(* Test swap *)
eq (2, 1) (swap (1, 2));;
eq ("world", "hello") (swap ("hello", "world"));;
t @@ fun () -> swap (swap (1, 2)) = (1, 2);;

(* Test operators *)
eq (2, "hello") ((( + ) 1) <<< (1, "hello"));;
eq (1, "HELLO") (String.uppercase_ascii >>> (1, "hello"));;
eq (2, "HELLO") ((( + ) 1 *** String.uppercase_ascii) (1, "hello"));;

(* Test &&& operator *)
t @@ fun () -> ((( + ) 1) &&& (( * ) 2)) 5 = (6, 10);;
t @@ fun () -> (String.length &&& String.uppercase_ascii) "hello" = (5, "HELLO");;

(* Test merge/fold *)
eq 3 (merge ( + ) (1, 2));;
eq 10 (fold ( * ) (2, 5));;
eq "HelloWorld" (merge ( ^ ) ("Hello", "World"));;

(* Test dup *)
eq (5, 5) (dup 5);;
eq ("x", "x") (dup "x");;
t @@ fun () -> let (a, b) = dup 42 in a = b;;

(* Test dup_map *)
eq (5, 10) (dup_map (( * ) 2) 5);;
eq ("hello", "HELLO") (dup_map String.uppercase_ascii "hello");;
t @@ fun () -> dup_map (fun x -> x + 1) 5 = (5, 6);;

(* Test equal *)
t @@ fun () -> equal Int.equal String.equal (1, "a") (1, "a");;
t @@ fun () -> not (equal Int.equal String.equal (1, "a") (1, "b"));;
t @@ fun () -> not (equal Int.equal String.equal (1, "a") (2, "a"));;

(* Test compare *)
t @@ fun () -> compare Int.compare String.compare (1, "a") (1, "a") = 0;;
t @@ fun () -> compare Int.compare String.compare (1, "a") (1, "b") < 0;;
t @@ fun () -> compare Int.compare String.compare (1, "b") (1, "a") > 0;;
t @@ fun () -> compare Int.compare String.compare (1, "x") (2, "x") < 0;;
t @@ fun () -> compare Int.compare String.compare (2, "x") (1, "x") > 0;;

(* Test to_string *)
eq "1,hello" (to_string Int.to_string CCFun.id (1, "hello"));;
eq "5::10" (to_string ~sep:"::" Int.to_string Int.to_string (5, 10));;
eq "true-false" (to_string ~sep:"-" Bool.to_string Bool.to_string (true, false));;

(* Property tests with QCheck *)
q Q.(pair int int) (fun p -> swap (swap p) = p);;

q Q.(pair int string) (fun p ->
    map_fst CCFun.id p = map_fst (fun x -> x) p
);;

q Q.(pair int string) (fun p ->
    map_snd CCFun.id p = map_snd (fun x -> x) p
);;

q Q.(pair int int) (fun (a, b) ->
    merge ( + ) (a, b) = a + b
);;

q Q.int (fun x ->
    dup x = (x, x)
);;

q Q.(pair int int) (fun p ->
    equal Int.equal Int.equal p p
);;

q Q.(pair int int) (fun p ->
    compare Int.compare Int.compare p p = 0
);;

q Q.(triple int int int) (fun (a, b, c) ->
    let p1 = (a, b) in
    let p2 = (a, c) in
    if b = c then
      equal Int.equal Int.equal p1 p2
    else
      not (equal Int.equal Int.equal p1 p2)
);;

q Q.(pair small_int small_int) (fun (a, b) ->
    let p1 = (a, b) in
    let p2 = (b, a) in
    if a = b then
      equal Int.equal Int.equal p1 p2
    else
      not (equal Int.equal Int.equal p1 p2)
);;
