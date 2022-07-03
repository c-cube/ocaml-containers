open CCInt
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

eq ~printer:Q.Print.(list int) [ 0; 1; 2; 3; 4; 5 ] (range 0 5 |> Iter.to_list)
;;
eq ~printer:Q.Print.(list int) [ 0 ] (range 0 0 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [ 5; 4; 3; 2 ] (range 5 2 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [] (range' 0 0 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [ 0; 1; 2; 3; 4 ] (range' 0 5 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [ 5; 4; 3 ] (range' 5 2 |> Iter.to_list);;
t @@ fun () -> pow 2 10 = 1024;;
t @@ fun () -> pow 2 15 = 32768;;
t @@ fun () -> pow 10 5 = 100000;;
t @@ fun () -> pow 1 0 = 1;;
t @@ fun () -> pow 0 1 = 0;;
t @@ fun () -> floor_div 3 5 = 0;;
t @@ fun () -> floor_div 5 5 = 1;;
t @@ fun () -> floor_div 20 5 = 4;;
t @@ fun () -> floor_div 12 5 = 2;;
t @@ fun () -> floor_div 0 5 = 0;;
t @@ fun () -> floor_div (-1) 5 = -1;;
t @@ fun () -> floor_div (-5) 5 = -1;;
t @@ fun () -> floor_div (-12) 5 = -3;;
t @@ fun () -> floor_div 0 (-5) = 0;;
t @@ fun () -> floor_div 3 (-5) = -1;;
t @@ fun () -> floor_div 5 (-5) = -1;;
t @@ fun () -> floor_div 9 (-5) = -2;;
t @@ fun () -> floor_div 20 (-5) = -4;;
t @@ fun () -> floor_div (-2) (-5) = 0;;
t @@ fun () -> floor_div (-8) (-5) = 1;;
t @@ fun () -> floor_div (-35) (-5) = 7;;

t @@ fun () ->
try
  ignore (floor_div 12 0);
  false
with Division_by_zero -> true
;;

t @@ fun () ->
try
  ignore (floor_div (-12) 0);
  false
with Division_by_zero -> true
;;

q (Q.pair Q.small_signed_int Q.pos_int) (fun (n, m) ->
    floor_div n m = int_of_float @@ floor (float n /. float m))
;;

q (Q.pair Q.small_signed_int Q.pos_int) (fun (n, m) ->
    floor_div n (-m) = int_of_float @@ floor (float n /. float (-m)))
;;

t @@ fun () -> rem 3 5 = 3;;
t @@ fun () -> rem 5 5 = 0;;
t @@ fun () -> rem 9 5 = 4;;
t @@ fun () -> rem (-1) 5 = 4;;
t @@ fun () -> rem (-5) 5 = 0;;
t @@ fun () -> rem (-20) 5 = 0;;
t @@ fun () -> rem (-9) 5 = 1;;
t @@ fun () -> rem 0 5 = 0;;
t @@ fun () -> rem 0 (-5) = 0;;
t @@ fun () -> rem 3 (-5) = -2;;
t @@ fun () -> rem 5 (-5) = 0;;
t @@ fun () -> rem 9 (-5) = -1;;
t @@ fun () -> rem (-2) (-5) = -2;;
t @@ fun () -> rem (-8) (-5) = -3;;
t @@ fun () -> rem (-35) (-5) = 0;;

t @@ fun () ->
try
  ignore (rem 12 0);
  false
with Division_by_zero -> true
;;

t @@ fun () ->
try
  ignore (rem (-12) 0);
  false
with Division_by_zero -> true
;;

q (Q.pair Q.int Q.pos_int) (fun (n, m) ->
    let y = rem n m in
    y >= 0 && y < m)
;;

q (Q.pair Q.int Q.pos_int) (fun (n, m) ->
    let y = rem n (-m) in
    y > -m && y <= 0)
;;

q (Q.pair Q.int Q.pos_int) (fun (n, m) -> n = (m * floor_div n m) + rem n m);;

q (Q.pair Q.int Q.pos_int) (fun (n, m) ->
    n = (-m * floor_div n (-m)) + rem n (-m))
;;

eq None (of_string "moo");;
eq (Some 42) (of_string "42");;
eq 1 (of_float 1.2);;
eq ~printer:CCFun.id "0b111" (to_string_binary 7);;
eq ~printer:CCFun.id "-0b111" (to_string_binary (-7));;
eq ~printer:CCFun.id "0b0" (to_string_binary 0);;
q ~count:10_000 Q.int (fun n -> n = int_of_string (to_string_binary n));;

(* note: the last test checks that no error occurs due to overflows. *)

eq ~printer:Q.Print.(list int) [ 0 ] (range_by ~step:1 0 0 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [] (range_by ~step:1 5 0 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [] (range_by ~step:2 1 0 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [ 0; 2; 4 ] (range_by ~step:2 0 4 |> Iter.to_list)
;;
eq ~printer:Q.Print.(list int) [ 0; 2; 4 ] (range_by ~step:2 0 5 |> Iter.to_list)
;;
eq ~printer:Q.Print.(list int) [ 0 ] (range_by ~step:~-1 0 0 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [] (range_by ~step:~-1 0 5 |> Iter.to_list);;
eq ~printer:Q.Print.(list int) [] (range_by ~step:~-2 0 1 |> Iter.to_list);;

eq
  ~printer:Q.Print.(list int)
  [ 5; 3; 1 ]
  (range_by ~step:~-2 5 1 |> Iter.to_list)
;;

eq
  ~printer:Q.Print.(list int)
  [ 5; 3; 1 ]
  (range_by ~step:~-2 5 0 |> Iter.to_list)
;;

eq ~printer:Q.Print.(list int) [ 0 ] (range_by ~step:max_int 0 2 |> Iter.to_list)
;;

q
  Q.(pair small_int small_int)
  (fun (i, j) ->
    let i = min i j and j = max i j in
    CCList.equal CCInt.equal
      (CCInt.range_by ~step:1 i j |> Iter.to_list)
      (CCInt.range i j |> Iter.to_list))
;;

eq 0 (popcount 0);;
eq 1 (popcount 1);;
eq (Sys.word_size - 2) (popcount max_int);;
eq 1 (popcount min_int);;
eq 10 (popcount 0b1110010110110001010);;
eq 5 (popcount 0b1101110000000000)

let simple_popcnt i =
  let rec loop n i =
    if i = 0 then
      n
    else if i land 0b1 = 1 then
      loop (n + 1) (i lsr 1)
    else
      loop n (i lsr 1)
  in
  loop 0 i
;;

eq 0 (simple_popcnt 0);;
eq 1 (simple_popcnt 1);;
eq (Sys.word_size - 2) (simple_popcnt max_int);;
eq 1 (simple_popcnt min_int);;
eq 5 (simple_popcnt 0b1101110000000000);;

q ~count:3_000 ~long_factor:10
  Q.(
    let g = int in
    set_gen (Gen.graft_corners g.gen [ min_int; max_int; 0; -1; 1 ] ()) g)
  (fun i ->
    if simple_popcnt i <> popcount i then
      Q.Test.fail_reportf "on %d: simple-popcount=%d, popcount=%d" i
        (simple_popcnt i) (popcount i);
    true)
