open CCInt64
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> pow 2L 10L = 1024L;;
t @@ fun () -> pow 2L 15L = 32768L;;
t @@ fun () -> pow 10L 5L = 100000L;;
t @@ fun () -> pow 42L 0L = 1L;;
t @@ fun () -> pow 0L 1L = 0L;;
t @@ fun () -> floor_div 3L 5L = 0L;;
t @@ fun () -> floor_div 5L 5L = 1L;;
t @@ fun () -> floor_div 20L 5L = 4L;;
t @@ fun () -> floor_div 12L 5L = 2L;;
t @@ fun () -> floor_div 0L 5L = 0L;;
t @@ fun () -> floor_div (-1L) 5L = -1L;;
t @@ fun () -> floor_div (-5L) 5L = -1L;;
t @@ fun () -> floor_div (-12L) 5L = -3L;;
t @@ fun () -> floor_div 0L (-5L) = 0L;;
t @@ fun () -> floor_div 3L (-5L) = -1L;;
t @@ fun () -> floor_div 5L (-5L) = -1L;;
t @@ fun () -> floor_div 9L (-5L) = -2L;;
t @@ fun () -> floor_div 20L (-5L) = -4L;;
t @@ fun () -> floor_div (-2L) (-5L) = 0L;;
t @@ fun () -> floor_div (-8L) (-5L) = 1L;;
t @@ fun () -> floor_div (-35L) (-5L) = 7L;;

t @@ fun () ->
try
  ignore (floor_div 12L 0L);
  false
with Division_by_zero -> true
;;

t @@ fun () ->
try
  ignore (floor_div (-12L) 0L);
  false
with Division_by_zero -> true
;;

q
  (Q.pair (Q.map of_int Q.int_small) (Q.map of_int Q.nat_small))
  (fun (n, m) ->
    let m = m + 1L in
    floor_div n m = of_float @@ floor (to_float n /. to_float m))
;;

q
  (Q.pair (Q.map of_int Q.int_small) (Q.map of_int Q.nat_small))
  (fun (n, m) ->
    let m = m + 1L in
    floor_div n (-m) = of_float @@ floor (to_float n /. to_float (-m)))
;;

eq
  ~printer:Q.Print.(list to_string)
  [ 0L; 1L; 2L; 3L; 4L; 5L ]
  (range 0L 5L |> Iter.to_list)
;;

eq ~printer:Q.Print.(list to_string) [ 0L ] (range 0L 0L |> Iter.to_list);;

eq
  ~printer:Q.Print.(list to_string)
  [ 5L; 4L; 3L; 2L ]
  (range 5L 2L |> Iter.to_list)

(* note: the last test checks that no error occurs due to overflows. *)
let eq' = eq ~printer:Q.Print.(list to_string);;

eq' [ 0L ] (range_by ~step:1L 0L 0L |> Iter.to_list);;
eq' [] (range_by ~step:1L 5L 0L |> Iter.to_list);;
eq' [] (range_by ~step:2L 1L 0L |> Iter.to_list);;
eq' [ 0L; 2L; 4L ] (range_by ~step:2L 0L 4L |> Iter.to_list);;
eq' [ 0L; 2L; 4L ] (range_by ~step:2L 0L 5L |> Iter.to_list);;
eq' [ 0L ] (range_by ~step:(neg 1L) 0L 0L |> Iter.to_list);;
eq' [] (range_by ~step:(neg 1L) 0L 5L |> Iter.to_list);;
eq' [] (range_by ~step:(neg 2L) 0L 1L |> Iter.to_list);;
eq' [ 5L; 3L; 1L ] (range_by ~step:(neg 2L) 5L 1L |> Iter.to_list);;
eq' [ 5L; 3L; 1L ] (range_by ~step:(neg 2L) 5L 0L |> Iter.to_list);;
eq' [ 0L ] (range_by ~step:max_int 0L 2L |> Iter.to_list);;

q
  Q.(pair (map of_int nat_small) (map of_int nat_small))
  (fun (i, j) ->
    let i = min i j and j = max i j in
    CCList.equal CCInt64.equal
      (CCInt64.range_by ~step:1L i j |> Iter.to_list)
      (CCInt64.range i j |> Iter.to_list))
;;

eq ~printer:CCFun.id "0b111" (to_string_binary 7L);;
eq ~printer:CCFun.id "-0b111" (to_string_binary (-7L));;
eq ~printer:CCFun.id "0b0" (to_string_binary 0L)

let eq' = eq ~printer:CCInt.to_string;;

eq' 0 (popcount 0L);;
eq' 1 (popcount 1L);;
eq' 63 (popcount max_int);;
eq' 1 (popcount min_int);;
eq' 10 (popcount 0b1110010110110001010L);;
eq' 5 (popcount 0b1101110000000000L)
