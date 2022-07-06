open CCInt32
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> pow 2l 10l = 1024l;;
t @@ fun () -> pow 2l 15l = 32768l;;
t @@ fun () -> pow 10l 5l = 100000l;;
t @@ fun () -> pow 42l 0l = 1l;;
t @@ fun () -> pow 0l 1l = 0l;;
t @@ fun () -> floor_div 3l 5l = 0l;;
t @@ fun () -> floor_div 5l 5l = 1l;;
t @@ fun () -> floor_div 20l 5l = 4l;;
t @@ fun () -> floor_div 12l 5l = 2l;;
t @@ fun () -> floor_div 0l 5l = 0l;;
t @@ fun () -> floor_div (-1l) 5l = -1l;;
t @@ fun () -> floor_div (-5l) 5l = -1l;;
t @@ fun () -> floor_div (-12l) 5l = -3l;;
t @@ fun () -> floor_div 0l (-5l) = 0l;;
t @@ fun () -> floor_div 3l (-5l) = -1l;;
t @@ fun () -> floor_div 5l (-5l) = -1l;;
t @@ fun () -> floor_div 9l (-5l) = -2l;;
t @@ fun () -> floor_div 20l (-5l) = -4l;;
t @@ fun () -> floor_div (-2l) (-5l) = 0l;;
t @@ fun () -> floor_div (-8l) (-5l) = 1l;;
t @@ fun () -> floor_div (-35l) (-5l) = 7l;;

t @@ fun () ->
try
  ignore (floor_div 12l 0l);
  false
with Division_by_zero -> true
;;

t @@ fun () ->
try
  ignore (floor_div (-12l) 0l);
  false
with Division_by_zero -> true
;;

q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat))
  (fun (n, m) ->
    let m = m + 1l in
    floor_div n m = of_float @@ floor (to_float n /. to_float m))
;;

q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat))
  (fun (n, m) ->
    let m = m + 1l in
    floor_div n (-m) = of_float @@ floor (to_float n /. to_float (-m)))

let eq' = eq ~printer:Q.Print.(list to_string);;

eq' [ 0l; 1l; 2l; 3l; 4l; 5l ] (range 0l 5l |> Iter.to_list);;
eq' [ 0l ] (range 0l 0l |> Iter.to_list);;
eq' [ 5l; 4l; 3l; 2l ] (range 5l 2l |> Iter.to_list)

(* note: the last test checks that no error occurs due to overflows. *)
let eq' = eq ~printer:Q.Print.(list to_string);;

eq' [ 0l ] (range_by ~step:1l 0l 0l |> Iter.to_list);;
eq' [] (range_by ~step:1l 5l 0l |> Iter.to_list);;
eq' [] (range_by ~step:2l 1l 0l |> Iter.to_list);;
eq' [ 0l; 2l; 4l ] (range_by ~step:2l 0l 4l |> Iter.to_list);;
eq' [ 0l; 2l; 4l ] (range_by ~step:2l 0l 5l |> Iter.to_list);;
eq' [ 0l ] (range_by ~step:(neg 1l) 0l 0l |> Iter.to_list);;
eq' [] (range_by ~step:(neg 1l) 0l 5l |> Iter.to_list);;
eq' [] (range_by ~step:(neg 2l) 0l 1l |> Iter.to_list);;
eq' [ 5l; 3l; 1l ] (range_by ~step:(neg 2l) 5l 1l |> Iter.to_list);;
eq' [ 5l; 3l; 1l ] (range_by ~step:(neg 2l) 5l 0l |> Iter.to_list);;
eq' [ 0l ] (range_by ~step:max_int 0l 2l |> Iter.to_list);;

q
  Q.(pair (map of_int small_int) (map of_int small_int))
  (fun (i, j) ->
    let i = min i j and j = max i j in
    CCList.equal CCInt32.equal
      (CCInt32.range_by ~step:1l i j |> Iter.to_list)
      (CCInt32.range i j |> Iter.to_list))
;;

eq ~printer:CCFun.id "0b111" (to_string_binary 7l);;
eq ~printer:CCFun.id "-0b111" (to_string_binary (-7l));;
eq ~printer:CCFun.id "0b0" (to_string_binary 0l);;

q
  Q.(0 -- (Int32.max_int |> Int32.to_int))
  (fun i ->
    let n1 = CCInt.popcount i in
    let n2 = CCInt32.popcount (Int32.of_int i) in
    CCInt.(n1 = n2))

let eq' = eq ~printer:CCInt.to_string;;

eq' 0 (popcount 0l);;
eq' 1 (popcount 1l);;
eq' 31 (popcount max_int);;
eq' 1 (popcount min_int);;
eq' 10 (popcount 0b1110010110110001010l);;
eq' 5 (popcount 0b1101110000000000l)
