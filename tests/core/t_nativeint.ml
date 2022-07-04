open CCNativeint
module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () -> pow 2n 10n = 1024n;;
t @@ fun () -> pow 2n 15n = 32768n;;
t @@ fun () -> pow 10n 5n = 100000n;;
t @@ fun () -> pow 42n 0n = 1n;;
t @@ fun () -> pow 0n 1n = 0n;;
t @@ fun () -> floor_div 3n 5n = 0n;;
t @@ fun () -> floor_div 5n 5n = 1n;;
t @@ fun () -> floor_div 20n 5n = 4n;;
t @@ fun () -> floor_div 12n 5n = 2n;;
t @@ fun () -> floor_div 0n 5n = 0n;;
t @@ fun () -> floor_div (-1n) 5n = -1n;;
t @@ fun () -> floor_div (-5n) 5n = -1n;;
t @@ fun () -> floor_div (-12n) 5n = -3n;;
t @@ fun () -> floor_div 0n (-5n) = 0n;;
t @@ fun () -> floor_div 3n (-5n) = -1n;;
t @@ fun () -> floor_div 5n (-5n) = -1n;;
t @@ fun () -> floor_div 9n (-5n) = -2n;;
t @@ fun () -> floor_div 20n (-5n) = -4n;;
t @@ fun () -> floor_div (-2n) (-5n) = 0n;;
t @@ fun () -> floor_div (-8n) (-5n) = 1n;;
t @@ fun () -> floor_div (-35n) (-5n) = 7n;;

t @@ fun () ->
try
  ignore (floor_div 12n 0n);
  false
with Division_by_zero -> true
;;

t @@ fun () ->
try
  ignore (floor_div (-12n) 0n);
  false
with Division_by_zero -> true
;;

q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat))
  (fun (n, m) ->
    let m = m + 1n in
    floor_div n m = of_float @@ floor (to_float n /. to_float m))
;;

q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat))
  (fun (n, m) ->
    let m = m + 1n in
    floor_div n (-m) = of_float @@ floor (to_float n /. to_float (-m)))

let eq' = eq ~printer:Q.Print.(list to_string);;

eq' [ 0n; 1n; 2n; 3n; 4n; 5n ] (range 0n 5n |> Iter.to_list);;
eq' [ 0n ] (range 0n 0n |> Iter.to_list);;
eq' [ 5n; 4n; 3n; 2n ] (range 5n 2n |> Iter.to_list)

(* note: the last test checks that no error occurs due to overflows. *)
let eq' = eq ~printer:Q.Print.(list to_string);;

eq' [ 0n ] (range_by ~step:1n 0n 0n |> Iter.to_list);;
eq' [] (range_by ~step:1n 5n 0n |> Iter.to_list);;
eq' [] (range_by ~step:2n 1n 0n |> Iter.to_list);;
eq' [ 0n; 2n; 4n ] (range_by ~step:2n 0n 4n |> Iter.to_list);;
eq' [ 0n; 2n; 4n ] (range_by ~step:2n 0n 5n |> Iter.to_list);;
eq' [ 0n ] (range_by ~step:(neg 1n) 0n 0n |> Iter.to_list);;
eq' [] (range_by ~step:(neg 1n) 0n 5n |> Iter.to_list);;
eq' [] (range_by ~step:(neg 2n) 0n 1n |> Iter.to_list);;
eq' [ 5n; 3n; 1n ] (range_by ~step:(neg 2n) 5n 1n |> Iter.to_list);;
eq' [ 5n; 3n; 1n ] (range_by ~step:(neg 2n) 5n 0n |> Iter.to_list);;
eq' [ 0n ] (range_by ~step:max_int 0n 2n |> Iter.to_list);;

q
  Q.(pair (map of_int small_int) (map of_int small_int))
  (fun (i, j) ->
    let i = min i j and j = max i j in
    CCList.equal CCNativeint.equal
      (CCNativeint.range_by ~step:1n i j |> Iter.to_list)
      (CCNativeint.range i j |> Iter.to_list))
;;

eq ~printer:CCFun.id "0b111" (to_string_binary 7n);;
eq ~printer:CCFun.id "-0b111" (to_string_binary (-7n));;
eq ~printer:CCFun.id "0b0" (to_string_binary 0n)
