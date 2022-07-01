
open CCArray

module T = (val Containers_testlib.make ~__FILE__ ())
include T;;

t @@ fun () ->
  let st = Random.State.make [||] in let a = 0--10000 in
  let b = Array.copy a in shuffle_with st a; a <> b;;

eq  (Some 1) (get_safe [|1;2;3|] 0);;
eq  (Some 2) (get_safe [|1;2;3|] 1);;
eq  (Some 3) (get_safe [|1;2;3|] 2);;
eq  None (get_safe [|1;2;3|] 4);;
eq  None (get_safe [|1;2;3|] max_int);;
eq  None (get_safe [|1;2;3|] ~-1);;
eq  None (get_safe [|1;2;3|] ~-42);;

q Q.(array int) (fun a ->
    let b = map ((+) 1) a in
    map_inplace ((+) 1) a;
    b = a);;

t @@ fun () -> fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 (Array.of_list [true;true;false;true]) = 2;;

eq (6, [|"1"; "2"; "3"|])
    (fold_map (fun acc x->acc+x, string_of_int x) 0 [|1;2;3|]);;

q Q.(array int) (fun a ->
  fold_map (fun acc x -> x::acc, x) [] a = (List.rev @@ Array.to_list a, a));;

eq ~printer:Q.Print.(array int) [|0;1;3;6|] (scan_left (+) 0 [|1;2;3|]);;
eq ~printer:Q.Print.(array int) [|0|] (scan_left (+) 0 [||]);;

t @@ fun () -> reverse_in_place [| |]; true;;
t @@ fun () -> reverse_in_place [| 1 |]; true;;
t @@ fun () -> let a = [| 1; 2; 3; 4; 5 |] in
  reverse_in_place a;
  a = [| 5;4;3;2;1 |];;
t @@ fun () -> let a = [| 1; 2; 3; 4; 5; 6 |] in
  reverse_in_place a;
  a = [| 6;5;4;3;2;1 |];;

eq ~cmp:(=) ~printer:Q.Print.(array int)  [||] (sorted Stdlib.compare [||]);;
eq ~cmp:(=) ~printer:Q.Print.(array int) [|0;1;2;3;4|] (sorted Stdlib.compare [|3;2;1;4;0|]);;

q Q.(array int) (fun a ->
    let b = Array.copy a in
    Array.sort Stdlib.compare b; b = sorted Stdlib.compare a);;

eq ~cmp:(=) ~printer:Q.Print.(array int) [||] (sort_indices Stdlib.compare [||]);;
eq ~cmp:(=) ~printer:Q.Print.(array int) [|4;2;1;0;3|] (sort_indices Stdlib.compare [|"d";"c";"b";"e";"a"|]);;

q Q.(array_of_size Gen.(0 -- 30) printable_string) (fun a ->
    let b = sort_indices String.compare a in
    sorted String.compare a = Array.map (Array.get a) b);;

eq ~cmp:(=) ~printer:Q.Print.(array int) [||] (sort_ranking Stdlib.compare [||]);;
eq ~cmp:(=) ~printer:Q.Print.(array int) [|3;2;1;4;0|] (sort_ranking Stdlib.compare [|"d";"c";"b";"e";"a"|]);;

q Q.(array_of_size Gen.(0--50) printable_string) (fun a ->
    let b = sort_ranking String.compare a in
    let a_sorted = sorted String.compare a in
    a = Array.map (Array.get a_sorted) b);;

q Q.(array small_int) (fun a -> rev (rev a) = a);;

t @@ fun () -> rev [| 1; 2; 3 |] = [| 3; 2; 1 |];;
t @@ fun () -> rev [| 1; 2; |] = [| 2; 1 |];;
t @@ fun () -> rev [| |] = [| |];;

q Q.(array small_int) (fun a ->
  mem 1 a = (Array.mem 1 a));;

t @@ fun () -> filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None)
    [| 1; 2; 3; 4 |] = [| "2"; "4" |];;
t @@ fun () -> filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None)
    [| 1; 2; 3; 4; 5; 6 |]
    = [| "2"; "4"; "6" |];;
t @@ fun () -> let a = [| 1; 3; 5 |] in
  let a' = flat_map (fun x -> [| x; x+1 |]) a in
  a' = [| 1; 2; 3; 4; 5; 6 |];;

eq ~cmp:(=) ~printer:Q.Print.(array int)  [| 11; 12; 21; 22 |] (sorted CCInt.compare @@ monoid_product (+) [| 10; 20 |] [| 1; 2 |]);;
eq ~cmp:(=) ~printer:Q.Print.(array int)  [| 11; 12; 13; 14 |] (sorted CCInt.compare @@ monoid_product (+) [| 10 |] [| 1; 2; 3; 4 |]);;

t @@ fun () -> lookup ~cmp:CCInt.compare 2 [|0;1;2;3;4;5|] = Some 2;;
t @@ fun () -> lookup ~cmp:CCInt.compare 4 [|0;1;2;3;4;5|] = Some 4;;
t @@ fun () -> lookup ~cmp:CCInt.compare 0 [|1;2;3;4;5|] = None;;
t @@ fun () -> lookup ~cmp:CCInt.compare 6 [|1;2;3;4;5|] = None;;
t @@ fun () -> lookup ~cmp:CCInt.compare 3 [| |] = None;;
t @@ fun () -> lookup ~cmp:CCInt.compare 1 [| 1 |] = Some 0;;
t @@ fun () -> lookup ~cmp:CCInt.compare 2 [| 1 |] = None;;

t @@ fun () -> bsearch ~cmp:CCInt.compare 3 [|1; 2; 2; 3; 4; 10|] = `At 3;;
t @@ fun () -> bsearch ~cmp:CCInt.compare 5 [|1; 2; 2; 3; 4; 10|] = `Just_after 4;;
t @@ fun () -> bsearch ~cmp:CCInt.compare 1 [|1; 2; 5; 5; 11; 12|] = `At 0;;
t @@ fun () -> bsearch ~cmp:CCInt.compare 12 [|1; 2; 5; 5; 11; 12|] = `At 5;;
t @@ fun () -> bsearch ~cmp:CCInt.compare 10 [|1; 2; 2; 3; 4; 9|] = `All_lower;;
t @@ fun () -> bsearch ~cmp:CCInt.compare 0 [|1; 2; 2; 3; 4; 9|] = `All_bigger;;
t @@ fun () -> bsearch ~cmp:CCInt.compare 3 [| |] = `Empty;;

t @@ fun () -> (1 -- 4) |> Array.to_list = [1;2;3;4];;
t @@ fun () -> (4 -- 1) |> Array.to_list = [4;3;2;1];;
t @@ fun () -> (0 -- 0) |> Array.to_list = [0];;

q Q.(pair small_int small_int) (fun (a,b) ->
    (a -- b) |> Array.to_list = CCList.(a -- b));;
q Q.(pair small_int small_int) (fun (a,b) ->
    (a --^ b) |> Array.to_list = CCList.(a --^ b));;
q Q.(pair (array small_int)(array small_int)) (fun (a,b) ->
    equal (=) a b = equal (=) b a);;

t @@ fun () -> equal (=) [|1|] [|1|];;
t @@ fun () -> compare CCOrd.poly [| 1; 2; 3 |] [| 1; 2; 3 |] = 0;;
t @@ fun () -> compare CCOrd.poly [| 1; 2; 3 |] [| 2; 2; 3 |] < 0;;
t @@ fun () -> compare CCOrd.poly [| 1; 2; |] [| 1; 2; 3 |] < 0;;
t @@ fun () -> compare CCOrd.poly [| 1; 2; 3 |] [| 1; 2; |] > 0;;

t @@ fun () -> let a = [| 1;2;3 |] in
    swap a 0 1;
    a = [| 2;1;3 |];;
t @@ fun () -> let a = [| 1;2;3 |] in
    swap a 0 2;
    a = [| 3;2;1 |];;

q Q.(array_of_size Gen.(0 -- 100) small_int) (fun a ->
    let b = Array.copy a in
    for i = 0 to Array.length a-1 do
      for j = i+1 to Array.length a-1 do
        swap a i j; done; done;
    for i = 0 to Array.length a-1 do
      for j = i+1 to Array.length a-1 do
        swap a i j; done; done;
    a=b);;

eq ~printer:(fun s -> s) (to_string string_of_int [|1;2;3;4;5;6|]) "1, 2, 3, 4, 5, 6";;
eq ~printer:(fun s -> s) (to_string string_of_int [||]) "";;
eq ~printer:(fun s -> s) (to_string ~sep:" " string_of_int [|1;2;3;4;5;6|]) "1 2 3 4 5 6";;
eq ~printer:(fun s -> s) (to_string string_of_int [|1|]) "1";;

eq  [] (to_seq [||] |> CCList.of_seq);;
eq  [1;2;3] (to_seq [|1;2;3|] |> CCList.of_seq);;
eq  CCList.(1 -- 1000) (to_seq (1--1000) |> CCList.of_seq);;

module IA = struct
  let get = Array.get
  let set = Array.set
  let length = Array.length
  type elt = int
  type t = int array
end

let gen_arr = Q.Gen.(array_size (1--100) small_int)
let arr_arbitrary = Q.make
  ~print:Q.Print.(array int)
  ~small:Array.length
  ~shrink:Q.Shrink.(array ?shrink:None)
  gen_arr;;

q ~count:300
  arr_arbitrary (fun a ->
    let a1 = Array.copy a and a2 = Array.copy a in
    Array.sort CCInt.compare a1; sort_generic (module IA) ~cmp:CCInt.compare a2;
    a1 = a2 );;

