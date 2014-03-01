
let conv_json =
  let src = Conv.Source.(list_ (pair int_ int_)) in
  fun x -> Conv.into src Conv.Json.sink x

let manual_json =
  fun l ->
    `List (List.map (fun (a,b) -> `List [`Int a; `Int b]) l)

let bench_list x =
  let res = Benchmark.throughputN 5 
    [ "conv", conv_json, x
    ; "manual", manual_json, x
    ] in
  Benchmark.tabulate res

let () =
  Printf.printf "list of 5 elements...\n";
  bench_list [1,2; 3,4; 5,6; 7,8; 9,10]
