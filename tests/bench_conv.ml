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

(** benchmark points *)
module Point = Conv.Point

let rec point_to_json_manual p =
  let module P = Point in
  `Assoc
    [ "x", `Int p.P.x
    ; "y", `Int p.P.y
    ; "color", `String p.P.color
    ; "prev", (match p.P.prev with
      | None -> `String "none"
      | Some p' -> point_to_json_manual p')
    ]

let list_point_to_json_manual l =
  `List (List.map point_to_json_manual l)

let conv_list_point_to_json l =
  Conv.into (Conv.Source.list_ Point.source) Conv.Json.sink l

let bench_point_list x =
  let res = Benchmark.throughputN 5 
    [ "conv", conv_list_point_to_json, x
    ; "manual", list_point_to_json_manual, x
    ] in
  Benchmark.tabulate res


let () =
  Printf.printf "list of 5 elements...\n";
  bench_list [1,2; 3,4; 5,6; 7,8; 9,10];

  let l = Gen.(1 -- 100 |> map (fun x->x,x) |> to_rev_list) in
  Printf.printf "list of %d elements...\n" (List.length l);
  bench_list l;

  let l = Gen.(repeat Point.p |> take 10 |> to_rev_list) in
  Printf.printf "list of %d points...\n" (List.length l);
  bench_point_list l;
  ()
