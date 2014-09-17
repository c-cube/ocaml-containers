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

(* conversion back from json *)
let rec point_of_json_manual (j:Conv.Json.t) =
  let module P = Point in
  match j with
  | `Assoc l ->
      let x = List.assoc "x" l in
      let y = List.assoc "y" l in
      let color = List.assoc "color" l in
      let prev = List.assoc "prev" l in
      let prev = match prev with
        | `String "none" -> None
        | `List [`String "some"; p'] -> Some (point_of_json_manual p')
        | _ -> failwith "expected point"
      in
      begin match x, y, color with
      | `Int x, `Int y, `String color -> P.({x;y;color;prev;})
      | _ -> failwith "expected point"
      end
  | _ -> failwith "expected point"

let points_of_json_manual = function
  | `List l -> List.map point_of_json_manual l
  | _ -> failwith "expected list of points"

let points_of_json_conv =
  Conv.from Conv.Json.source (Conv.Sink.list_ Point.sink)

let bench_point_list_back l =
  let res = Benchmark.throughputN 5 
    [ "conv", points_of_json_conv, l
    ; "manual", points_of_json_manual, l
    ] in
  Benchmark.tabulate res

let () =
  Printf.printf "list of 5 elements...\n";
  bench_list [1,2; 3,4; 5,6; 7,8; 9,10];

  let l = CCGen.(1 -- 100 |> map (fun x->x,x) |> to_rev_list) in
  Printf.printf "list of %d elements...\n" (List.length l);
  bench_list l;

  let l = CCGen.(repeat Point.p |> take 10 |> to_rev_list) in
  Printf.printf "list of %d points...\n" (List.length l);
  bench_point_list l;

  (* convert back from json *)
  let l' = conv_list_point_to_json l in
  Printf.printf "from JSON...\n";
  bench_point_list_back l';
  ()
