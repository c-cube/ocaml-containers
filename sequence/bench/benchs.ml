
module S = Sequence
open Sequence.Infix

let small = [10;20;50;100;500]
let medium = small @ [1000;10_000;100_000]
let big = medium @ [500_000; 1_000_000; 2_000_000]

let bench_fold n =
  0 -- n |> S.fold (+) 0 |> ignore

let bench_flatmap n =
  0 -- n |> S.flatMap (fun i -> i -- (i+5)) |> (fun _ -> ())

let bench_product n =
  S.product (0 -- n) (0 -- n) (fun (i,j) -> ())

let _ =
  List.iter
    (fun (name,bench,sizes) ->
      Format.printf "-------------------------------------------------------@.";
      Format.printf "bench %s@." name;
      List.iter
        (fun n ->
          let name = name ^ " on " ^ string_of_int n in
          let res = Benchmark.throughput1 2 ~name bench n in
          Benchmark.tabulate res;
        ) sizes
    )
    [ "fold", bench_fold, big
    ; "flatmap", bench_flatmap, medium
    ; "product", bench_product, small
    ];
  ()
