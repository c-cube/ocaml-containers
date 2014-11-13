(** Generic benchs *)

let draw_line () =
  output_string stdout (CCString.repeat "*" 80);
  output_char stdout '\n'

module L = struct

  let f_ x =
    if x mod 10 = 0 then []
    else if x mod 5 = 1 then [x;x+1]
    else [x;x+1;x+2;x+3]

  let bench_flatmap ?(time=2) n =
    draw_line ();
    Printf.printf "flat_map for %d elements\n" n;
    let l = CCList.(1 -- n) in
    let res = Benchmark.throughputN time
      [ "flat_map", CCList.flat_map f_, l
      ; "flatten o CCList.map", (fun l -> List.flatten (CCList.map f_ l)), l
      ; "flatten o map", (fun l -> List.flatten (List.map f_ l)), l
      ] in
    Benchmark.tabulate res

  let run() =
    bench_flatmap 100;
    bench_flatmap 10_000;
    bench_flatmap ~time:4 100_000;
    ()
end

(* TODO *)

let () =
  L.run ();
  ()


