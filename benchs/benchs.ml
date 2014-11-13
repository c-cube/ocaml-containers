(** Generic benchs *)

let draw_line () =
  output_string stdout (CCString.repeat "*" 80);
  output_char stdout '\n'

module L = struct

  (* FLAT MAP *)

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

  (* APPEND *)

  let append_ f (l1, l2, l3) =
    ignore (f (f l1 l2) l3)

  let bench_append ?(time=2) n =
    draw_line ();
    Printf.printf "append for %d elements\n" n;
    let l1 = CCList.(1 -- n) in
    let l2 = CCList.(n+1 -- 2*n) in
    let l3 = CCList.(2*n+1 -- 3*n) in
    let arg = l1, l2, l3 in
    let res = Benchmark.throughputN time
      [ "CCList.append", append_ CCList.append, arg
      ; "List.append", append_ List.append, arg
      ] in
    Benchmark.tabulate res

  (* FLATTEN *)

  let bench_flatten ?(time=2) n =
    draw_line ();
    Printf.printf "flatten for %d elements\n" n;
    let l = CCList.Idx.mapi (fun i x -> CCList.(x -- (x+ min i 100))) CCList.(1 -- n) in
    let res = Benchmark.throughputN time
      [ "CCList.flatten", CCList.flatten, l
      ; "List.flatten", List.flatten, l
      ; "fold_right append", (fun l -> List.fold_right List.append l []), l
      ; "CCList.(fold_right append)", (fun l->CCList.fold_right CCList.append l []), l
      ] in
    Benchmark.tabulate res


  (* MAIN *)

  let run() =
    bench_flatten 100;
    bench_flatten 10_000;
    bench_flatten ~time:4 100_000;
    bench_flatmap 100;
    bench_flatmap 10_000;
    bench_flatmap ~time:4 100_000;
    bench_append 100;
    bench_append 10_000;
    bench_append ~time:4 100_000;
    ()
end

(* TODO *)

let tbl_ =
  [ "list", L.run
  ]

let bench_all () =
  List.iter (fun (name, run) ->
    draw_line ();
    Printf.printf "run tests for %s...\n" name;
    run()
  ) tbl_

let which_ = ref ("all", bench_all)
let set_which s =
  if s = "all" then which_ := s, bench_all
  else try
    let run = List.assoc s tbl_ in
    which_ := s, run
  with Not_found ->
    failwith ("unknown test " ^ s)
let options = []

let () =
  Arg.parse options set_which "benchs [which]";
  let name, run = !which_ in
  Printf.printf "run test %s\n" name;
  run ();
  ()


