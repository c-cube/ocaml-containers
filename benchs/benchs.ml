(** Generic benchs *)

module L = struct

  (* FLAT MAP *)

  let f_ x =
    if x mod 10 = 0 then []
    else if x mod 5 = 1 then [x;x+1]
    else [x;x+1;x+2;x+3]

  let bench_flat_map ?(time=2) n =
    let l = CCList.(1 -- n) in
    CCBench.throughputN time
      [ "flat_map", CCList.flat_map f_, l
      ; "flatten o CCList.map", (fun l -> List.flatten (CCList.map f_ l)), l
      ; "flatten o map", (fun l -> List.flatten (List.map f_ l)), l
      ]

  (* APPEND *)

  let append_ f (l1, l2, l3) =
    ignore (f (f l1 l2) l3)

  let bench_append ?(time=2) n =
    let l1 = CCList.(1 -- n) in
    let l2 = CCList.(n+1 -- 2*n) in
    let l3 = CCList.(2*n+1 -- 3*n) in
    let arg = l1, l2, l3 in
    CCBench.throughputN time
      [ "CCList.append", append_ CCList.append, arg
      ; "List.append", append_ List.append, arg
      ]

  (* FLATTEN *)

  let bench_flatten ?(time=2) n =
    let l = CCList.Idx.mapi (fun i x -> CCList.(x -- (x+ min i 100))) CCList.(1 -- n) in
    CCBench.throughputN time
      [ "CCList.flatten", CCList.flatten, l
      ; "List.flatten", List.flatten, l
      ; "fold_right append", (fun l -> List.fold_right List.append l []), l
      ; "CCList.(fold_right append)", (fun l->CCList.fold_right CCList.append l []), l
      ]

  (* MAIN *)

  let bench = CCBench.(
    "list" >:::
      [ "flat_map" >::
        map_int
          [ bench_flat_map ~time:2, 100
          ; bench_flat_map ~time:2, 10_000
          ; bench_flat_map ~time:4, 100_000]
      ; "flatten" >::
        map_int
          [ bench_flatten ~time:2, 100
          ; bench_flatten ~time:2, 10_000
          ; bench_flatten ~time:4, 100_000]
      ; "append" >::
        map_int
          [ bench_append ~time:2, 100
          ; bench_append ~time:2, 10_000
          ; bench_append ~time:4, 100_000]
      ]
    )

  let () = CCBench.Glob.register bench
end

let () =
  CCBench.Glob.run_main ()


