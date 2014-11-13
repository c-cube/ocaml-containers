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

  let () = CCBench.register CCBench.(
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
end

module Vec = struct
  let f x = x+1

  let map_push_ f v =
    let v' = CCVector.create () in
    CCVector.iter (fun x -> CCVector.push v' (f x)) v;
    v'

  let map_push_size_ f v =
    let v' = CCVector.create_with ~capacity:(CCVector.length v) 0 in
    CCVector.iter (fun x -> CCVector.push v' (f x)) v;
    v'

  let bench_map n =
    let v = CCVector.init n (fun x->x) in
    CCBench.throughputN 2
      [ "map", CCVector.map f, v
      ; "map_push", map_push_ f, v
      ; "map_push_cap", map_push_size_ f, v
      ]

  let try_append_ app n v2 () =
    let v1 = CCVector.init n (fun x->x) in
    app v1 v2;
    assert (CCVector.length v1 = 2*n);
    ()

  let append_naive_ v1 v2 =
    CCVector.iter (fun x -> CCVector.push v1 x) v2

  let bench_append n =
    let v2 = CCVector.init n (fun x->n+x) in
    CCBench.throughputN 2
      [ "append", try_append_ CCVector.append n v2, ()
      ; "append_naive", try_append_ append_naive_ n v2, ()
      ]

  let () = CCBench.register CCBench.(
    "vector" >:::
      [ "map" >:: with_int bench_map [100; 10_000; 100_000]
      ; "append" >:: with_int bench_append [100; 10_000; 50_000]
      ]
  )
end

let () =
  CCBench.run_main ()


