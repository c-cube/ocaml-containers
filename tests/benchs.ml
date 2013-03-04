
(** Benchmarking *)

let phashtbl_add n =
  let h = PHashtbl.create 50 in
  for i = 0 to n do
    PHashtbl.add h i i;
  done;
  h

let hashtbl_add n =
  let h = Hashtbl.create 50 in
  for i = 0 to n do
    Hashtbl.add h i i;
  done;
  h

let _ =
  let n = 50000 in
  let res = Bench.bench_funs
    ["phashtbl_add", (fun n -> ignore (phashtbl_add n));
     "hashtbl_add", (fun n -> ignore (hashtbl_add n));]
    n
  in
  Bench.summarize 1. res

let phashtbl_replace n =
  let h = PHashtbl.create 50 in
  for i = 0 to n do
    PHashtbl.replace h i i;
  done;
  for i = 0 to n do
    PHashtbl.replace h i i;
  done;
  h

let hashtbl_replace n =
  let h = Hashtbl.create 50 in
  for i = 0 to n do
    Hashtbl.replace h i i;
  done;
  for i = 0 to n do
    Hashtbl.replace h i i;
  done;
  h

let _ =
  let n = 50000 in
  let res = Bench.bench_funs
    ["phashtbl_replace", (fun n -> ignore (phashtbl_replace n));
     "hashtbl_replace", (fun n -> ignore (hashtbl_replace n));]
    n
  in
  Bench.summarize 1. res

let phashtbl_mem h =
  fun n ->
    for i = 0 to n do
      ignore (PHashtbl.find h i);
    done
     
let hashtbl_mem h =
  fun n ->
    for i = 0 to n do
      ignore (Hashtbl.find h i);
    done

let _ =
  let n = 50000 in
  let h = phashtbl_add n in
  let h' = hashtbl_add n in
  let res = Bench.bench_funs
    ["phashtbl_mem", phashtbl_mem h;
     "hashtbl_mem", hashtbl_mem h';]
    n
  in
  Bench.summarize 1. res
