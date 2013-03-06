
(** Benchmarking *)

(** {2 Cache} *)

(** Cached fibonacci function *)
module Fibo(C : Cache.S with type key = int) = struct
  let fib ~size =
    let rec fib fib' n =
      match n with
      | 0 -> 0
      | 1 -> 1
      | 2 -> 1
      | n ->
        fib' (n-1) + fib' (n-2)
    in
    let _cache, cached_fib = C.with_cache_rec size fib in
    cached_fib
end

module LinearIntCache = Cache.Linear(struct
  type t = int
  let equal i j = i = j
end)

module ReplacingIntCache = Cache.Replacing(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module LRUIntCache = Cache.LRU(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module DummyIntCache = Cache.Dummy(struct type t = int end)

let _ =
  (* Fibonacci for those caching implementations *)
  let module LinearFibo = Fibo(LinearIntCache) in
  let module ReplacingFibo = Fibo(ReplacingIntCache) in
  let module LRUFibo= Fibo(LRUIntCache) in
  let module DummyFibo = Fibo(DummyIntCache) in
  (* benchmark caches with fibo function *)
  let bench_fib fib () = 
    ignore (List.map fib [5;10;20;30;35]);
    ()
  in
  Bench.bench
    [ "linear_fib", bench_fib (LinearFibo.fib ~size:5);
      "replacing_fib", bench_fib (ReplacingFibo.fib ~size:256);
      "LRU_fib", bench_fib (LRUFibo.fib ~size:256);
      "dummy_fib", bench_fib (DummyFibo.fib ~size:5);
    ]

(** {2 PHashtbl} *)

module IHashtbl = Hashtbl.Make(struct
  type t = int
  let equal i j = i - j = 0
  let hash i = i
end)

let phashtbl_add n =
  let h = PHashtbl.create 50 in
  for i = n downto 0 do
    PHashtbl.add h i i;
  done;
  h

let hashtbl_add n =
  let h = Hashtbl.create 50 in
  for i = n downto 0 do
    Hashtbl.add h i i;
  done;
  h

let ihashtbl_add n =
  let h = IHashtbl.create 50 in
  for i = n downto 0 do
    IHashtbl.add h i i;
  done;
  h

let _ =
  Format.printf "----------------------------------------@.";
  let res = Bench.bench_n
    ["phashtbl_add", (fun n -> ignore (phashtbl_add n));
     "hashtbl_add", (fun n -> ignore (hashtbl_add n));
     "ihashtbl_add", (fun n -> ignore (ihashtbl_add n));]
  in
  Bench.summarize 1. res

let phashtbl_replace n =
  let h = PHashtbl.create 50 in
  for i = 0 to n do
    PHashtbl.replace h i i;
  done;
  for i = n downto 0 do
    PHashtbl.replace h i i;
  done;
  h

let hashtbl_replace n =
  let h = Hashtbl.create 50 in
  for i = 0 to n do
    Hashtbl.replace h i i;
  done;
  for i = n downto 0 do
    Hashtbl.replace h i i;
  done;
  h

let ihashtbl_replace n =
  let h = IHashtbl.create 50 in
  for i = 0 to n do
    IHashtbl.replace h i i;
  done;
  for i = n downto 0 do
    IHashtbl.replace h i i;
  done;
  h

let _ =
  Format.printf "----------------------------------------@.";
  let res = Bench.bench_n
    ["phashtbl_replace", (fun n -> ignore (phashtbl_replace n));
     "hashtbl_replace", (fun n -> ignore (hashtbl_replace n));
     "ihashtbl_replace", (fun n -> ignore (ihashtbl_replace n));]
  in
  Bench.summarize 1. res

let my_len = 250
let round_n n = abs ((abs n) mod my_len)

let phashtbl_find h =
  fun n ->
    for i = 0 to n do
      ignore (PHashtbl.find h (round_n i));
    done
     
let hashtbl_find h =
  fun n ->
    for i = 0 to n do
      ignore (Hashtbl.find h (round_n i));
    done
     
let ihashtbl_find h =
  fun n ->
    for i = 0 to n do
      ignore (IHashtbl.find h (round_n i));
    done

let _ =
  let h = phashtbl_add my_len in
  let h' = hashtbl_add my_len in
  let h'' = ihashtbl_add my_len in
  List.iter (fun n ->
    Format.printf "----------------------------------------@.";
    Format.printf "try on size %d@.@.@." n;
    Bench.bench [
      "phashtbl_find", (fun () -> phashtbl_find h n);
      "hashtbl_find", (fun () -> hashtbl_find h' n);
      "ihashtbl_find", (fun () -> ihashtbl_find h'' n);
    ])
    [10;20;100;1000;10000;100000]
