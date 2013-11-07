
(** Benchmarking *)
(** {2 hashtables} *)

module IHashtbl = Hashtbl.Make(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module IFlatHashtbl = FlatHashtbl.Make(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module IFHashtbl = FHashtbl.Tree(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module IPersistentHashtbl = PersistentHashtbl.Make(struct
  type t = int
  let equal i j = i = j
  let hash i = i
end)

module IMap = Map.Make(struct
  type t = int
  let compare i j = i - j
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

let iflathashtbl_add n =
  let h = IFlatHashtbl.create 50 in
  for i = n downto 0 do
    IFlatHashtbl.replace h i i;
  done;
  h

let ifhashtbl_add n =
  let h = ref (IFHashtbl.empty 32) in
  for i = n downto 0 do
    h := IFHashtbl.replace !h i i;
  done;
  !h

let skiplist_add n =
  let l = SkipList.create compare in
  for i = n downto 0 do
    SkipList.add l i i;
  done;
  l

let ipersistenthashtbl_add n =
  let h = ref (IPersistentHashtbl.create 32) in
  for i = n downto 0 do
    h := IPersistentHashtbl.replace !h i i;
  done;
  !h

let imap_add n =
  let h = ref IMap.empty in
  for i = n downto 0 do
    h := IMap.add i i !h;
  done;
  !h

let _ =
  Format.printf "----------------------------------------@.";
  let res = Bench.bench_n
    ["phashtbl_add", (fun n -> ignore (phashtbl_add n));
     "hashtbl_add", (fun n -> ignore (hashtbl_add n));
     "ihashtbl_add", (fun n -> ignore (ihashtbl_add n));
     "iflathashtbl_add", (fun n -> ignore (iflathashtbl_add n));
     "ifhashtbl_add", (fun n -> ignore (ifhashtbl_add n));
     "ipersistenthashtbl_add", (fun n -> ignore (ipersistenthashtbl_add n));
     "skiplist_add", (fun n -> ignore (skiplist_add n));
     "imap_add", (fun n -> ignore (imap_add n));
    ]
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

let iflathashtbl_replace n =
  let h = IFlatHashtbl.create 50 in
  for i = 0 to n do
    IFlatHashtbl.replace h i i;
  done;
  for i = n downto 0 do
    IFlatHashtbl.replace h i i;
  done;
  h

let ifhashtbl_replace n =
  let h = ref (IFHashtbl.empty 32) in
  for i = 0 to n do
    h := IFHashtbl.replace !h i i;
  done;
  for i = n downto 0 do
    h := IFHashtbl.replace !h i i;
  done;
  !h

let ipersistenthashtbl_replace n =
  let h = ref (IPersistentHashtbl.create 32) in
  for i = 0 to n do
    h := IPersistentHashtbl.replace !h i i;
  done;
  for i = n downto 0 do
    h := IPersistentHashtbl.replace !h i i;
  done;
  !h

let skiplist_replace n =
  let l = SkipList.create compare in
  for i = 0 to n do
    SkipList.add l i i;
  done;
  for i = n downto 0 do
    SkipList.add l i i;
  done;
  l

let imap_replace n =
  let h = ref IMap.empty in
  for i = 0 to n do
    h := IMap.add i i !h;
  done;
  for i = n downto 0 do
    h := IMap.add i i !h;
  done;
  !h

let _ =
  Format.printf "----------------------------------------@.";
  let res = Bench.bench_n
    ["phashtbl_replace", (fun n -> ignore (phashtbl_replace n));
     "hashtbl_replace", (fun n -> ignore (hashtbl_replace n));
     "ihashtbl_replace", (fun n -> ignore (ihashtbl_replace n));
     "iflathashtbl_replace", (fun n -> ignore (iflathashtbl_replace n));
     "ifhashtbl_replace", (fun n -> ignore (ifhashtbl_replace n));
     "ipersistenthashtbl_replace", (fun n -> ignore (ipersistenthashtbl_replace n));
     "skiplist_replace", (fun n -> ignore (skiplist_replace n));
     "imap_replace", (fun n -> ignore (imap_replace n));
    ]
  in
  Bench.summarize 1. res

let my_len = 250

let phashtbl_find h =
  fun n ->
    for i = 0 to n-1 do
      ignore (PHashtbl.find h i);
    done
     
let hashtbl_find h =
  fun n ->
    for i = 0 to n-1 do
      ignore (Hashtbl.find h i);
    done
     
let ihashtbl_find h =
  fun n ->
    for i = 0 to n-1 do
      ignore (IHashtbl.find h i);
    done
     
let iflathashtbl_find h =
  fun n ->
    for i = 0 to n-1 do
      ignore (IFlatHashtbl.find h i);
    done
     
let ifhashtbl_find h =
  fun n ->
    for i = 0 to n-1 do
      ignore (IFHashtbl.find h i);
    done
     
let ipersistenthashtbl_find h =
  fun n ->
    for i = 0 to n-1 do
      ignore (IPersistentHashtbl.find h i);
    done

let skiplist_find l =
  fun n ->
    for i = 0 to n-1 do
      ignore (SkipList.find l i);
    done

let array_find a =
  fun n ->
    for i = 0 to n-1 do
      ignore (Array.get a i);
    done

let imap_find m =
  fun n ->
    for i = 0 to n-1 do
      ignore (IMap.find i m);
    done

let _ =
  List.iter
    (fun len ->
      let h = phashtbl_add len in
      let h' = hashtbl_add len in
      let h'' = ihashtbl_add len in
      let h''' = iflathashtbl_add len in
      let h'''' = ifhashtbl_add len in
      let h''''' = ipersistenthashtbl_add len in
      let l = skiplist_add len in
      let a = Array.init len (fun i -> string_of_int i) in
      let m = imap_add len in
      Format.printf "----------------------------------------@.";
      Format.printf "try on size %d@.@.@." len;
      Bench.bench [
        "phashtbl_find", (fun () -> phashtbl_find h len);
        "hashtbl_find", (fun () -> hashtbl_find h' len);
        "ihashtbl_find", (fun () -> ihashtbl_find h'' len);
        "iflathashtbl_find", (fun () -> iflathashtbl_find h''' len);
        "ifhashtbl_find", (fun () -> ifhashtbl_find h'''' len);
        "ipersistenthashtbl_find", (fun () -> ipersistenthashtbl_find h''''' len);
        "skiplist_find", (fun () -> skiplist_find l len);
        "array_find", (fun () -> array_find a len);
        "imap_find", (fun () -> imap_find m len);
      ])
    [10;20;100;1000;10000]

(** {2 Sequence/Gen} *)

let _ =
  let n = 1_000_000 in
  let seq () = Sequence.fold (+) 0 (Sequence.int_range ~start:0 ~stop:n) in
  let enum () = Gen.fold (+) 0 (Gen.int_range 0 n) in
  Bench.bench
    [ "sequence.fold", seq;
      "gen.fold", enum;
    ]

let _ =
  let n = 100_000 in
  let seq () =
    let open Sequence in
    let seq = int_range ~start:0 ~stop:n in
    let seq = flatMap (fun x -> int_range ~start:x ~stop:(x+10)) seq in
    fold (+) 0 seq in
  let enum () =
    let open Gen in
    let seq = int_range 0 n in
    let seq = flatMap (fun x -> int_range x (x+10)) seq in
    fold (+) 0 seq in
  Bench.bench
    [ "sequence.flatMap", seq;
      "gen.flatMap", enum;
    ]

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
    let cache = C.create size in
    let cached_fib x = C.with_cache_rec cache fib x in
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
  let conf = Bench.config in
  conf.Bench.samples <- 100;
  Bench.bench
    [ "linear_fib", bench_fib (LinearFibo.fib ~size:5);
      "replacing_fib", bench_fib (ReplacingFibo.fib ~size:256);
      "LRU_fib", bench_fib (LRUFibo.fib ~size:256);
      "dummy_fib", bench_fib (DummyFibo.fib ~size:5);
    ];
  conf.Bench.samples <- 1000;
  ()

