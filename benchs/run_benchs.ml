(** Generic benchs *)

let (@>) = Benchmark.Tree.(@>)
let (@>>) = Benchmark.Tree.(@>>)
let (@>>>) = Benchmark.Tree.(@>>>)

module L = struct
  (* FLAT MAP *)

  let f_ x =
    if x mod 10 = 0 then []
    else if x mod 5 = 1 then [x;x+1]
    else [x;x+1;x+2;x+3]

  let bench_flat_map ?(time=2) n = "" @> lazy(
    let l = CCList.(1 -- n) in
    let flatten_map_ l = List.flatten (CCList.map f_ l)
    and flatten_ccmap_ l = List.flatten (List.map f_ l) in
    Benchmark.throughputN time
      [ "flat_map", CCList.flat_map f_, l
      ; "flatten o CCList.map", flatten_ccmap_, l
      ; "flatten o map", flatten_map_, l
      ]
  )

  (* APPEND *)

  let append_ f (l1, l2, l3) =
    ignore (f (f l1 l2) l3)

  let bench_append ?(time=2) n = "" @> lazy (
    let l1 = CCList.(1 -- n) in
    let l2 = CCList.(n+1 -- 2*n) in
    let l3 = CCList.(2*n+1 -- 3*n) in
    let arg = l1, l2, l3 in
    Benchmark.throughputN time
      [ "CCList.append", append_ CCList.append, arg
      ; "List.append", append_ List.append, arg
      ]
    )

  (* FLATTEN *)

  let bench_flatten ?(time=2) n = "" @> lazy (
    let fold_right_append_ l =
      List.fold_right List.append l []
    and cc_fold_right_append_ l =
      CCList.fold_right CCList.append l []
    in
    let l =
      CCList.Idx.mapi
        (fun i x -> CCList.(x -- (x+ min i 100)))
        CCList.(1 -- n)
    in
    Benchmark.throughputN time
      [ "CCList.flatten", CCList.flatten, l
      ; "List.flatten", List.flatten, l
      ; "fold_right append", fold_right_append_, l
      ; "CCList.(fold_right append)", cc_fold_right_append_, l
      ]
  )

  (* MAIN *)

  let () = Benchmark.Tree.(
    "list" @>>>
      [ "flat_map" @>>
          with_int (bench_flat_map ~time:2) [100; 10_000; 100_00]
      ; "flatten" @>>
        with_int (bench_flatten ~time:2) [100; 10_000; 100_000]
      ; "append" @>>
        with_int (bench_append ~time:2) [100; 10_000; 100_000]
      ]
    |> register)
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

  let bench_map n = "map" @> lazy (
    let v = CCVector.init n (fun x->x) in
    Benchmark.throughputN 2
      [ "map", CCVector.map f, v
      ; "map_push", map_push_ f, v
      ; "map_push_cap", map_push_size_ f, v
      ]
  )

  let try_append_ app n v2 () =
    let v1 = CCVector.init n (fun x->x) in
    app v1 v2;
    assert (CCVector.length v1 = 2*n);
    ()

  let append_naive_ v1 v2 =
    CCVector.iter (fun x -> CCVector.push v1 x) v2

  let bench_append n = "append" @> lazy (
    let v2 = CCVector.init n (fun x->n+x) in
    Benchmark.throughputN 2
      [ "append", try_append_ CCVector.append n v2, ()
      ; "append_naive", try_append_ append_naive_ n v2, ()
      ]
    )

  let () = Benchmark.Tree.(register (
    "vector" @>>>
      [ with_int bench_map [100; 10_000; 100_000]
      ; with_int bench_append [100; 10_000; 50_000]
      ]
  ))
end

module Cache = struct
  module C = CCCache

  let make_fib c =
    let f = C.with_cache_rec c
      (fun fib n -> match n with
        | 0 -> 0
        | 1 -> 1
        | 2 -> 1
        | n -> fib (n-1) + fib (n-2)
      )
    in
    fun x ->
      C.clear c;
      f x

  let bench_fib n = "fib" @> lazy (
    let l =
      [ "replacing_fib (128)", make_fib (C.replacing 128), n
      ; "LRU_fib (128)", make_fib (C.lru 128), n
      ; "replacing_fib (16)", make_fib (C.replacing 16), n
      ; "LRU_fib (16)", make_fib (C.lru 16), n
      ; "unbounded", make_fib (C.unbounded 32), n
      ]
    in
    let l = if n <= 20
      then  [ "linear_fib (5)", make_fib (C.linear 5), n
            ; "linear_fib (32)", make_fib (C.linear 32), n
            ; "dummy_fib", make_fib C.dummy, n
            ] @ l
      else l
    in
    Benchmark.throughputN 3 l
  )

  let () = Benchmark.Tree.(register (
    "cache" @>>>
      [ with_int bench_fib [10; 20; 100; 200; 1_000;]
      ]
  ))
end

module Tbl = struct
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

  module IPersistentHashtbl = CCPersistentHashtbl.Make(struct
    type t = int
    let equal i j = i = j
    let hash i = i
  end)

  module IMap = Map.Make(struct
    type t = int
    let compare i j = i - j
  end)

  module ICCHashtbl = CCFlatHashtbl.Make(struct
    type t = int
    let equal i j = i = j
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

  let icchashtbl_add n =
    let h = ICCHashtbl.create 50 in
    for i = n downto 0 do
      ICCHashtbl.add h i i;
    done;
    h

  let bench_maps1 n = "add" @> lazy (
    Benchmark.throughputN 3
      ["phashtbl_add", (fun n -> ignore (phashtbl_add n)), n;
       "hashtbl_add", (fun n -> ignore (hashtbl_add n)), n;
       "ihashtbl_add", (fun n -> ignore (ihashtbl_add n)), n;
       "iflathashtbl_add", (fun n -> ignore (iflathashtbl_add n)), n;
       "ifhashtbl_add", (fun n -> ignore (ifhashtbl_add n)), n;
       "ipersistenthashtbl_add", (fun n -> ignore (ipersistenthashtbl_add n)), n;
       "skiplist_add", (fun n -> ignore (skiplist_add n)), n;
       "imap_add", (fun n -> ignore (imap_add n)), n;
       "ccflathashtbl_add", (fun n -> ignore (icchashtbl_add n)), n;
      ]
  )

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

  let icchashtbl_replace n =
    let h = ICCHashtbl.create 50 in
    for i = 0 to n do
      ICCHashtbl.add h i i;
    done;
    for i = n downto 0 do
      ICCHashtbl.add h i i;
    done;
    h

  let bench_maps2 n = "replace" @> lazy (
    Benchmark.throughputN 3
      ["phashtbl_replace", (fun n -> ignore (phashtbl_replace n)), n;
       "hashtbl_replace", (fun n -> ignore (hashtbl_replace n)), n;
       "ihashtbl_replace", (fun n -> ignore (ihashtbl_replace n)), n;
       "iflathashtbl_replace", (fun n -> ignore (iflathashtbl_replace n)), n;
       "ifhashtbl_replace", (fun n -> ignore (ifhashtbl_replace n)), n;
       "ipersistenthashtbl_replace", (fun n -> ignore (ipersistenthashtbl_replace n)), n;
       "skiplist_replace", (fun n -> ignore (skiplist_replace n)), n;
       "imap_replace", (fun n -> ignore (imap_replace n)), n;
       "ccflathashtbl_replace", (fun n -> ignore (icchashtbl_replace n)), n;
      ])

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

  let icchashtbl_find m =
    fun n ->
      for i = 0 to n-1 do
        ignore (ICCHashtbl.get_exn i m);
      done

  let bench_maps3 n = "find" @> lazy (
    let h = phashtbl_add n in
    let h' = hashtbl_add n in
    let h'' = ihashtbl_add n in
    let h''' = iflathashtbl_add n in
    let h'''' = ifhashtbl_add n in
    let h''''' = ipersistenthashtbl_add n in
    let l = skiplist_add n in
    let a = Array.init n (fun i -> string_of_int i) in
    let m = imap_add n in
    let h'''''' = icchashtbl_add n in
    Benchmark.throughputN 3 [
      "phashtbl_find", (fun () -> phashtbl_find h n), ();
      "hashtbl_find", (fun () -> hashtbl_find h' n), ();
      "ihashtbl_find", (fun () -> ihashtbl_find h'' n), ();
      "iflathashtbl_find", (fun () -> iflathashtbl_find h''' n), ();
      "ifhashtbl_find", (fun () -> ifhashtbl_find h'''' n), ();
      "ipersistenthashtbl_find", (fun () -> ipersistenthashtbl_find h''''' n), ();
      "skiplist_find", (fun () -> skiplist_find l n), ();
      "array_find", (fun () -> array_find a n), ();
      "imap_find", (fun () -> imap_find m n), ();
      "cchashtbl_find", (fun () -> icchashtbl_find h'''''' n), ();
    ])

  let () = Benchmark.Tree.(register (
    "tbl" @>>>
      [ with_int bench_maps1 [10; 100; 1_000; 10_000;]
      ; with_int bench_maps2 [10; 100; 1_000; 10_000]
      ; with_int bench_maps3 [10; 20; 100; 1_000; 10_000]
      ]
  ))
end

module Iter = struct
  (** {2 Sequence/Gen} *)

  let bench_fold n = "fold" @> lazy (
    let seq () = Sequence.fold (+) 0 Sequence.(0 --n) in
    let gen () = Gen.fold (+) 0 Gen.(0 -- n) in
    let klist () = CCKList.fold (+) 0 CCKList.(0 -- n) in
    Benchmark.throughputN 3
      [ "sequence.fold", seq, ();
        "gen.fold", gen, ();
        "klist.fold", klist, ();
      ]
  )

  let bench_flat_map n = "flat_map" @> lazy (
    let seq () = Sequence.(
      0 -- n |> flat_map (fun x -> x-- (x+10)) |> fold (+) 0
    )
    and gen () = Gen.(
      0 -- n |> flat_map (fun x -> x-- (x+10)) |> fold (+) 0
    )
    and klist () = CCKList.(
      0 -- n |> flat_map (fun x -> x-- (x+10)) |> fold (+) 0
    )
    in
    Benchmark.throughputN 3
      [ "sequence.flat_map", seq, ();
        "gen.flat_map", gen, ();
        "klist.flat_map", klist, ();
      ]
  )

  let bench_iter n = "iter" @> lazy (
    let seq () =
      let i = ref 2 in
      Sequence.(
        1 -- n |> iter (fun x -> i := !i * x)
      )
    and gen () =
      let i = ref 2 in
      Gen.(
        1 -- n |> iter (fun x -> i := !i * x)
      )
    and klist () =
      let i = ref 2 in
      CCKList.(
        1 -- n |> iter (fun x -> i := !i * x)
      )
    in
    Benchmark.throughputN 3
      [ "sequence.iter", seq, ();
        "gen.iter", gen, ();
        "klist.iter", klist, ();
      ]
  )

  let () = Benchmark.Tree.(register (
    "iter" @>>>
      [ with_int bench_fold [100; 1_000; 10_000; 1_000_000]
      ; with_int bench_flat_map [1_000; 10_000]
      ; with_int bench_iter [1_000; 10_000]
      ]
  ))
end

module Batch = struct
  (** benchmark CCBatch *)

  open Containers_advanced

  module type COLL = sig
    val name : string
    include CCBatch.COLLECTION
    val doubleton : 'a -> 'a -> 'a t
    val (--) : int -> int -> int t
    val equal : int t -> int t -> bool
  end

  module Make(C : COLL) = struct
    let f1 x = x mod 2 = 0
    let f2 x = -x
    let f3 x = C.doubleton x (x+1)
    let f4 x = -x
    let collect a = C.fold (+) 0 a

    let naive a =
      let a = C.filter f1 a in
      let a = C.flat_map f3 a in
      let a = C.filter f1 a in
      let a = C.map f2 a in
      let a = C.flat_map f3 a in
      let a = C.map f4 a in
      ignore (collect a);
      a

    module BA = CCBatch.Make(C)

    let ops =
      BA.(filter f1 >>> flat_map f3 >>> filter f1 >>>
          map f2 >>> flat_map f3 >>> map f4)

    let batch a =
      let a = BA.apply ops a in
      ignore (collect a);
      a

    let bench_for ~time n = "" @> lazy (
      let a = C.(0 -- n) in
      (* debug
      CCPrint.printf "naive: %a\n" (CCArray.pp CCInt.pp) (naive a);
      CCPrint.printf "simple: %a\n" (CCArray.pp CCInt.pp) (batch_simple a);
      CCPrint.printf "batch: %a\n" (CCArray.pp CCInt.pp) (batch a);
      *)
      assert (C.equal (batch a) (naive a));
      Benchmark.throughputN time
        [ C.name ^ "_naive", naive, a
        ; C.name ^ "_batch", batch, a
        ]
    )

    let bench = Benchmark.(
      C.name @>>>
      [ bench_for ~time:1 100
      ; bench_for ~time:4 100_000
      ; bench_for ~time:4 1_000_000
      ])
  end

  module BenchArray = Make(struct
    include CCArray
    let name = "array"
    let equal a b = a=b
    let doubleton x y = [| x; y |]
    let fold = Array.fold_left
  end)

  module BenchList = Make(struct
    include CCList
    let name = "list"
    let equal a b = a=b
    let doubleton x y = [ x; y ]
    let fold = List.fold_left
  end)

  module BenchKList = Make(struct
    include CCKList
    let name = "klist"
    let equal a b = equal (=) a b
    let doubleton x y = CCKList.of_list [ x; y ]
  end)

  let () = Benchmark.Tree.(register (
    "batch" @>>>
      [ BenchKList.bench
      ; BenchArray.bench
      ; BenchList.bench
      ]
  ))
end

let () =
  Benchmark.Tree.run_global ()
