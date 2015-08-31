(** Generic benchs *)

module B = Benchmark
let (@>) = B.Tree.(@>)
let (@>>) = B.Tree.(@>>)
let (@>>>) = B.Tree.(@>>>)
let (|>) = CCFun.(|>)

let app_int f n = string_of_int n @> lazy (f n)
let app_ints f l = B.Tree.concat (List.map (app_int f) l)

(* composition *)
let (%%) f g x = f (g x)

module L = struct
  (* FLAT MAP *)

  let f_ x =
    if x mod 10 = 0 then []
    else if x mod 5 = 1 then [x;x+1]
    else [x;x+1;x+2;x+3]

  let bench_flat_map ?(time=2) n =
    let l = CCList.(1 -- n) in
    let flatten_map_ l = List.flatten (CCList.map f_ l)
    and flatten_ccmap_ l = List.flatten (List.map f_ l) in
    B.throughputN time
      [ "flat_map", CCList.flat_map f_, l
      ; "flatten o CCList.map", flatten_ccmap_, l
      ; "flatten o map", flatten_map_, l
      ]

  (* APPEND *)

  let append_ f (l1, l2, l3) =
    ignore (f (f l1 l2) l3)

  let bench_append ?(time=2) n =
    let l1 = CCList.(1 -- n) in
    let l2 = CCList.(n+1 -- 2*n) in
    let l3 = CCList.(2*n+1 -- 3*n) in
    let arg = l1, l2, l3 in
    B.throughputN time
      [ "CCList.append", append_ CCList.append, arg
      ; "List.append", append_ List.append, arg
      ]

  (* FLATTEN *)

  let bench_flatten ?(time=2) n =
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
    B.throughputN time
      [ "CCList.flatten", CCList.flatten, l
      ; "List.flatten", List.flatten, l
      ; "fold_right append", fold_right_append_, l
      ; "CCList.(fold_right append)", cc_fold_right_append_, l
      ]

  (* MAIN *)

  let () = B.Tree.register (
    "list" @>>>
      [ "flat_map" @>>
        B.Tree.concat
          [ app_int (bench_flat_map ~time:2) 100
          ; app_int (bench_flat_map ~time:2) 10_000
          ; app_int (bench_flat_map ~time:4) 100_000]
      ; "flatten" @>>
        B.Tree.concat
          [ app_int (bench_flatten ~time:2) 100
          ; app_int (bench_flatten ~time:2) 10_000
          ; app_int (bench_flatten ~time:4) 100_000]
      ; "append" @>>
        B.Tree.concat
          [ app_int (bench_append ~time:2) 100
          ; app_int (bench_append ~time:2) 10_000
          ; app_int (bench_append ~time:4) 100_000]
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
    B.throughputN 2
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
    B.throughputN 2
      [ "append", try_append_ CCVector.append n v2, ()
      ; "append_naive", try_append_ append_naive_ n v2, ()
      ]

  let () = B.Tree.register (
    "vector" @>>>
      [ "map" @>> app_ints bench_map [100; 10_000; 100_000]
      ; "append" @>> app_ints bench_append [100; 10_000; 50_000]
      ]
  )
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

  let bench_fib n =
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
    B.throughputN 3 l

  let () = B.Tree.register (
    "cache" @>>>
      [ "fib" @>> app_ints bench_fib [10; 20; 100; 200; 1_000;]
      ]
  )
end

module Tbl = struct
  module IHashtbl = Hashtbl.Make(struct
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

  let intmap_add n =
    let h = ref CCIntMap.empty in
    for i = n downto 0 do
      h := CCIntMap.add i i !h;
    done;
    !h

  let icchashtbl_add n =
    let h = ICCHashtbl.create 50 in
    for i = n downto 0 do
      ICCHashtbl.add h i i;
    done;
    h

  let bench_maps1 n =
    B.throughputN 3
      ["phashtbl_add", (fun n -> ignore (phashtbl_add n)), n;
       "hashtbl_add", (fun n -> ignore (hashtbl_add n)), n;
       "ihashtbl_add", (fun n -> ignore (ihashtbl_add n)), n;
       "ipersistenthashtbl_add", (fun n -> ignore (ipersistenthashtbl_add n)), n;
       "imap_add", (fun n -> ignore (imap_add n)), n;
       "intmap_add", (fun n -> ignore (intmap_add n)), n;
       "ccflathashtbl_add", (fun n -> ignore (icchashtbl_add n)), n;
      ]

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

  let ipersistenthashtbl_replace n =
    let h = ref (IPersistentHashtbl.create 32) in
    for i = 0 to n do
      h := IPersistentHashtbl.replace !h i i;
    done;
    for i = n downto 0 do
      h := IPersistentHashtbl.replace !h i i;
    done;
    !h

  let imap_replace n =
    let h = ref IMap.empty in
    for i = 0 to n do
      h := IMap.add i i !h;
    done;
    for i = n downto 0 do
      h := IMap.add i i !h;
    done;
    !h

  let intmap_replace n =
    let h = ref CCIntMap.empty in
    for i = 0 to n do
      h := CCIntMap.add i i !h;
    done;
    for i = n downto 0 do
      h := CCIntMap.add i i !h;
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

  let bench_maps2 n =
    B.throughputN 3
      ["phashtbl_replace", (fun n -> ignore (phashtbl_replace n)), n;
       "hashtbl_replace", (fun n -> ignore (hashtbl_replace n)), n;
       "ihashtbl_replace", (fun n -> ignore (ihashtbl_replace n)), n;
       "ipersistenthashtbl_replace", (fun n -> ignore (ipersistenthashtbl_replace n)), n;
       "imap_replace", (fun n -> ignore (imap_replace n)), n;
       "intmap_replace", (fun n -> ignore (intmap_replace n)), n;
       "ccflathashtbl_replace", (fun n -> ignore (icchashtbl_replace n)), n;
      ]

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

  let ipersistenthashtbl_find h =
    fun n ->
      for i = 0 to n-1 do
        ignore (IPersistentHashtbl.find h i);
      done

  let array_find a =
    fun n ->
      for i = 0 to n-1 do
        ignore (Array.get a i);
      done

  let persistent_array_find a =
    fun n ->
      for i = 0 to n-1 do
        ignore (CCPersistentArray.get a i);
      done

  let imap_find m =
    fun n ->
      for i = 0 to n-1 do
        ignore (IMap.find i m);
      done

  let intmap_find m =
    fun n ->
      for i = 0 to n-1 do
        ignore (CCIntMap.find i m);
      done

  let icchashtbl_find m =
    fun n ->
      for i = 0 to n-1 do
        ignore (ICCHashtbl.get_exn i m);
      done

  let bench_maps3 n =
    let h = phashtbl_add n in
    let h' = hashtbl_add n in
    let h'' = ihashtbl_add n in
    let h''''' = ipersistenthashtbl_add n in
    let a = Array.init n string_of_int in
    let pa = CCPersistentArray.init n string_of_int in
    let m = imap_add n in
    let m' = intmap_add n in
    let h'''''' = icchashtbl_add n in
    B.throughputN 3 [
      "phashtbl_find", (fun () -> phashtbl_find h n), ();
      "hashtbl_find", (fun () -> hashtbl_find h' n), ();
      "ihashtbl_find", (fun () -> ihashtbl_find h'' n), ();
      "ipersistenthashtbl_find", (fun () -> ipersistenthashtbl_find h''''' n), ();
      "array_find", (fun () -> array_find a n), ();
      "persistent_array_find", (fun () -> persistent_array_find pa n), ();
      "imap_find", (fun () -> imap_find m n), ();
      "intmap_find", (fun () -> intmap_find m' n), ();
      "ccflathashtbl_find", (fun () -> icchashtbl_find h'''''' n), ();
    ]

  let () = B.Tree.register (
    "tbl" @>>>
      [ "add" @>> app_ints bench_maps1 [10; 100; 1_000; 10_000;]
      ; "replace" @>> app_ints bench_maps2 [10; 100; 1_000; 10_000]
      ; "find" @>> app_ints bench_maps3 [10; 20; 100; 1_000; 10_000]
      ])
end

module Iter = struct
  (** {2 Sequence/Gen} *)

  let bench_fold n =
    let seq () = Sequence.fold (+) 0 Sequence.(0 --n) in
    let gen () = Gen.fold (+) 0 Gen.(0 -- n) in
    let klist () = CCKList.fold (+) 0 CCKList.(0 -- n) in
    B.throughputN 3
      [ "sequence.fold", seq, ();
        "gen.fold", gen, ();
        "klist.fold", klist, ();
      ]

  let bench_flat_map n =
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
    B.throughputN 3
      [ "sequence.flat_map", seq, ();
        "gen.flat_map", gen, ();
        "klist.flat_map", klist, ();
      ]

  let bench_iter n =
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
    B.throughputN 3
      [ "sequence.iter", seq, ();
        "gen.iter", gen, ();
        "klist.iter", klist, ();
      ]

  let () = B.Tree.register (
    "iter" @>>>
      [ "fold" @>> app_ints bench_fold [100; 1_000; 10_000; 1_000_000]
      ; "flat_map" @>> app_ints bench_flat_map [1_000; 10_000]
      ; "iter" @>> app_ints bench_iter [1_000; 10_000]
      ])
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

    let bench_for ~time n =
      let a = C.(0 -- n) in
      (* debug
      CCPrint.printf "naive: %a\n" (CCArray.pp CCInt.pp) (naive a);
      CCPrint.printf "simple: %a\n" (CCArray.pp CCInt.pp) (batch_simple a);
      CCPrint.printf "batch: %a\n" (CCArray.pp CCInt.pp) (batch a);
      *)
      assert (C.equal (batch a) (naive a));
      B.throughputN time
        [ C.name ^ "_naive", naive, a
        ; C.name ^ "_batch", batch, a
        ]

    let bench = B.(
      C.name @>> B.Tree.concat
      [ app_int (bench_for ~time:1) 100
      ; app_int (bench_for ~time:4) 100_000
      ; app_int (bench_for ~time:4) 1_000_000
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

  let () = B.Tree.register (
    "batch" @>> B.Tree.concat
      [ BenchKList.bench
      ; BenchArray.bench
      ; BenchList.bench
      ])
end

module Deque = struct
  module type DEQUE = sig
    type 'a t
    val create : unit -> 'a t
    val of_seq : 'a Sequence.t -> 'a t
    val iter : ('a -> unit) -> 'a t -> unit
    val push_front : 'a t -> 'a -> unit
    val push_back : 'a t -> 'a -> unit
    val is_empty : 'a t -> bool
    val take_front : 'a t -> 'a
    val take_back : 'a t -> 'a
    val append_back : into:'a t -> 'a t -> unit
    val length : _ t -> int
  end

  module Base : DEQUE = struct
    type 'a elt = {
      content : 'a;
      mutable prev : 'a elt;
      mutable next : 'a elt;
    } (** A cell holding a single element *)

    and 'a t = 'a elt option ref
      (** The deque, a double linked list of cells *)

    exception Empty

    let create () = ref None

    let is_empty d =
      match !d with
      | None -> true
      | Some _ -> false

    let push_front d x =
      match !d with
      | None ->
        let rec elt = {
          content = x; prev = elt; next = elt;
        } in
        d := Some elt
      | Some first ->
        let elt = { content = x; prev = first.prev; next=first; } in
        first.prev.next <- elt;
        first.prev <- elt;
        d := Some elt

    let push_back d x =
      match !d with
      | None ->
        let rec elt = {
          content = x; prev = elt; next = elt; } in
        d := Some elt
      | Some first ->
        let elt = { content = x; next=first; prev=first.prev; } in
        first.prev.next <- elt;
        first.prev <- elt

    let take_back d =
      match !d with
      | None -> raise Empty
      | Some first when first == first.prev ->
        (* only one element *)
        d := None;
        first.content
      | Some first ->
        let elt = first.prev in
        elt.prev.next <- first;
        first.prev <- elt.prev;  (* remove [first.prev] from list *)
        elt.content

    let take_front d =
      match !d with
      | None -> raise Empty
      | Some first when first == first.prev ->
        (* only one element *)
        d := None;
        first.content
      | Some first ->
        first.prev.next <- first.next; (* remove [first] from list *)
        first.next.prev <- first.prev;
        d := Some first.next;
        first.content

    let iter f d =
      match !d with
      | None -> ()
      | Some first ->
        let rec iter elt =
          f elt.content;
          if elt.next != first then iter elt.next
        in
        iter first

    let of_seq seq =
      let q =create () in seq (push_back q); q

    let append_back ~into q = iter (push_back into) q

    let length q =
      let n = ref 0 in
      iter (fun _ -> incr n) q;
      !n
  end

  module FQueue : DEQUE = struct
    type 'a t = 'a CCFQueue.t ref
    let create () = ref CCFQueue.empty
    let of_seq s = ref (CCFQueue.of_seq s)
    let iter f q = CCFQueue.iter f !q
    let push_front q x = q:= CCFQueue.cons x !q
    let push_back q x = q:= CCFQueue.snoc !q x
    let is_empty q = CCFQueue.is_empty !q
    let take_front q =
      let x, q' = CCFQueue.take_front_exn !q in
      q := q';
      x
    let take_back q =
      let q', x = CCFQueue.take_back_exn !q in
      q := q';
      x

    let append_back ~into q = into := CCFQueue.append !into !q
    let length q = CCFQueue.size !q
  end

  let base = (module Base : DEQUE)
  let cur = (module CCDeque : DEQUE)
  let fqueue = (module FQueue : DEQUE)

  let bench_iter n =
    let seq = Sequence.(1 -- n) in
    let make (module D : DEQUE) =
      let q = D.of_seq seq in
      fun () ->
        let n = ref 0 in
        D.iter (fun _ -> incr n) q;
        ()
    in
    B.throughputN 3
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let bench_push_front n =
    let make (module D : DEQUE) () =
      let q = D.create() in
      for i=0 to n do D.push_front q i done
    in
    B.throughputN 3
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let bench_push_back n =
    let make (module D : DEQUE) =
      let q = D.create() in
      fun () ->
        for i=0 to n do D.push_back q i done
    in
    B.throughputN 3
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let bench_append n =
    let seq = Sequence.(1 -- n) in
    let make (module D :DEQUE) =
      let q1 = D.of_seq seq in
      let q2 = D.of_seq seq in
      fun () -> D.append_back ~into:q1 q2
    in
    B.throughputN 3
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let bench_length n =
    let seq = Sequence.(1--n) in
    let make (module D:DEQUE) =
      let q = D.of_seq seq in
      fun () -> ignore (D.length q)
    in
    B.throughputN 3
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let () = B.Tree.register (
    "deque" @>>>
    [ "iter" @>> app_ints bench_iter [100; 1_000; 100_000]
    ; "push_front" @>> app_ints bench_push_front [100; 1_000; 100_000]
    ; "push_back" @>> app_ints bench_push_back [100; 1_000; 100_000]
    ; "append_back" @>> app_ints bench_append [100; 1_000; 100_000]
    ; "length" @>> app_ints bench_length [100; 1_000]
    ]
  )
end

let () =
  B.Tree.run_global ()
