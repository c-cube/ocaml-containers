(** Generic benchs *)

module B = Benchmark
let (@>) = B.Tree.(@>)
let (@>>) = B.Tree.(@>>)
let (@>>>) = B.Tree.(@>>>)
let (|>) = CCFun.(|>)

let app_int f n = string_of_int n @> lazy (f n)
let app_ints f l = B.Tree.concat (List.map (app_int f) l)

(* for benchmark *)
let repeat = 3

(* composition *)
let (%%) f g x = f (g x)

module L = struct
  (* MAP *)

  let f_ x = x+1

  let bench_map ?(time=2) n =
    let l = CCList.(1 -- n) in
    let ral = CCRAL.of_list l in
    let map_naive () = ignore (try  List.map f_ l with Stack_overflow -> [])
    and map_tailrec () = ignore (List.rev (List.rev_map f_ l))
    and ccmap () = ignore (CCList.map f_ l)
    and ralmap () = ignore (CCRAL.map ~f:f_ ral)
    in
    B.throughputN time ~repeat
      [ "List.map", map_naive, ()
      ; "List.rev_map o rev", map_tailrec, ()
      ; "CCList.map", ccmap, ()
      ; "CCRAL.map", ralmap, ()
      ]

  (* FLAT MAP *)

  let f_ x =
    if x mod 10 = 0 then []
    else if x mod 5 = 1 then [x;x+1]
    else [x;x+1;x+2;x+3]

  let f_ral_ x =
    if x mod 10 = 0 then CCRAL.empty
    else if x mod 5 = 1 then CCRAL.of_list [x;x+1]
    else CCRAL.of_list [x;x+1;x+2;x+3]

  let bench_flat_map ?(time=2) n =
    let l = CCList.(1 -- n) in
    let ral = CCRAL.of_list l in
    let flatten_map_ l () = ignore @@ List.flatten (CCList.map f_ l)
    and flatmap l () = ignore @@ CCList.flat_map f_ l
    and flatten_ccmap_ l () = ignore @@ List.flatten (List.map f_ l)
    and flatmap_ral_ l () = ignore @@ CCRAL.flat_map f_ral_ l
    in
    B.throughputN time ~repeat
      [ "flat_map", flatmap l, ()
      ; "flatten o CCList.map", flatten_ccmap_ l, ()
      ; "flatten o map", flatten_map_ l, ()
      ; "ral_flatmap", flatmap_ral_ ral, ()
      ]

  (* APPEND *)

  let append_ f (l1, l2, l3) =
    ignore (f (f l1 l2) l3)

  let bench_append ?(time=2) n =
    let l1 = CCList.(1 -- n) in
    let l2 = CCList.(n+1 -- 2*n) in
    let l3 = CCList.(2*n+1 -- 3*n) in
    let arg = l1, l2, l3 in
    B.throughputN time ~repeat
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
    B.throughputN time ~repeat
      [ "CCList.flatten", CCList.flatten, l
      ; "List.flatten", List.flatten, l
      ; "fold_right append", fold_right_append_, l
      ; "CCList.(fold_right append)", cc_fold_right_append_, l
      ]

  (* RANDOM ACCESS *)

  let bench_nth ?(time=2) n =
    let l = CCList.(1 -- n) in
    let ral = CCRAL.of_list l in
    let bench_list l () =
      for i = 0 to n-1 do ignore (List.nth l i) done
    and bench_ral l () =
      for i = 0 to n-1 do ignore (CCRAL.get_exn l i) done
    in
    B.throughputN time ~repeat
      [ "List.nth", bench_list l, ()
      ; "RAL.get", bench_ral ral, ()
      ]

  (* MAIN *)

  let () = B.Tree.register (
    "list" @>>>
      [ "map" @>>
        B.Tree.concat
          [ app_int (bench_map ~time:2) 100
          ; app_int (bench_map ~time:2) 10_000
          ; app_int (bench_map ~time:4) 100_000
          ; app_int (bench_map ~time:4) 500_000 ]
      ; "flat_map" @>>
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
      ; "nth" @>>
        B.Tree.concat
          [ app_int (bench_nth ~time:2) 100
          ; app_int (bench_nth ~time:2) 10_000
          ; app_int (bench_nth ~time:4) 100_000]
      ]
    )
end

module Arr = struct
  let rand = Random.State.make [| 1;2;3;4 |]

  let mk_arr n =
    Array.init n (fun _ -> Random.State.int rand 5_000)

  module IntArr = struct
    type elt=int
    type t = int array
    let get = Array.get
    let set = Array.set
    let length = Array.length
  end

  let sort_ccarray a =
    CCArray.sort_generic (module IntArr) ~cmp:CCInt.compare a

  let sort_std a = Array.sort CCInt.compare a

  (* helper, to apply a sort function over a list of arrays *)
  let app_list sort l =
    List.iter
      (fun a ->
        let a = Array.copy a in
        sort a
      ) l

  let bench_sort ?(time=2) n =
    let a1 = mk_arr n in
    let a2 = mk_arr n in
    let a3 = mk_arr n in
    B.throughputN time ~repeat
      [ "std", app_list sort_std, [a1;a2;a3]
      ; "ccarray.sort_gen", app_list sort_ccarray, [a1;a2;a3]
      ]

  let () =
    B.Tree.register ("array" @>>>
      [ "sort" @>>
        app_ints (bench_sort ?time:None) [100; 1000; 10_000; 50_000; 100_000; 500_000]
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
    B.throughputN 2 ~repeat
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
    B.throughputN 2 ~repeat
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
    B.throughputN 3 l ~repeat

  let () = B.Tree.register (
    "cache" @>>>
      [ "fib" @>> app_ints bench_fib [10; 20; 100; 200; 1_000;]
      ]
  )
end

module Tbl = struct
  (** Signature for mutable map *)
  module type MUT = sig
    type key
    type 'a t
    val name : string
    val find : 'a t -> key -> 'a
    val create : int -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val replace : 'a t -> key -> 'a -> unit
  end

  module type INT_MUT = MUT with type key = int
  module type STRING_MUT = MUT with type key = string

  module type IMMUT = sig
    type key
    type 'a t
    val name : string
    val empty : 'a t
    val find : key -> 'a t -> 'a
    val add : key -> 'a -> 'a t -> 'a t
  end

  module type INT_IMMUT = IMMUT with type key = int

  module MUT_OF_IMMUT(T : IMMUT)
  : MUT with type key = T.key and type 'a t = 'a T.t ref = struct
    type key = T.key
    type 'a t = 'a T.t ref
    let name = T.name
    let create _ = ref T.empty
    let find m k = T.find k !m
    let add m k v = m := T.add k v !m
    let replace = add
  end

  module type KEY = sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  type _ key_type =
    | Int : int key_type
    | Str : string key_type

  let arg_make : type a. a key_type -> (module KEY with type t = a) * string
  = function
    | Int -> (module CCInt), "int"
    | Str ->
        let module S = struct type t = string include CCString end in
        (module S : KEY with type t = string), "string"

  let sprintf = Printf.sprintf

  let hashtbl_make : type a. a key_type -> (module MUT with type key = a)
  = fun key ->
    let (module Key), name = arg_make key in
    let module T = struct
      let name = sprintf "hashtbl(%s)" name
      include Hashtbl.Make(Key)
    end in
    (module T)

  let persistent_hashtbl_ref : type a. a key_type -> (module MUT with type key = a)
  = fun key ->
    let (module Key), name = arg_make key  in
    let module T = Ref_impl.PersistentHashtbl(Key) in
    let module U = struct
      type key = a
      type 'a t = 'a T.t ref
      let name = sprintf "persistent_tbl_old(%s)" name
      let create _ = ref (T.empty ())
      let find m k = T.find !m k
      let add m k v = m := T.replace !m k v
      let replace = add
    end in
    (module U)

  let persistent_hashtbl : type a. a key_type -> (module MUT with type key = a)
  = fun key ->
    let (module Key), name = arg_make key  in
    let module T = CCPersistentHashtbl.Make(Key) in
    let module U = struct
      type key = a
      type 'a t = 'a T.t ref
      let name = sprintf "persistent_tbl(%s)" name
      let create _ = ref (T.empty ())
      let find m k = T.find !m k
      let add m k v = m := T.replace !m k v
      let replace = add
    end in
    (module U)

  let hashtbl =
    let module T = struct
      type key = int
      type 'a t = (int, 'a) Hashtbl.t
      let name = "hashtbl"
      let create i = Hashtbl.create i
      let find = Hashtbl.find
      let add = Hashtbl.add
      let replace = Hashtbl.replace
    end in
    (module T : INT_MUT)

  let map : type a. a key_type -> (module MUT with type key = a)
  = fun k ->
    let (module K), name = arg_make k in
    let module T = struct let name = sprintf "map(%s)" name include Map.Make(K) end in
    let module U = MUT_OF_IMMUT(T) in
    (module U : MUT with type key = a)

  let wbt : type a. a key_type -> (module MUT with type key = a)
  = fun k ->
    let (module K), name = arg_make k in
    let module T = struct
      let name = sprintf "ccwbt(%s)" name
      include CCWBTree.Make(K)
      let find = get_exn
    end in
    let module U = MUT_OF_IMMUT(T) in
    (module U : MUT with type key = a)

  let flat_hashtbl =
    let module T = CCFlatHashtbl.Make(CCInt) in
    let module U = struct
      type key = int
      type 'a t = 'a T.t
      let name = "ccflat_hashtbl"
      let create = T.create
      let find = T.find_exn
      let add = T.add
      let replace = T.add
    end in
    (module U : INT_MUT)

  let trie : (module MUT with type key = string) =
    let module T = struct
      let name = "trie(string)"
      include CCTrie.String
      let find = find_exn
    end in
    let module U = MUT_OF_IMMUT(T) in
    (module U)

  let hashtrie : type a. a key_type -> (module MUT with type key = a)
  = fun k ->
    let (module K), name = arg_make k in
    let module T = struct
      let name = sprintf "cchashtrie(%s)" name
      include CCHashTrie.Make(K)
      let find = get_exn
    end in
    let module U = MUT_OF_IMMUT(T) in
    (module U)

  let hashtrie_mut : type a. a key_type -> (module MUT with type key = a)
  = fun k ->
    let (module K), name = arg_make k in
    let module T = struct
      let name = sprintf "cchashtrie_mut(%s)" name
      type key = K.t
      module M = CCHashTrie.Make(K)
      type 'a t = {
        id: CCHashTrie.Transient.t;
        mutable map: 'a M.t;
      }
      let create _ = { id=CCHashTrie.Transient.create(); map=M.empty}
      let find m k = M.get_exn k m.map
      let add m k v = m.map <- M.add_mut ~id:m.id k v m.map
      let replace = add
    end in
    (module T)

  let hamt : type a. a key_type -> (module MUT with type key = a)
  = fun k ->
    let (module K), name = arg_make k in
    let module T = struct
      let name = sprintf "hamt(%s)" name
      include Hamt.Make(Hamt.StdConfig)(K)
      let find = find_exn
    end in
    let module U = MUT_OF_IMMUT(T) in
    (module U)

  let modules_int =
    [ hashtbl_make Int
    ; hashtbl
    ; persistent_hashtbl Int
    (* ; poly_hashtbl *)
    ; map Int
    ; wbt Int
    ; flat_hashtbl
    ; hashtrie Int
    ; hashtrie_mut Int
    ; hamt Int
    ]

  let modules_string =
    [ hashtbl_make Str
    ; map Str
    ; wbt Str
    ; hashtrie Str
    ; persistent_hashtbl Str
    ; hamt Str
    ; trie
    ]

  let bench_add_to which n =
    let make (module T : INT_MUT) =
      let run() =
        let t = T.create 50 in
        for i = n downto 0 do
          T.add t i i;
        done
      in
      T.name, run, ()
    in
    B.throughputN 3 ~repeat (List.map make which)

  let bench_add = bench_add_to modules_int

  let bench_add_string_to l n =
    let keys = CCList.( 1 -- n |> map (fun i->string_of_int i,i)) in
    let make (module T : STRING_MUT) =
      let run() =
        let t = T.create 50 in
        List.iter
          (fun (k,v) -> T.add t k v)
          keys
      in
      T.name, run, ()
    in
    B.throughputN 3 ~repeat (List.map make l)

  let bench_add_string = bench_add_string_to modules_string

  let bench_replace n =
    let make (module T : INT_MUT) =
      let run() =
        let t = T.create 50 in
        for i = 0 to n do
          T.replace t i i;
        done;
        for i = n downto 0 do
          T.replace t i i;
        done;
        ()
      in
      T.name, run, ()
    in
    B.throughputN 3 ~repeat (List.map make modules_int)

  module type INT_FIND = sig
    type 'a t
    val name : string
    val init : int -> (int -> 'a) -> 'a t
    val find : 'a t -> int -> 'a
  end

  let find_of_mut (module T : INT_MUT) : (module INT_FIND) =
    let module U = struct
      include T
      let init n f =
        let t = T.create n in
        for i=0 to n-1 do T.add t i (f i) done;
        t
    end in
    (module U)

  let array =
    let module T = struct
      type 'a t = 'a array
      let name = "array"
      let init = Array.init
      let find a i = a.(i)
    end in
    (module T : INT_FIND)

  let persistent_array =
    let module A = CCPersistentArray in
    let module T = struct
      type 'a t = 'a A.t
      let name = "persistent_array"
      let init = A.init
      let find = A.get
    end in
    (module T : INT_FIND)

  let modules_int_find =
    [ array
    ; persistent_array ] @
    List.map find_of_mut modules_int

  let bench_find_to which n =
    let make (module T : INT_FIND) =
      let m = T.init n (fun i -> i) in
      let run() =
        for i = 0 to n-1 do
          ignore (T.find m i)
        done
      in
      T.name, run, ()
    in
    Benchmark.throughputN 3 ~repeat (List.map make which)

  let bench_find = bench_find_to modules_int_find

  let bench_find_string_to l n =
    let keys = CCList.( 1 -- n |> map (fun i->string_of_int i,i)) in
    let make (module T : STRING_MUT) =
      let m = T.create n in
      List.iter (fun (k,v) -> T.add m k v) keys;
      let run() =
        List.iter
          (fun (k,_) -> ignore (T.find m k))
          keys
      in
      T.name, run, ()
    in
    Benchmark.throughputN 3 ~repeat (List.map make l)

  let bench_find_string = bench_find_string_to modules_string

  let () =
    B.Tree.register ("tbl" @>>>
      [ "add_int" @>> app_ints bench_add [10; 100; 1_000; 10_000;]
      ; "add_string" @>> app_ints bench_add_string [10; 100; 1_000; 10_000;]
      ; "replace" @>> app_ints bench_replace [10; 100; 1_000; 10_000]
      ; "find" @>> app_ints bench_find [10; 20; 100; 1_000; 10_000]
      ; "find_string" @>> app_ints bench_find_string [10; 20; 100; 1_000; 10_000]
      ]);
    B.Tree.register ("tbl_persistent" @>>>
      (* we also compare to the regular Hashtbl, as a frame of reference *)
      let l_int = [persistent_hashtbl Int; persistent_hashtbl_ref Int; hashtbl_make Int ] in
      let l_str = [persistent_hashtbl Str; persistent_hashtbl_ref Str; hashtbl_make Str ] in
      [ "add_int" @>> app_ints (bench_add_to l_int) [10; 100; 1_000; 10_000;]
      ; "find_int" @>> app_ints
          (bench_find_to (List.map find_of_mut l_int))
          [10; 20; 100; 1_000; 10_000]
      ; "add_string" @>> app_ints
          (bench_add_string_to l_str) [10; 100; 1_000; 10_000;]
      ; "find_string" @>> app_ints
          (bench_find_string_to l_str) [10; 20; 100; 1_000; 10_000]
      ]);
    ()
end

module Iter = struct
  (** {2 Sequence/Gen} *)

  let bench_fold n =
    let seq () = Sequence.fold (+) 0 Sequence.(0 --n) in
    let gen () = Gen.fold (+) 0 Gen.(0 -- n) in
    let klist () = CCKList.fold (+) 0 CCKList.(0 -- n) in
    B.throughputN 3 ~repeat
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
    B.throughputN 3 ~repeat
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
    B.throughputN 3 ~repeat
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
      B.throughputN time ~repeat
        [ C.name ^ "_naive", naive, a
        ; C.name ^ "_batch", batch, a
        ]

    let bench =
      C.name @>> B.Tree.concat
      [ app_int (bench_for ~time:1) 100
      ; app_int (bench_for ~time:4) 100_000
      ; app_int (bench_for ~time:4) 1_000_000
      ]
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
    B.throughputN 3 ~repeat
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let bench_push_front n =
    let make (module D : DEQUE) () =
      let q = D.create() in
      for i=0 to n do D.push_front q i done
    in
    B.throughputN 3 ~repeat
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
    B.throughputN 3 ~repeat
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
    B.throughputN 3 ~repeat
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
    B.throughputN 3 ~repeat
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

module Thread = struct
  module Q = CCBlockingQueue

  module type TAKE_PUSH = sig
    val take : 'a Q.t -> 'a
    val push : 'a Q.t -> 'a -> unit
    val take_list: 'a Q.t -> int -> 'a list
    val push_list : 'a Q.t -> 'a list -> unit
  end

  let cur = (module Q : TAKE_PUSH)
  let naive =
    let module Q = struct
      let take = Q.take
      let push = Q.push
      let push_list q l = List.iter (push q) l
      let rec take_list q n =
        if n=0 then []
        else
          let x = take q in
          x :: take_list q (n-1)
    end in
    (module Q : TAKE_PUSH)

  (* n senders, n receivers *)
  let bench_queue ~size ~senders ~receivers n =
    let make (module TP : TAKE_PUSH) =
      let l = CCList.(1 -- n) in
      fun () ->
        let q = Q.create size in
        let res = CCLock.create 0 in
        let expected_res = 2 * senders * Sequence.(1 -- n |> fold (+) 0) in
        let a_senders = CCThread.Arr.spawn senders
          (fun _ ->
            TP.push_list q l;
            TP.push_list q l
          )
        and a_receivers = CCThread.Arr.spawn receivers
          (fun _ ->
            let l1 = TP.take_list q n in
            let l2 = TP.take_list q n in
            let n = List.fold_left (+) 0 l1 + List.fold_left (+) 0 l2 in
            CCLock.update res ((+) n);
            ()
          )
        in
        CCThread.Arr.join a_senders;
        CCThread.Arr.join a_receivers;
        assert (expected_res = CCLock.get res);
        ()
    in
    B.throughputN 3 ~repeat
      [ "cur", make cur, ()
      ; "naive", make naive, ()
      ]

  let fib_pool_ ~size n =
    let module P = CCPool.Make(struct let min_size = 0 let max_size = size end) in
    let open P.Fut.Infix in
    let rec fib n =
      if n<=1 then P.Fut.return 1
      else
        let f1 = fib (n-1)
        and f2 = fib (n-2) in
        P.Fut.return (+) <*> f1 <*> f2
    in
    P.Fut.get (fib n)

  let fib_manual n =
    let rec fib n =
      if n<= 1  then 1
      else fib (n-1) + fib (n-2)
    in
    fib n

  (* pool of size [size] *)
  let bench_pool ~size n =
    assert (fib_manual n = fib_pool_ ~size n);
    B.throughputN 3 ~repeat
      [ "sequential", fib_manual, n
      ; "pool", fib_pool_ ~size, n
      ]

  let bench_sequence ~size n =
    let module P = CCPool.Make(struct let min_size = 0 let max_size = size end) in
    let id_ x = Thread.delay 0.0001; x in
    let mk_list() = CCList.init n (P.Fut.make1 id_) in
    let mk_sequence () =
      let l = mk_list() in
      P.Fut.sequence_l l |> P.Fut.get
    (* reserves a thread for the computation *)
    and mk_blocking () =
      let l = mk_list() in
      P.Fut.make (fun () -> List.map P.Fut.get l) |> P.Fut.get
    in
    B.throughputN 3 ~repeat
      [ "sequence", mk_sequence, ()
      ; "blocking", mk_blocking, ()
      ]

  let () = B.Tree.register (
    let take_push = CCList.map
      (fun (size,senders,receivers) ->
        Printf.sprintf "queue.take/push (size=%d,senders=%d,receivers=%d)"
          size senders receivers
        @>>
        app_ints (bench_queue ~size ~senders ~receivers)
          [100; 1_000]
      ) [ 2, 3, 3
        ; 5, 3, 3
        ; 1, 5, 5
        ; 2, 10, 10
        ; 5, 10, 10
        ; 20, 10, 10
      ]
    in

    "thread" @>>>
      ( take_push @
      [ "fib_size5" @>> app_ints (bench_pool ~size:5) [10; 15; 30; 35]
      ; "fib_size15" @>> app_ints (bench_pool ~size:15) [10; 15; 30; 35]
      ; "sequence" @>> app_ints (bench_sequence ~size:15) [100; 500; 10_000; 100_000]
      ]
      )
  )
end

module Graph = struct
  (* divisors graph *)
  let div_children_ i =
    (* divisors of [i] that are [>= j] *)
    let rec aux j i yield =
      if j < i
      then (
        if (i mod j = 0) then yield (i,j);
        aux (j+1) i yield
      )
    in
    aux 1 i

  let div_graph_ = {CCGraph.
    origin=fst;
    dest=snd;
    children=div_children_
  }

  module H = Hashtbl.Make(CCInt)

  let dfs_raw n () =
    let explored = H.create (n+10) in
    let st = Stack.create() in
    let res = ref 0 in
    Stack.push n st;
    while not (Stack.is_empty st) do
      let i = Stack.pop st in
      if not (H.mem explored i) then (
        H.add explored i ();
        incr res;
        div_children_ i (fun (_,j) -> Stack.push j st);
      )
      done;
    !res

  let dfs_ n () =
    let tbl = CCGraph.mk_table ~eq:CCInt.equal ~hash:CCInt.hash (n+10) in
    CCGraph.Traverse.dfs ~tbl ~graph:div_graph_
      (Sequence.return n)
    |> Sequence.fold (fun acc _ -> acc+1) 0

  let dfs_event n () =
    let tbl = CCGraph.mk_table ~eq:CCInt.equal ~hash:CCInt.hash (n+10) in
    CCGraph.Traverse.Event.dfs ~tbl ~graph:div_graph_
      (Sequence.return n)
    |> Sequence.fold
      (fun acc -> function
        | `Enter _ -> acc+1
        | `Exit _
        | `Edge _ -> acc)
      0

  let bench_dfs n =
    assert (
      let n1 = dfs_raw n () in
      let n2 = dfs_ n ()  in
      let n3 = dfs_event n () in
      n1 = n2 &&
      n2 = n3);
    B.throughputN 2 ~repeat
      [ "raw", dfs_raw n, ()
      ; "ccgraph", dfs_ n, ()
      ; "ccgraph_event", dfs_event n, ()
      ]

  let () =
    B.Tree.register ("graph" @>>>
      [ "dfs" @>>
        app_ints bench_dfs [100; 1000; 10_000; 50_000; 100_000; 500_000]
      ]
    )
end

module Str = struct
  (* random string, but always returns the same for a given size *)
  let rand_str_ ?(among="abcdefgh") n =
    let module Q = QCheck in
    let st = Random.State.make [| n + 17 |] in
    let gen_c = QCheck.Gen.oneofl (CCString.to_list among) in
    QCheck.Gen.string_size ~gen:gen_c (QCheck.Gen.return n) st

  let find ?(start=0) ~sub s =
    let n = String.length sub in
    let i = ref start in
    try
      while !i + n <= String.length s do
        if CCString.is_sub ~sub 0 s !i ~len:n then raise Exit;
        incr i
      done;
      -1
    with Exit ->
      !i

  let rfind ~sub s =
    let n = String.length sub in
    let i = ref (String.length s - n) in
    try
      while !i >= 0 do
        if CCString.is_sub ~sub 0 s !i ~len:n then raise Exit;
        decr i
      done;
      ~-1
    with Exit ->
      !i

  let find_all ?(start=0) ~sub s =
    let i = ref start in
    fun () ->
      let res = find ~sub s ~start:!i in
      if res = ~-1 then None
      else (
        i := res + 1;
        Some res
      )

  let find_all_l ?start ~sub s = find_all ?start ~sub s |> Gen.to_list

  let pp_pb needle haystack =
    Format.printf "search needle `%s` in `%s`...@."
      needle (String.sub haystack 0 (min 300 (String.length haystack)))

  (* benchmark String.{,r}find *)
  let bench_find_ ~dir ~size n =
    let needle = rand_str_ size in
    let haystack = rand_str_ n in
    pp_pb needle haystack;
    let mk_naive = match dir with
      | `Direct -> fun () -> find ~sub:needle haystack
      | `Reverse -> fun () -> rfind ~sub:needle haystack
    and mk_current = match dir with
      | `Direct -> fun () -> CCString.find ~sub:needle haystack
      | `Reverse -> fun () -> CCString.rfind ~sub:needle haystack
    and mk_current_compiled = match dir with
      | `Direct -> let f = CCString.find ~start:0 ~sub:needle in fun () -> f haystack
      | `Reverse -> let f = CCString.rfind ~sub:needle in fun () -> f haystack
    in
    assert (mk_naive () = mk_current ());
    B.throughputN 3 ~repeat
      [ "naive", mk_naive, ()
      ; "current", mk_current, ()
      ; "current_compiled", mk_current_compiled, ()
      ]

  (* benchmark String.find_all *)
  let bench_find_all ~size n =
    let needle = rand_str_ size in
    let haystack = rand_str_ n in
    pp_pb needle haystack;
    let mk_naive () = find_all_l ~sub:needle haystack
    and mk_current () = CCString.find_all_l ~sub:needle haystack
    and mk_current_compiled =
      let f = CCString.find_all_l ~start:0 ~sub:needle in fun () -> f haystack in
    assert (mk_naive () = mk_current ());
    B.throughputN 3 ~repeat
      [ "naive", mk_naive, ()
      ; "current", mk_current, ()
      ; "current_compiled", mk_current_compiled, ()
      ]

  (* benchmark String.find_all on constant strings *)
  let bench_find_all_special ~size n =
    let needle = CCString.repeat "a" (size-1) ^ "b" in
    let haystack = CCString.repeat "a" n in
    pp_pb needle haystack;
    let mk_naive () = find_all_l ~sub:needle haystack
    and mk_current () = CCString.find_all_l ~sub:needle haystack in
    assert (mk_naive () = mk_current ());
    B.throughputN 3 ~repeat
      [ "naive", mk_naive, ()
      ; "current", mk_current, ()
      ]

  let bench_find  = bench_find_ ~dir:`Direct
  let bench_rfind  = bench_find_ ~dir:`Reverse

  let () = B.Tree.register (
    "string" @>>>
      [ "find" @>>>
          [ "3" @>> app_ints (bench_find ~size:3) [100; 100_000; 500_000]
          ; "5" @>> app_ints (bench_find ~size:5) [100; 100_000; 500_000]
          ; "15" @>> app_ints (bench_find ~size:15) [100; 100_000; 500_000]
          ; "50" @>> app_ints (bench_find ~size:50) [100; 100_000; 500_000]
          ; "500" @>> app_ints (bench_find ~size:500) [100_000; 500_000]
          ];
        "find_all" @>>>
          [ "1" @>> app_ints (bench_find_all ~size:1) [100; 100_000; 500_000]
          ; "3" @>> app_ints (bench_find_all ~size:3) [100; 100_000; 500_000]
          ; "5" @>> app_ints (bench_find_all ~size:5) [100; 100_000; 500_000]
          ; "15" @>> app_ints (bench_find_all ~size:15) [100; 100_000; 500_000]
          ; "50" @>> app_ints (bench_find_all ~size:50) [100; 100_000; 500_000]
          ; "500" @>> app_ints (bench_find_all ~size:500) [100_000; 500_000]
          ; "special" @>>>
            [ "6" @>> app_ints (bench_find_all_special ~size:6) [100_000; 500_000]
            ; "30" @>> app_ints (bench_find_all_special ~size:30) [100_000; 500_000]
            ; "100" @>> app_ints (bench_find_all_special ~size:100) [100_000; 500_000]
            ]
          ];
        "rfind" @>>>
          [ "3" @>> app_ints (bench_rfind ~size:3) [100; 100_000; 500_000]
          ; "15" @>> app_ints (bench_rfind ~size:15) [100; 100_000; 500_000]
          ; "50" @>> app_ints (bench_rfind ~size:50) [100; 100_000; 500_000]
          ; "500" @>> app_ints (bench_rfind ~size:500) [100_000; 500_000]
          ];
      ])

end

module Alloc = struct
  module type ALLOC_ARR = sig
    type 'a t
    val name : string
    val create : int -> 'a t
    val make : 'a t -> int -> 'a -> 'a array
    val free : 'a t -> 'a array -> unit
  end

  let dummy =
    let module A = struct
      type _ t = unit
      let name = "dummy"
      let create _ = ()
      let make _ i x = Array.make i x
      let free _ _ = ()
    end in
    (module A : ALLOC_ARR)

  let alloc_cache ~buck_size =
    let module A = struct
      type 'a t = 'a CCAllocCache.Arr.t
      let name = Printf.sprintf "alloc_cache(%d)" buck_size
      let create n = CCAllocCache.Arr.create ~buck_size n
      let make = CCAllocCache.Arr.make
      let free = CCAllocCache.Arr.free
    end in
    (module A : ALLOC_ARR)

  (* repeat [n] times:
    - repeat [batch] times:
      - allocate [batch] arrays of size from 1 to batch+1
    - free those arrays
  *)
  let bench1 ~batch n =
    let make (module C : ALLOC_ARR) () =
      let c = C.create (batch*2) in
      let tmp = Array.make (batch * batch) [||] in (* temporary storage *)
      for _ = 1 to n do
        for j = 0 to batch-1 do
          for k = 0 to batch-1 do
            tmp.(j*batch + k) <- C.make c (k+1) '_';
          done;
        done;
        Array.iter (C.free c) tmp (* free the whole array *)
      done
    in
    B.throughputN 3 ~repeat
      [ "dummy", make dummy, ()
      ; "cache(5)", make (alloc_cache ~buck_size:5), ()
      ; "cache(20)", make (alloc_cache ~buck_size:20), ()
      ; "cache(50)", make (alloc_cache ~buck_size:50), ()
      ]

  let () = B.Tree.register (
    "alloc" @>>>
      [ "bench1(batch=5)" @>>
          app_ints (bench1 ~batch:5) [100; 1_000]
      ; "bench1(batch=15)" @>>
          app_ints (bench1 ~batch:15) [100; 1_000]
      ; "bench1(batch=50)" @>>
          app_ints (bench1 ~batch:50) [100; 1_000]
      ]
    )
end

let () =
  try B.Tree.run_global ()
  with Arg.Help msg -> print_endline msg
