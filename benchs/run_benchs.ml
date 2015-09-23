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
    and ralmap () = ignore (CCRAL.map f_ ral)
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

  let bench_flat_map ?(time=2) n =
    let l = CCList.(1 -- n) in
    let flatten_map_ l = List.flatten (CCList.map f_ l)
    and flatten_ccmap_ l = List.flatten (List.map f_ l) in
    B.throughputN time ~repeat
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
      let name = sprintf "hashtbl.make(%s)" name
      include Hashtbl.Make(Key)
    end in
    (module T)

  let persistent_hashtbl =
    let module T = CCPersistentHashtbl.Make(CCInt) in
    let module U = struct
      type key = int
      type 'a t = 'a T.t ref
      let name = "ccpersistent_hashtbl"
      let create _ = ref (T.empty ())
      let find m k = T.find !m k
      let add m k v = m := T.replace !m k v
      let replace = add
    end in
    (module U : INT_MUT)

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
    ; persistent_hashtbl
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
    ; hamt Str
    ; trie
    ]

  let bench_add n =
    let make (module T : INT_MUT) =
      let run() =
        let t = T.create 50 in
        for i = n downto 0 do
          T.add t i i;
        done
      in
      T.name, run, ()
    in
    B.throughputN 3 ~repeat (List.map make modules_int)

  let bench_add_string n =
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
    B.throughputN 3 ~repeat (List.map make modules_string)

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

  let bench_find n =
    let make (module T : INT_FIND) =
      let m = T.init n (fun i -> i) in
      let run() =
        for i = 0 to n-1 do
          ignore (T.find m i)
        done
      in
      T.name, run, ()
    in
    Benchmark.throughputN 3 ~repeat (List.map make modules_int_find)

  let bench_find_string n =
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
    Benchmark.throughputN 3 ~repeat (List.map make modules_string)

  let () = B.Tree.register (
    "tbl" @>>>
      [ "add_int" @>> app_ints bench_add [10; 100; 1_000; 10_000;]
      ; "add_string" @>> app_ints bench_add_string [10; 100; 1_000; 10_000;]
      ; "replace" @>> app_ints bench_replace [10; 100; 1_000; 10_000]
      ; "find" @>> app_ints bench_find [10; 20; 100; 1_000; 10_000]
      ; "find_string" @>> app_ints bench_find_string [10; 20; 100; 1_000; 10_000]
      ])
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
  module Q = CCThread.Queue

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
      []
      )
  )
end

let () =
  B.Tree.run_global ()
