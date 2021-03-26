(** Generic benchs *)

[@@@warning "-5"]

module B = Benchmark
let (@>) = B.Tree.(@>)
let (@>>) = B.Tree.(@>>)
let (@>>>) = B.Tree.(@>>>)
let (|>) = CCFun.(|>)

module Int_map = Map.Make(CCInt)

let app_int f n = string_of_int n @> lazy (f n)
let app_ints f l = B.Tree.concat (List.map (app_int f) l)

(* for benchmark *)
let repeat = 3

(* composition *)
let (%%) f g x = f (g x)

let opaque_ignore x = ignore (Sys.opaque_identity x)

module L = struct
  let bench_iter ?(time=2) n =
    let f i = opaque_ignore i in
    let l = CCList.(1 -- n) in
    let ral = CCRAL.of_list l in
    let vec = CCFun_vec.of_list l in
    let sek = Sek.Persistent.of_array 0 (Array.of_list l) in
    let iter_list () = List.iter f l
    and raliter () = CCRAL.iter ~f ral
    and funvec_iter () = CCFun_vec.iter ~f vec
    and sek_iter () = Sek.Persistent.iter Sek.forward f sek
    in
    B.throughputN time ~repeat
      [ "List.iter", iter_list, ()
      ; "CCRAL.iter", raliter, ()
      ; "CCFun_vec.iter", funvec_iter, ()
      ; "Sek.Persistent.iter", sek_iter, ()
      ]

  (* MAP *)

  let f_ x = x+1

  let rec map_naive f l = match l with
    | [] -> []
    | x :: tail ->
      let y = f x in
      y :: map_naive f tail

  let bench_map ?(time=2) n =
    let l = CCList.(1 -- n) in
    let ral = CCRAL.of_list l in
    let map_naive () = ignore (try  List.map f_ l with Stack_overflow -> [])
    and map_naive2 () = ignore (try  map_naive f_ l with Stack_overflow -> [])
    and map_tailrec () = ignore (List.rev (List.rev_map f_ l))
    and ccmap () = ignore (CCList.map f_ l)
    and ralmap () = ignore (CCRAL.map ~f:f_ ral)
    in
    B.throughputN time ~repeat
      [ "List.map", map_naive, ()
      ; "List.map(inline)", map_naive2, ()
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
    let v1 = CCFun_vec.of_list l1 in
    let v2 = CCFun_vec.of_list l2 in
    let v3 = CCFun_vec.of_list l3 in
    let s1 = Sek.Persistent.of_array 0 (Array.of_list l1) in
    let s2 = Sek.Persistent.of_array 0 (Array.of_list l2) in
    let s3 = Sek.Persistent.of_array 0 (Array.of_list l3) in
    let bench_list l1 l2 l3 () = opaque_ignore (List.(append (append l1 l2) l3)) in
    let bench_cclist l1 l2 l3 () = opaque_ignore (CCList.(append (append l1 l2) l3)) in
    let bench_funvec l1 l2 l3 () = opaque_ignore (CCFun_vec.(append (append l1 l2) l3)) in
    let bench_sek l1 l2 l3 () = opaque_ignore (Sek.Persistent.(concat (concat l1 l2) l3)) in
    B.throughputN time ~repeat
      [ "CCList.append", bench_list l1 l2 l3, ()
      ; "List.append", bench_cclist l1 l2 l3, ()
      ; "CCFun_vec.append", bench_funvec v1 v2 v3, ()
      ; "Sek.concat", bench_sek s1 s2 s3, ()
      ]

  (* FLATTEN *)

  let bench_flatten ?(time=2) n =
    let fold_right_append_ l =
      List.fold_right List.append l []
    and cc_fold_right_append_ l =
      CCList.fold_right CCList.append l []
    in
    let l =
      CCList.mapi
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
    let l = CCList.(0 -- (n - 1)) in
    let ral = CCRAL.of_list l in
    let v = CCFun_vec.of_list l in
    let bv = BatVect.of_list l in
    let map = List.fold_left (fun map i -> Int_map.add i i map) Int_map.empty l in
    let sek = Sek.Persistent.of_array 0 (Array.of_list l) in
    let bench_list l () =
      for i = 0 to n-1 do opaque_ignore (List.nth l i) done
    and bench_map l () =
      for i = 0 to n-1 do opaque_ignore (Int_map.find i l) done
    and bench_ral l () =
      for i = 0 to n-1 do opaque_ignore (CCRAL.get_exn l i) done
    and bench_funvec l () =
      for i = 0 to n-1 do opaque_ignore (CCFun_vec.get_exn i l) done
    and bench_batvec l () =
      for i = 0 to n-1 do opaque_ignore (BatVect.get l i) done
    and bench_sek l () =
      for i = 0 to n-1 do opaque_ignore (Sek.Persistent.get l i) done
    in
    B.throughputN time ~repeat
      [ "List.nth", bench_list l, ()
      ; "Map.find", bench_map map, ()
      ; "RAL.get", bench_ral ral, ()
      ; "funvec.get", bench_funvec v, ()
      ; "batvec.get", bench_batvec bv, ()
      ; "Sek.Persistent.get", bench_sek sek, ()
      ]

  let bench_set ?(time=2) n =
    let l = CCList.(0 -- (n - 1)) in
    let ral = CCRAL.of_list l in
(*     let v = CCFun_vec.of_list l in *)
    let bv = BatVect.of_list l in
    let sek = Sek.Persistent.of_array 0 (Array.of_list l) in
    let map = List.fold_left (fun map i -> Int_map.add i i map) Int_map.empty l in
    let bench_map l () =
      for i = 0 to n-1 do opaque_ignore (Int_map.add i (-i) l) done
    and bench_ral l () =
      for i = 0 to n-1 do opaque_ignore (CCRAL.set l i (-i)) done
(*
    and bench_funvec l () =
      for _i = 0 to n-1 do opaque_ignore ((* TODO *)) done
*)
    and bench_batvec l () =
      for i = 0 to n-1 do opaque_ignore (BatVect.set l i (-i)) done
    and bench_sek l () =
      for i = 0 to n-1 do opaque_ignore (Sek.Persistent.set l i (-i)) done
    in
    B.throughputN time ~repeat
      [ "Map.add", bench_map map, ()
      ; "RAL.set", bench_ral ral, ()
(*       ; "funvec.set", bench_funvec v, () *)
      ; "batvec.set", bench_batvec bv, ()
      ; "Sek.Persistent.set", bench_sek sek, ()
      ]

  let bench_push ?(time=2) n =
  (*let ral = ref CCRAL.empty in *)
    let v = ref CCFun_vec.empty in
    let bv = ref BatVect.empty in
    let map = ref Int_map.empty in
    let sek = ref (Sek.Persistent.create 0) in
    let bench_map l () =
      for i = 0 to n-1 do l := Int_map.add i i !l done; opaque_ignore l
  (*
      and bench_ral l () =
        (* Note: Better implementation probably possible *)
        for i = 0 to n-1 do l := CCRAL.append !l (CCRAL.return i) done; opaque_ignore l
  *)
    and bench_funvec l () =
      for i = 0 to n-1 do l := CCFun_vec.push i !l done; opaque_ignore l
    and bench_batvec l () =
      for i = 0 to n-1 do l := BatVect.append i !l done; opaque_ignore l
    and bench_sek l () =
      for i = 0 to n-1 do l := Sek.Persistent.push Sek.front !l i done; opaque_ignore l
    in
    B.throughputN time ~repeat
      [ "Map.add", bench_map map, ()
(*       ; "RAL.append", bench_ral ral, () *) (* too slow *)
      ; "Sek.Persistent.push", bench_sek sek, ()
      ; "funvec.push", bench_funvec v, ()
      ; "batvec.append", bench_batvec bv, ()
      ]

  let bench_pop ?(time=2) n =
    let l = CCList.(0 -- (n - 1)) in
    let ral = CCRAL.of_list l in
    let v = CCFun_vec.of_list l in
    let bv = BatVect.of_list l in
    let map = List.fold_left (fun map i -> Int_map.add i i map) Int_map.empty l in
    let sek = Sek.Persistent.of_array 0 (Array.of_list l) in
    let bench_map l () =
      let l = ref l in
      for i = 0 to n-1 do l := Int_map.remove i !l done; opaque_ignore l
    and bench_ral l () =
      let l = ref l in
      for _ = 0 to n-1 do l := CCRAL.tl !l done; opaque_ignore l
    and bench_funvec l () =
      let l = ref l in
      for _ = 0 to n-1 do l := snd (CCFun_vec.pop_exn !l) done; opaque_ignore l
    and bench_batvec l () =
      let l = ref l in
      for _ = 0 to n-1 do l := snd (BatVect.pop !l) done; opaque_ignore l
    and bench_sek l () =
      let l = ref l in
      for _ = 0 to n-1 do l := snd (Sek.Persistent.pop Sek.back !l) done; opaque_ignore l
    in
    B.throughputN time ~repeat
      [ "Map.remove", bench_map map, ()
      ; "RAL.tl", bench_ral ral, ()
      ; "funvec.pop", bench_funvec v, ()
      ; "batvec.pop", bench_batvec bv, ()
      ; "Sek.Persistent.pop", bench_sek sek, ()
      ]

  (* MAIN *)

  let () = B.Tree.register (
    "list" @>>>
      [ "iter" @>>
        B.Tree.concat
          [ app_int (bench_iter ~time:2) 100
          ; app_int (bench_iter ~time:2) 10_000
          ; app_int (bench_iter ~time:4) 100_000 ]
      ; "map" @>>
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
      ; "set" @>>
        B.Tree.concat
          [ app_int (bench_set ~time:2) 100
          ; app_int (bench_set ~time:2) 10_000
          ; app_int (bench_set ~time:4) 100_000]
      ; "push" @>>
        B.Tree.concat
          [ app_int (bench_push ~time:2) 100
          ; app_int (bench_push ~time:2) 10_000
          ; app_int (bench_push ~time:4) 100_000]
      ; "pop" @>>
        B.Tree.concat
          [ app_int (bench_pop ~time:2) 100
          ; app_int (bench_pop ~time:2) 10_000
          ; app_int (bench_pop ~time:4) 100_000]
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

  module Quicksort_ref = struct
    module A = Array
    module Rand = Random.State

    let seed_ = [|123456|]

    type state = {
      mutable l: int; (* left pointer *)
      mutable g: int; (* right pointer *)
      mutable k: int;
    }

    let rand_idx_ rand i j = i + Rand.int rand (j-i)

    let swap_ a i j =
      if i=j then ()
      else (
        let tmp = A.get a i in
        A.set a i (A.get a j);
        A.set a j tmp
      )

    (* limit: under which we switch to insertion *)
    let sort ~limit ~cmp a =
      let rec insert_ a i k =
        if k<i then ()
        else if cmp (A.get a k) (A.get a (k+1)) > 0 then (
          swap_ a k (k+1);
          insert_ a i (k-1)
        )
      in
      (* recursive part of insertion sort *)
      let rec sort_insertion_rec a i j k =
        if k<j then (
          insert_ a i (k-1);
          sort_insertion_rec a i j (k+1)
        )
      in
      (* insertion sort, for small slices *)
      let sort_insertion a i j =
        if j-i > 1 then sort_insertion_rec a i j (i+1)
      in
      let rand = Rand.make seed_ in
      (* sort slice.
         There is a chance that the two pivots are equal, but it's unlikely. *)
      let rec sort_slice_ ~st a i j =
        if j-i>limit then (
          st.l <- i;
          st.g <- j-1;
          st.k <- i;
          (* choose pivots *)
          let p = A.get a (rand_idx_ rand i j) in
          let q = A.get a (rand_idx_ rand i j) in
          (* invariant: st.p <= st.q, swap them otherwise *)
          let p, q = if cmp p q > 0 then q, p else p, q in
          while st.k <= st.g do
            let cur = A.get a st.k in
            if cmp cur p < 0 then (
              (* insert in leftmost band *)
              if st.k <> st.l then swap_ a st.k st.l;
              st.l <- st.l + 1
            ) else if cmp cur q > 0 then (
              (* insert in rightmost band *)
              while st.k < st.g && cmp (A.get a st.g) q > 0 do
                st.g <- st.g - 1
              done;
              swap_ a st.k st.g;
              st.g <- st.g - 1;
              (* the element swapped from the right might be in the first situation.
                 that is, < p  (we know it's <= q already) *)
              if cmp (A.get a st.k) p < 0 then (
                if st.k <> st.l then swap_ a st.k st.l;
                st.l <- st.l + 1
              )
            );
            st.k <- st.k + 1
          done;
          (* save values before recursing *)
          let l = st.l and g = st.g and sort_middle = cmp p q < 0 in
          sort_slice_ ~st a i l;
          if sort_middle then sort_slice_ ~st a l (g+1);
          sort_slice_ ~st a (g+1) j;
        ) else sort_insertion a i j
      in
      if A.length a > 0 then (
        let st = { l=0; g=A.length a; k=0; } in
        sort_slice_ ~st a 0 (A.length a)
      )
  end

  let quicksort ~limit a = Quicksort_ref.sort ~limit ~cmp:CCInt.compare a

  let sort_std a = Array.sort CCInt.compare a

  (* helper, to apply a sort function over a list of arrays *)
  let app_list sort l =
    List.iter
      (fun a ->
        let a = Array.copy a in
        sort a
      ) l

  let () =
    List.iter
      (fun n ->
         let a1 = mk_arr n in
         let a2 = Array.copy a1 in
         sort_std a1;
         quicksort ~limit:10 a2;
         assert (CCArray.equal CCInt.equal a1 a2))
      [ 10; 100; 1000]

  let bench_sort ?(time=2) n =
    let a1 = mk_arr n in
    let a2 = mk_arr n in
    let a3 = mk_arr n in
    B.throughputN time ~repeat
      [ "std", app_list sort_std, [a1;a2;a3]
      ; "ccarray.sort_gen", app_list sort_ccarray, [a1;a2;a3]
      ; "ccarray.quicksort(limit=5)", app_list (quicksort ~limit:5), [a1;a2;a3]
      ; "ccarray.quicksort(limit=10)", app_list (quicksort ~limit:10), [a1;a2;a3]
      ; "ccarray.quicksort(limit=20)", app_list (quicksort ~limit:20), [a1;a2;a3]
      ]

  let () =
    B.Tree.register ("array" @>>>
      [ "sort" @>>
        app_ints (bench_sort ?time:None) [50; 100; 1000; 10_000; 50_000; 100_000; 500_000]
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
      [ "replacing_fib (128)", make_fib (C.replacing ~eq:CCInt.equal 128), n
      ; "LRU_fib (128)", make_fib (C.lru ~eq:CCInt.equal 128), n
      ; "replacing_fib (16)", make_fib (C.replacing ~eq:CCInt.equal 16), n
      ; "LRU_fib (16)", make_fib (C.lru ~eq:CCInt.equal 16), n
      ; "unbounded", make_fib (C.unbounded ~eq:CCInt.equal 32), n
      ]
    in
    let l = if n <= 20
      then  [ "linear_fib (5)", make_fib (C.linear ~eq:CCInt.equal 5), n
            ; "linear_fib (32)", make_fib (C.linear ~eq:CCInt.equal 32), n
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
    | Str -> (module CCString : KEY with type t = string), "string"

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

(*
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
   *)

  let modules_int =
    [ hashtbl_make Int
    ; hashtbl
    ; persistent_hashtbl Int
    (* ; poly_hashtbl *)
    ; map Int
    ; wbt Int
    ; hashtrie Int
    ; hashtrie_mut Int
      (* ; hamt Int *)
    ]

  let modules_string =
    [ hashtbl_make Str
    ; map Str
    ; wbt Str
    ; hashtrie Str
    ; persistent_hashtbl Str
    (* ; hamt Str *)
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

module Iter_ = struct
  (** {2 Iter/Gen} *)

  let bench_fold n =
    let iter () = Iter.fold (+) 0 Iter.(0 --n) in
    let gen () = Gen.fold (+) 0 Gen.(0 -- n) in
    let oseq () = OSeq.fold (+) 0 OSeq.(0 -- n) in
    B.throughputN 3 ~repeat
      [ "iter.fold", iter, ();
        "gen.fold", gen, ();
        "oseq.fold", oseq, ();
      ]

  let bench_flat_map n =
    let iter () = Iter.(
      0 -- n |> flat_map (fun x -> x-- (x+10)) |> fold (+) 0
    )
    and gen () = Gen.(
      0 -- n |> flat_map (fun x -> x-- (x+10)) |> fold (+) 0
    )
    and oseq () = OSeq.(
      0 -- n |> flat_map (fun x -> x-- (x+10)) |> fold (+) 0
    )
    in
    B.throughputN 3 ~repeat
      [ "iter.flat_map", iter, ();
        "gen.flat_map", gen, ();
        "oseq.flat_map", oseq, ();
      ]

  let bench_iter n =
    let iter () =
      let i = ref 2 in
      Iter.(
        1 -- n |> iter (fun x -> i := !i * x)
      )
    and gen () =
      let i = ref 2 in
      Gen.(
        1 -- n |> iter (fun x -> i := !i * x)
      )
    and oseq () =
      let i = ref 2 in
      OSeq.(
        1 -- n |> iter (fun x -> i := !i * x)
      )
    in
    B.throughputN 3 ~repeat
      [ "iter.iter", iter, ();
        "gen.iter", gen, ();
        "oseq.iter", oseq, ();
      ]

  let bench_to_array n =
    let iter () = Iter.to_array (Iter.(1 -- n))
    and gen () = Gen.to_array (Gen.(1 -- n))
    and oseq () = OSeq.to_array (OSeq.(1 -- n)) in
    B.throughputN 3 ~repeat
      [ "iter.to_array", iter, ();
        "gen.to_array", gen, ();
        "oseq.to_array", oseq, ();
      ]

  let bench_cons n =
    let gen_cons x xs =
      let saw_x = ref false in
      fun () ->
        if !saw_x then (saw_x := true; Some x)
        else xs ()
    in
    let xs = Array.init n CCFun.id in
    let iter () = ignore (Array.fold_right Iter.cons xs Iter.empty : int Iter.t) in
    let gen () = ignore (Array.fold_right gen_cons xs Gen.empty : int Gen.t) in
    let oseq () = ignore (Array.fold_right OSeq.cons xs OSeq.empty : int OSeq.t) in
    B.throughputN 3 ~repeat
      [ "iter.cons", iter, ();
        "gen.cons", gen, ();
        "oseq.cons", oseq, ();
      ]

  let bench_cons_fold n =
    let gen_cons x xs =
      let saw_x = ref false in
      fun () ->
        if !saw_x then (saw_x := true; Some x)
        else xs ()
    in
    let xs = Array.init n CCFun.id in
    let iter () = Iter.fold (+) 0 (Array.fold_right Iter.cons xs Iter.empty) in
    let gen () = Gen.fold (+) 0 (Array.fold_right gen_cons xs Gen.empty) in
    let oseq () = OSeq.fold (+) 0 (Array.fold_right OSeq.cons xs OSeq.empty) in
    B.throughputN 3 ~repeat
      [ "iter.cons_fold", iter, ();
        "gen.cons_fold", gen, ();
        "oseq.cons_fold", oseq, ();
      ]

  let () = B.Tree.register (
    "iter" @>>>
      [ "fold" @>> app_ints bench_fold [100; 1_000; 10_000; 1_000_000]
      ; "flat_map" @>> app_ints bench_flat_map [1_000; 10_000]
      ; "iter" @>> app_ints bench_iter [1_000; 10_000]
      ; "to_array" @>> app_ints bench_to_array [1_000; 10_000]
      ; "cons" @>> app_ints bench_cons [1_000; 10_000; 100_000]
      ; "cons_fold" @>> app_ints bench_cons_fold [1_000; 10_000; 100_000]
      ])
end

module Deque = struct
  module type DEQUE = sig
    type 'a t
    val create : unit -> 'a t
    val of_iter : 'a Iter.t -> 'a t
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
      | Some first when Stdlib.(==) first first.prev ->
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
      | Some first when Stdlib.(==) first first.prev ->
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

    let of_iter seq =
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
    let of_iter s = ref (CCFQueue.of_iter s)
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
    let seq = Iter.(1 -- n) in
    let make (module D : DEQUE) =
      let q = D.of_iter seq in
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
    let seq = Iter.(1 -- n) in
    let make (module D :DEQUE) =
      let q1 = D.of_iter seq in
      let q2 = D.of_iter seq in
      fun () -> D.append_back ~into:q1 q2
    in
    B.throughputN 3 ~repeat
      [ "base", make base, ()
      ; "cur", make cur, ()
      ; "fqueue", make fqueue, ()
      ]

  let bench_length n =
    let seq = Iter.(1--n) in
    let make (module D:DEQUE) =
      let q = D.of_iter seq in
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

  let div_graph_ = CCGraph.divisors_graph

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
      (Iter.return n)
    |> Iter.fold (fun acc _ -> acc+1) 0

  let dfs_event n () =
    let tbl = CCGraph.mk_table ~eq:CCInt.equal ~hash:CCInt.hash (n+10) in
    CCGraph.Traverse.Event.dfs ~tbl ~eq:CCInt.equal ~graph:div_graph_
      (Iter.return n)
    |> Iter.fold
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
        if CCString.is_sub ~sub 0 s !i ~sub_len:n then raise Exit;
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
        if CCString.is_sub ~sub 0 s !i ~sub_len:n then raise Exit;
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
    assert (CCList.equal CCInt.equal (mk_naive ()) (mk_current ()));
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
    assert (CCList.equal CCInt.equal (mk_naive ()) (mk_current ()));
    B.throughputN 3 ~repeat
      [ "naive", mk_naive, ()
      ; "current", mk_current, ()
      ]

  let bench_find  = bench_find_ ~dir:`Direct
  let bench_rfind  = bench_find_ ~dir:`Reverse

  module Pre = struct
    let prefix_rec ~pre s =
      let rec same s1 s2 i =
        if i = String.length s1 then true
        else (
          CCChar.equal (String.unsafe_get s1 i) (String.unsafe_get s2 i) && same s1 s2 (i+1)
        )
      in
      String.length pre <= String.length s &&
      same pre s 0

    let prefix_while ~pre s =
      String.length pre <= String.length s &&
      begin
        let i = ref 0 in
        while !i < String.length pre &&
              CCChar.equal (String.unsafe_get s !i) (String.unsafe_get pre !i)
        do incr i done;
        !i = String.length pre
      end

    exception Exit_false

    let prefix_for_exn ~pre s =
      String.length pre <= String.length s &&
      try
        for i=0 to String.length pre-1 do
          if String.unsafe_get s i != String.unsafe_get pre i
          then raise Exit_false
        done;
        true
      with Exit_false -> false

    let prefix_sub ~pre:prfx s =
      let len_s = String.length s in
      let len_p = String.length prfx in
      if len_s < len_p then
        false
      else
        let sub = String.sub s 0 len_p in
        CCString.equal prfx sub

    let bat_prefix ~pre:p str =
      let len = String.length p in
      if String.length str < len then false
      else
        let rec loop str p i =
          if i = len then true
          else if not (CCChar.equal (String.unsafe_get str i) (String.unsafe_get p i)) then false
          else loop str p (i + 1)
        in loop str p 0

    let make ~max_len ~max_len_prefix n =
      let rand = Random.State.make_self_init () in
      let input =
        Array.init n
          (fun _ ->
             let str =
               QCheck.Gen.(string_size ~gen:printable (10 -- max_len))
               |> QCheck.Gen.generate1 ~rand
             in
             let prfx_len = Random.State.int rand (min max_len_prefix (String.length str + 1)) in
             let prfx =
               if Random.State.bool rand then
                 String.sub str 0 prfx_len
               else
                 String.sub str (String.length str - prfx_len) prfx_len
             in
             (prfx, str))
      in
      let output =
        Array.map
          (fun (pre, str) -> prefix_rec ~pre str)
          input
      in
      let test f () =
        Array.iteri
          (fun i (pre, y) ->
             let res = f ~pre y in
             assert (CCBool.equal res output.(i)))
          input
      in
      Benchmark.throughputN 3
        [
          "containers", test CCString.prefix, ();
          "while_unsafe", test prefix_while, ();
          "rec_unsafe", test prefix_rec, ();
          "for_exn_unsafe", test prefix_for_exn, ();
          "sub_eq", test prefix_sub, ();
          "bat_prefix", test bat_prefix, ();
        ]
  end

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
        "prefix" @>>>
        [ "max_len:1000,max_pre_len:15" @>> app_ints (Pre.make ~max_len:1000 ~max_len_prefix:15) [100; 1_000];
          "max_len:1000,max_pre_len:100" @>> app_ints (Pre.make ~max_len:1000 ~max_len_prefix:100) [100; 1_000];
          "max_len:1000,max_pre_len:300" @>> app_ints (Pre.make ~max_len:1000 ~max_len_prefix:300) [100; 1_000];
        ]
      ])

end

let () =
  try B.Tree.run_global ()
  with Arg.Help msg -> print_endline msg
