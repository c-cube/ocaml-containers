
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Caches} *)

type 'a equal = 'a -> 'a -> bool
type 'a hash = 'a -> int

let default_hash_ = Hashtbl.hash

(** {2 Value interface} *)

(** Invariants:
    - after [cache.set x y], [get cache x] must return [y] or raise [Not_found]
    - [cache.set x y] is only called if [get cache x] fails, never if [x] is already bound
    - [cache.size()] must be positive and correspond to the number of items in [cache.iter]
    - [cache.iter f] calls [f x y] with every [x] such that [cache.get x = y]
    - after [cache.clear()], [cache.get x] fails for every [x]
*)
type ('a,'b) t = {
  set : 'a -> 'b -> unit;
  get : 'a -> 'b;  (* or raise Not_found *)
  size : unit -> int;
  iter : ('a -> 'b -> unit) -> unit;
  clear : unit -> unit;
}

type ('a, 'b) callback = in_cache:bool -> 'a -> 'b -> unit

let clear c = c.clear ()

let add c x y =
  try
    (* check that x is not bound (see invariants) *)
    let _ = c.get x in
    false
  with Not_found ->
    c.set x y;
    true

let default_callback_ ~in_cache:_ _ _ = ()

let with_cache ?(cb=default_callback_) c f x =
  try
    let y = c.get x in
    cb ~in_cache:true x y;
    y
  with Not_found ->
    let y = f x in
    c.set x y;
    cb ~in_cache:false x y;
    y

let with_cache_rec ?(cb=default_callback_) c f =
  let rec f' x = with_cache ~cb c (f f') x in
  f'

(*$R
  let c = unbounded ~eq:CCInt.equal 256 in
  let fib = with_cache_rec c
    (fun self n -> match n with
      | 1 | 2 -> 1
      | _ -> self (n-1) + self (n-2)
    )
  in
  assert_equal 55 (fib 10);
  assert_equal 832040 (fib 30);
  assert_equal 12586269025 (fib 50);
  assert_equal 190392490709135 (fib 70)
*)

let size c = c.size ()

let iter c f = c.iter f

let dummy = {
  set=(fun _ _ -> ());
  get=(fun _ -> raise Not_found);
  clear=(fun _ -> ());
  size=(fun _ -> 0);
  iter=(fun _ -> ());
}

module Linear = struct
  type ('a,'b) bucket =
    | Empty
    | Pair of 'a * 'b

  type ('a,'b) t = {
    eq : 'a equal;
    arr : ('a,'b) bucket array;
    mutable i : int;  (* index for next assertion, cycles through *)
  }

  let make eq size =
    assert (size>0);
    {arr=Array.make size Empty; eq; i=0; }

  let clear c =
    Array.fill c.arr 0 (Array.length c.arr) Empty;
    c.i <- 0

  (* linear lookup *)
  let rec search_ c i x =
    if i=Array.length c.arr then raise Not_found;
    match c.arr.(i) with
      | Pair (x', y) when c.eq x x' -> y
      | Pair _
      | Empty -> search_ c (i+1) x

  let get c x = search_ c 0 x

  let set c x y =
    c.arr.(c.i) <- Pair (x,y);
    c.i <- (c.i + 1) mod Array.length c.arr

  let iter c f =
    Array.iter (function Pair (x,y) -> f x y | Empty -> ()) c.arr

  let size c () =
    let r = ref 0 in
    iter c (fun _ _ -> incr r);
    !r
end

let linear ~eq size =
  let size = max size 1 in
  let arr = Linear.make eq size in
  { get=(fun x -> Linear.get arr x);
    set=(fun x y -> Linear.set arr x y);
    clear=(fun () -> Linear.clear arr);
    size=Linear.size arr;
    iter=Linear.iter arr;
  }

module Replacing = struct
  type ('a,'b) bucket =
    | Empty
    | Pair of 'a * 'b

  type ('a,'b) t = {
    eq : 'a equal;
    hash : 'a hash;
    arr : ('a,'b) bucket array;
    mutable c_size : int;
  }

  let make eq hash size =
    assert (size>0);
    {arr=Array.make size Empty; eq; hash; c_size=0 }

  let clear c =
    c.c_size <- 0;
    Array.fill c.arr 0 (Array.length c.arr) Empty

  let get c x =
    let i = c.hash x mod Array.length c.arr in
    match c.arr.(i) with
      | Pair (x', y) when c.eq x x' -> y
      | Pair _
      | Empty -> raise Not_found

  let is_empty = function
    | Empty -> true
    | Pair _ -> false

  let set c x y =
    let i = c.hash x mod Array.length c.arr in
    if is_empty c.arr.(i) then c.c_size <- c.c_size + 1;
    c.arr.(i) <- Pair (x,y)

  let iter c f =
    Array.iter (function Empty -> () | Pair (x,y) -> f x y) c.arr

  let size c () = c.c_size
end

let replacing ~eq ?(hash=default_hash_) size =
  let c = Replacing.make eq hash size in
  { get=(fun x -> Replacing.get c x);
    set=(fun x y -> Replacing.set c x y);
    clear=(fun () -> Replacing.clear c);
    size=Replacing.size c;
    iter=Replacing.iter c;
  }

module type HASH = sig
  type t
  val equal : t equal
  val hash : t hash
end

module LRU(X:HASH) = struct
  type key = X.t

  module H = Hashtbl.Make(X)

  type 'a t = {
    table : 'a node H.t;  (* hashtable key -> node *)
    mutable first : 'a node option;
    size : int;           (* max size *)
  }
  and 'a node = {
    mutable key : key;
    mutable value : 'a;
    mutable next : 'a node;
    mutable prev : 'a node;
  } (** Meta data for the value, making a chained list *)

  let make size =
    assert (size > 0);
    { table = H.create size;
      size;
      first=None;
    }

  let clear c =
    H.clear c.table;
    c.first <- None;
    ()

  (* take first from queue *)
  let take_ c =
    match c.first with
      | Some n when Stdlib.(==) n.next n ->
        (* last element *)
        c.first <- None;
        n
      | Some n ->
        c.first <- Some n.next;
        n.prev.next <- n.next;
        n.next.prev <- n.prev;
        n
      | None ->
        failwith "LRU: empty queue"

  (* push at back of queue *)
  let push_ c n =
    match c.first with
      | None ->
        n.next <- n;
        n.prev <- n;
        c.first <- Some n
      | Some n1 when Stdlib.(==) n1 n -> ()
      | Some n1 ->
        n.prev <- n1.prev;
        n.next <- n1;
        n1.prev.next <- n;
        n1.prev <- n

  (* remove from queue *)
  let remove_ n =
    n.prev.next <- n.next;
    n.next.prev <- n.prev

  (* Replace least recently used element of [c] by x->y *)
  let replace_ c x y =
    (* remove old *)
    let n = take_ c in
    H.remove c.table n.key;
    (* add x->y, at the back of the queue *)
    n.key <- x;
    n.value <- y;
    H.add c.table x n;
    push_ c n;
    ()

  (* Insert x->y in the cache, increasing its entry count *)
  let insert_ c x y =
    let rec n = {
      key = x;
      value = y;
      next = n;
      prev = n;
    } in
    H.add c.table x n;
    push_ c n;
    ()

  let get c x =
    let n = H.find c.table x in
    (* put n at the back of the queue *)
    remove_ n;
    push_ c n;
    n.value

  let set c x y =
    let len = H.length c.table in
    assert (len <= c.size);
    if len = c.size
    then replace_ c x y
    else insert_ c x y

  let size c () = H.length c.table

  let iter c f =
    H.iter (fun x node -> f x node.value) c.table
end

let lru (type a) ~eq ?(hash=default_hash_) size =
  let module L = LRU(struct
      type t = a
      let equal = eq
      let hash = hash
    end) in
  let c = L.make size in
  { get=(fun x -> L.get c x);
    set=(fun x y -> L.set c x y);
    clear=(fun () -> L.clear c);
    size=L.size c;
    iter=L.iter c;
  }

(*$T
  let eq (i1,_)(i2,_) = i1=i2 and hash (i,_) = CCInt.hash i in \
  let c = lru ~eq ~hash 2 in \
  ignore (with_cache c CCFun.id (1, true)); \
  ignore (with_cache c CCFun.id (1, false)); \
  with_cache c CCFun.id (1, false) = (1, true)
*)

(*$T
  let f = (let r = ref 0 in fun _ -> incr r; !r) in \
  let c = lru ~eq:CCInt.equal 2 in \
  let res1 = with_cache c f 1 in \
  let res2 = with_cache c f 2 in \
  let res3 = with_cache c f 3 in \
  let res1_bis = with_cache c f 1 in \
  res1 <> res2 && res2 <> res3 && res3 <> res1_bis && res1_bis <> res1
*)

(*$R
  let f = (let r = ref 0 in fun _ -> incr r; !r) in
  let c = lru ~eq:CCEqual.unit 2 in
  let x = with_cache c f () in
  assert_equal 1 x;
  assert_equal 1 (size c);
  clear c ;
  assert_equal 0 (size c);
  let y = with_cache c f () in
  assert_equal 2 y ;
*)

module UNBOUNDED(X:HASH) = struct
  module H = Hashtbl.Make(X)

  let make size =
    assert (size > 0);
    H.create size

  let clear c = H.clear c

  let get c x = H.find c x

  let set c x y = H.replace c x y

  let size c () = H.length c

  let iter c f = H.iter f c
end

let unbounded (type a) ~eq ?(hash=default_hash_) size =
  let module C = UNBOUNDED(struct
      type t = a
      let equal = eq
      let hash = hash
    end) in
  let c = C.make size in
  { get=(fun x -> C.get c x);
    set=(fun x y -> C.set c x y);
    clear=(fun () -> C.clear c);
    iter=C.iter c;
    size=C.size c;
  }
