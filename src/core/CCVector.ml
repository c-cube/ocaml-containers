(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Growable, mutable vector} *)

type rw = [`RW]
type ro = [`RO]

type 'a iter = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** A vector of 'a. *)
type ('a,'mut) t = {
  mutable size : int;
  mutable vec : 'a array;
}

type 'a vector = ('a, rw) t

type 'a ro_vector = ('a, ro) t

external as_float_arr : 'a array -> float array = "%identity"
external as_obj_arr : 'a array -> Obj.t array = "%identity"

let fill_with_junk_ (a:_ array) i len : unit =
  if Obj.(tag (repr a) = double_array_tag) then (
    Array.fill (as_float_arr a) i len 0.;
  ) else (
    Array.fill (as_obj_arr a) i len (Obj.repr ());
  )

let freeze v = {
  size=v.size;
  vec=v.vec;
}

let freeze_copy v = {
  size=v.size;
  vec=Array.sub v.vec 0 v.size;
}

let create () = {
  size = 0;
  vec = [| |];
}

let create_with ?(capacity=128) x =
  let vec = Array.make capacity x in
  fill_with_junk_ vec 0 capacity;
  {
  size = 0;
  vec
}

(*$T
  (create_with ~capacity:200 1 |> capacity) >= 200
*)

let return x = {
  size=1;
  vec= [| x |];
}

(*$T
  return 42 |> to_list = [42]
  return 42 |> length = 1
*)

let make n x = {
  size=n;
  vec=Array.make n x;
}

let init n f = {
  size=n;
  vec=Array.init n f;
}

(* is the underlying array empty? *)
let array_is_empty_ v =
  Array.length v.vec = 0

(* resize the underlying array using x to temporarily fill the array *)
let resize_ v newcapacity x =
  assert (newcapacity >= v.size);
  assert (not (array_is_empty_ v));
  let new_vec = Array.make newcapacity x in
  Array.blit v.vec 0 new_vec 0 v.size;
  fill_with_junk_ new_vec v.size (newcapacity-v.size);
  v.vec <- new_vec;
  ()

(*$T
  let v = create_with ~capacity:10 1 in \
    ensure v 200; capacity v >= 200
*)

(*$T
  let v = create() in push v 0.; push v 1.; push v 2.; 3=length v
  let v = create() in push v 1.; push v 2.; push v 3.; 6. = (get v 0 +. get v 1 +. get v 2)
  let v = create() in push v 0; push v 1; push v 2; 3=length v
  let v = create() in push v 1; push v 2; push v 3; 6 = (get v 0 + get v 1 + get v 2)
  let v = create() in push v "a"; push v "b"; push v "c"; 3=length v
  let v = create() in push v "a"; push v "b"; push v "c"; "abc" = String.concat "" (to_list v)
*)

(*$R
  let v = create() in
  push v 0.; push v 1.;
  clear v;
  push v 0.; push v 1.; push v 7.; push v 10.; push v 12.;
  shrink v 2;
  assert_equal 1. (fold (+.) 0. v);
  clear v;
  assert_equal 0 (size v);
  push v 0.; push v 1.; push v 7.; push v 10.; push v 12.;
  assert_equal (1. +. 7. +. 10. +. 12.) (fold (+.) 0. v);
  *)

(* grow the array, using [x] as a filler if required *)
let grow_with_ v ~filler:x =
  if array_is_empty_ v then (
    let len = 4 in
    v.vec <- Array.make len x;
    (* do not really use [x], it was just for knowing the type *)
    fill_with_junk_ v.vec 0 len;
  ) else (
    let n = Array.length v.vec in
    let size = min (2 * n + 3) Sys.max_array_length in
    if size = n then failwith "vec: can't grow any further";
    resize_ v size v.vec.(0)
  )

(* v is not empty; ensure it has at least [size] slots.

   Use a doubling-size strategy so that calling many times [ensure] will
   behave well *)
let ensure_assuming_not_empty_ v ~size =
  if size > Sys.max_array_length
  then failwith "vec.ensure: size too big"
  else (
    let n = ref (max 8 (Array.length v.vec)) in
    while !n < size do n := min Sys.max_array_length (2* !n) done;
    resize_ v !n v.vec.(0)
  )

let ensure_with ~init v size =
  if array_is_empty_ v then (
    v.vec <- Array.make size init;
    fill_with_junk_ v.vec 0 size
  ) else (
    ensure_assuming_not_empty_ v ~size
  )

let ensure v size =
  if not (array_is_empty_ v) then (
    ensure_assuming_not_empty_  v ~size
  )

let clear v =
  v.size <- 0

(*$R
  let v = of_iter Iter.(1 -- 10) in
  OUnit.assert_equal 10 (size v);
  clear v;
  OUnit.assert_equal 0 (size v);
  OUnit.assert_bool "empty_after_clear" (Iter.is_empty (to_iter v));
*)

let clear_and_reset v =
  v.size <- 0;
  v.vec <- [||]

(* TODO*)
(*
  let v = create() in
  let a = Weak.create 1 in
  push v ("hello"^"world");
  Weak.set a 0 (Some (get v 0));
  Gc.full_major(); Gc.compact();
  OUnit.assert_bool "is alive" (Weak.check a 0);
  Gc.full_major(); Gc.compact();
  OUnit.assert_equal None (Weak.get a 0);
*)

let is_empty v = v.size = 0

let push_unsafe_ v x =
  Array.unsafe_set v.vec v.size x;
  v.size <- v.size + 1

let push v x =
  if v.size = Array.length v.vec then grow_with_ v ~filler:x;
  push_unsafe_ v x

(*$T
  let v = create () in push v 1; to_list v = [1]
  let v = of_list [1;2;3] in push v 4; to_list v = [1;2;3;4]
*)

(** Add all elements of b to a *)
let append a b =
  if array_is_empty_ a then (
    if array_is_empty_ b then ()
    else (
      a.vec <- Array.copy b.vec;
      a.size <- b.size
    )
  ) else (
    ensure_assuming_not_empty_ a ~size:(a.size + b.size);
    assert (Array.length a.vec >= a.size + b.size);
    Array.blit b.vec 0 a.vec a.size b.size;
    a.size <- a.size + b.size
  )

(*$T
  let v1 = init 5 (fun i->i) and v2 = init 5 (fun i->i+5) in \
  append v1 v2; to_list v1 = CCList.(0--9)
  let empty = create () and v2 = init 5 (fun i->i) in \
  append empty v2; to_list empty = CCList.(0--4)
  let v1 = init 5 (fun i->i) and empty = create () in \
  append v1 empty; to_list v1 = CCList.(0--4)
  let v = init 3 (fun i->i) in \
  append v v; to_list v = [0; 1; 2; 0; 1; 2]
  let empty = create () in \
  append empty empty; to_list empty = []
*)

(*$R
  let a = of_iter Iter.(1 -- 5) in
  let b = of_iter Iter.(6 -- 10) in
  append a b;
  OUnit.assert_equal 10 (size a);
  OUnit.assert_equal (Iter.to_array Iter.(1 -- 10)) (to_array a);
  OUnit.assert_equal (Iter.to_array Iter.(6 -- 10)) (to_array b);
*)

let get v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.get";
  Array.unsafe_get v.vec i

let set v i x =
  if i < 0 || i >= v.size then invalid_arg "CCVector.set";
  Array.unsafe_set v.vec i x

let remove_and_shift v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.remove";
  (* if v.(i) not the last element, then put last element at index i *)
  if i < v.size - 1
  then Array.blit v.vec (i+1) v.vec i (v.size - i - 1);
  (* remove one element *)
  v.size <- v.size - 1;
  fill_with_junk_ v.vec v.size 1

let remove_unordered v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.remove_unordered";
  (* if v.(i) not the last element, then put last element at index i *)
  if i < v.size - 1
  then v.vec.(i) <- v.vec.(v.size - 1);
  (* remove one element *)
  v.size <- v.size - 1;
  fill_with_junk_ v.vec v.size 1

(*$Q remove_and_shift
  Q.(list_of_size (Gen.int_range 10 10) small_int) (fun l -> \
    let v1 = of_list l and v2 = of_list l in \
    remove_and_shift v1 9; \
    remove_unordered v2 9; \
    to_list v1 = (to_list v2))
  Q.(list_of_size (Gen.int_range 10 10) small_int) (fun l -> \
    let l = List.sort CCInt.compare l in \
    let v = of_list l in\
    remove_and_shift v 3; \
    to_list v = (List.sort CCInt.compare (to_list v)))
  Q.(list_of_size (Gen.int_range 10 10) small_int) (fun l -> \
    let l = List.sort CCInt.compare l in \
    let v1 = of_list l and v2 = of_list l in \
    remove_and_shift v1 3; \
    remove_unordered v2 3; \
    to_list v1 = (List.sort CCInt.compare (to_list v2)))
*)

let append_iter a i = i (fun x -> push a x)

let append_std_seq a seq = Seq.iter (fun x -> push a x) seq

let append_array a b =
  let len_b = Array.length b in
  if array_is_empty_ a then (
    a.vec <- Array.copy b;
    a.size <- len_b;
  ) else (
    ensure_assuming_not_empty_ a ~size:(a.size + len_b);
    Array.blit b 0 a.vec a.size len_b;
    a.size <- a.size + len_b
  )

(*$T
  let v1 = init 5 (fun i->i) and v2 = Array.init 5 (fun i->i+5) in \
  append_array v1 v2; to_list v1 = CCList.(0--9)
  let empty = create () in \
  append_array empty CCArray.(0--5); to_list empty = CCList.(0--5)
  let v1 = init 5 (fun i->i) in \
  append_array v1 [| |]; to_list v1 = CCList.(0--4)
  let empty = create () in \
  append_array empty [| |]; to_list empty = []
*)

let append_list a b = match b with
  | [] -> ()
  | x :: _ ->
    (* need to push at least one elem *)
    let len_a = a.size in
    let len_b = List.length b in
    ensure_with ~init:x a (len_a + len_b);
    List.iter (push_unsafe_ a) b;
    ()

(*$Q
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    let v = of_list l1 in append_list v l2; \
    to_list v = (l1 @ l2))
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    let v = of_list l1 in append_list v l2; \
    length v = List.length l1 + List.length l2)
*)

let rec append_gen a b = match b() with
  | None -> ()
  | Some x -> push a x; append_gen a b

(*$Q
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    let v = of_list l1 in append_gen v (Gen.of_list l2); \
    to_list v = (l1 @ l2))
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    let v = of_list l1 in append_gen v (Gen.of_list l2); \
    length v = List.length l1 + List.length l2)
*)


(*$inject
  let gen x =
    let small = length in
    let print = CCOpt.map (fun p x -> Q.Print.list p (CCVector.to_list x)) x.Q.print in
    Q.make ?print ~small Q.Gen.(list x.Q.gen >|= of_list)
*)

(*$QR
  (Q.pair (gen Q.int) (gen Q.int)) (fun (v1,v2) ->
    let l1 = to_list v1 in
    append v1 v2;
    Iter.to_list (to_iter v1) =
      Iter.(to_list (append (of_list l1) (to_iter v2)))
  )
*)

let equal eq v1 v2 =
  v1.size = v2.size
  &&
  let n = v1.size in
  let rec check i =
    i = n || (eq (get v1 i) (get v2 i) && check (i+1))
  in
  check 0

(*$T
  equal (=) (create ()) (create ())
  equal (=) (return 42) (return 42)
  not (equal (=) (create ()) (return 42))
  not (equal (=) (return 42) (create ()))
*)

(*$Q
  Q.(let g = list_of_size Gen.(0--10) small_int in pair g g) (fun (l1,l2) -> \
    equal (=) (of_list l1) (of_list l2) = (l1=l2))
*)

(*$QR
  Q.(pair (small_list small_int)(small_list small_int)) (fun (l1,l2) ->
    let v1 = of_list l1 in
    let v2 = of_list l2 in
    equal (=) v1 v2 = (l1=l2))
*)

let compare cmp v1 v2 =
  let n = min v1.size v2.size in
  let rec check i =
    if i = n
    then compare v1.size v2.size
    else (
      let c = cmp (get v1 i) (get v2 i) in
      if c = 0 then check (i+1) else c
    )
  in check 0

(*$QR
  Q.(pair (small_list small_int)(small_list small_int)) (fun (l1,l2) ->
    let v1 = of_list l1 in
    let v2 = of_list l2 in
    compare Stdlib.compare v1 v2 = CCList.compare Stdlib.compare l1 l2)
*)

exception Empty

let pop_exn v =
  if v.size = 0 then raise Empty;
  let new_size = v.size - 1 in
  v.size <- new_size;
  let x = v.vec.(new_size) in
  (* free last element *)
  fill_with_junk_ v.vec new_size 1;
  x

let pop v =
  try Some (pop_exn v)
  with Empty -> None

let top v =
  if v.size = 0 then None else Some v.vec.(v.size-1)

let top_exn v =
  if v.size = 0 then raise Empty;
  v.vec.(v.size-1)

(*$T
  1 -- 10 |> top = Some 10
  create () |> top = None
  1 -- 10 |> top_exn = 10
*)

let copy v = {
  size = v.size;
  vec = Array.sub v.vec 0 v.size;
}

(*$T
  (let v = of_list [1;2;3] in let v' = copy v in \
    to_list v' = [1;2;3])
  create () |> copy |> is_empty
*)

(*$R
  let v = of_iter Iter.(1 -- 100) in
  OUnit.assert_equal 100 (size v);
  let v' = copy v in
  OUnit.assert_equal 100 (size v');
  clear v';
  OUnit.assert_bool "empty" (is_empty v');
  OUnit.assert_bool "not_empty" (not (is_empty v));
*)

(*$QR
  Q.(small_list small_int) (fun l ->
    let v = of_list l in
    let v' = copy v in
    equal (=) v v')
*)

let shrink v n =
  let old_size = v.size in
  if n < old_size then (
    v.size <- n;
    (* free elements by erasing them *)
    fill_with_junk_ v.vec n (old_size-n);
  )

(*$R
  let v = of_iter Iter.(1 -- 10) in
  shrink v 5;
  OUnit.assert_equal [1;2;3;4;5] (to_list v);
*)

(*$QR
  (gen Q.small_int) (fun v ->
    let n = size v / 2 in
    let l = to_list v in
    let h = Iter.(to_list (take n (of_list l))) in
    let v' = copy v in
    shrink v' n;
    h = to_list v'
  )
*)

let shrink_to_fit v : unit =
  if v.size = 0 then (
    v.vec <- [| |]
  ) else if v.size < Array.length v.vec then (
    v.vec <- Array.sub v.vec 0 v.size
  )

(*$QR
  (gen Q.small_int) (fun v ->
    let v' = copy v in
    shrink_to_fit v;
    to_list v = to_list v'
  )
*)

let sort' cmp v =
  (* possibly copy array (to avoid junk at its end), then sort the array *)
  let a =
    if Array.length v.vec = v.size then v.vec
    else Array.sub v.vec 0 v.size
  in
  Array.fast_sort cmp a;
  v.vec <- a

let sort cmp v =
  let v' = {
    size=v.size;
    vec=Array.sub v.vec 0 v.size;
  } in
  Array.sort cmp v'.vec;
  v'

(*$QR
  (gen Q.small_int) (fun v ->
    let v' = copy v in
    sort' Stdlib.compare v';
    let l = to_list v' in
    List.sort Stdlib.compare l = l
  )
*)

let uniq_sort cmp v =
  sort' cmp v;
  let n = v.size in
  (* traverse to remove duplicates. i= current index,
     j=current append index, j<=i. new_size is the size
     the vector will have after removing duplicates. *)
  let rec traverse prev i j =
    if i >= n then () (* done traversing *)
    else if cmp prev v.vec.(i) = 0
    then (
      v.size <- v.size - 1;
      traverse prev (i+1) j
    ) (* duplicate, remove it *)
    else (
      v.vec.(j) <- v.vec.(i);
      traverse v.vec.(i) (i+1) (j+1)
    ) (* keep it *)
  in
  if v.size > 0
  then traverse v.vec.(0) 1 1
(* start at 1, to get the first element in hand *)

(*$T
  let v = of_list [1;4;5;3;2;4;1] in \
  uniq_sort Stdlib.compare v; to_list v = [1;2;3;4;5]
*)

(*$QR & ~long_factor:10
  Q.(small_list small_int) (fun l ->
    let v = of_list l in
    uniq_sort Stdlib.compare v;
    to_list v = (CCList.sort_uniq ~cmp:Stdlib.compare l))
*)

let iter k v =
  let n = v.size in
  for i = 0 to n-1 do
    k (Array.unsafe_get v.vec i)
  done

let iteri k v =
  let n = v.size in
  for i = 0 to n-1 do
    k i (Array.unsafe_get v.vec i)
  done

(*$T
  let v = (0--6) in \
    iteri (fun i x ->  if i = 3 then remove_unordered v i) v; length v = 6
*)

let map f v =
  if array_is_empty_ v
  then create ()
  else (
    let vec = Array.init v.size (fun i -> f (Array.unsafe_get v.vec i)) in
    { size=v.size; vec; }
  )

(*$T
  let v = create() in push v 1; push v 2; push v 3; \
  to_list (map string_of_int v) = ["1"; "2"; "3"]
*)

(*$QR
  Q.(pair (fun1 Observable.int small_int) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    to_list (map f v) = List.map f l)
*)

let mapi f v =
  if array_is_empty_ v
  then create ()
  else (
    let vec = Array.init v.size (fun i -> f i (Array.unsafe_get v.vec i)) in
    { size=v.size; vec; }
  )

(*$T mapi
  let v = create() in push v 1; push v 2; push v 3; \
  to_list (mapi (fun i e -> Printf.sprintf "%i %i" i e) v) = ["0 1"; "1 2"; "2 3"]
*)

(*$QR mapi
  Q.(pair (fun2 Observable.int Observable.int small_int) (small_list small_int))
    (fun (Q.Fun (_,f),l) ->
      let v = of_list l in
      to_list (mapi f v) = List.mapi f l)
*)

let map_in_place f v =
  iteri
    (fun i x -> Array.unsafe_set v.vec i (f x))
    v

(*$QR
  Q.(pair (fun1 Observable.int small_int) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    map_in_place f v;
    to_list v = List.map f l)
*)


let filter_in_place p v =
  let i = ref 0 in (* cur element *)
  let j = ref 0 in  (* cur insertion point *)
  let n = v.size in
  while !i < n do
    if p v.vec.(! i) then (
      (* move element i at the first empty slot.
         invariant: i >= j*)
      if !i > !j then v.vec.(!j) <- v.vec.(!i);
      incr i;
      incr j
    ) else incr i
  done;
  (* free elements *)
  fill_with_junk_ v.vec !j (v.size - !j);
  v.size <- !j

(*$T
  let v = 1 -- 10 in filter_in_place (fun x->x<4) v; \
    to_list v = [1;2;3]
*)

(*$QR
  Q.(pair (fun1 Observable.int bool) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    filter_in_place f v;
    to_list v = List.filter f l)
*)

let filter p v =
  if array_is_empty_ v then (
    create ()
  ) else (
    let v' = create_with ~capacity:v.size v.vec.(0) in
    iter
      (fun x -> if p x then push_unsafe_ v' x)
      v;
    v'
  )

(*$T
  filter (fun x-> x mod 2=0) (of_list [1;2;3;4;5]) |> to_list = [2;4]
  filter (fun x-> x mod 2=0) (1 -- 1_000_000) |> length = 500_000
*)

(*$QR
  Q.(pair (fun1 Observable.int bool) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    to_list (filter f v) = List.filter f l)
*)

let fold f acc v =
  let rec fold acc i =
    if i = v.size then acc
    else
      let x = Array.unsafe_get v.vec i in
      fold (f acc x) (i+1)
  in fold acc 0

(*$T
  fold (+) 0 (of_list [1;2;3;4;5]) = 15
  fold (+) 0 (create ()) = 0
*)

(*$QR
  Q.(pair (fun2 Observable.int Observable.int small_int) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    fold f 0 v = List.fold_left f 0 l)
*)

let exists p v =
  let n = v.size in
  let rec check i =
    if i = n then false
    else p v.vec.(i) || check (i+1)
  in check 0

(*$QR
  Q.(pair (fun1 Observable.int bool) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    exists f v = List.exists f l)
*)

let for_all p v =
  let n = v.size in
  let rec check i =
    if i = n then true
    else p v.vec.(i) && check (i+1)
  in check 0

(*$QR
  Q.(pair (fun1 Observable.int bool) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    for_all f v = List.for_all f l)
*)

let member ~eq x v =
  exists (eq x) v

let find_internal_ p v =
  let n = v.size in
  let rec check i =
    if i = n then raise_notrace Not_found
    else (
      let x = v.vec.(i) in
      if p x then x
      else check (i+1)
    )
  in check 0

let find_exn p v =
  try find_internal_ p v
  with Not_found ->
    raise Not_found

let find p v =
  try Some (find_internal_ p v)
  with Not_found -> None

(*$QR
  Q.(pair (fun1 Observable.int bool) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    find f v = CCList.find_pred f l)
*)

let find_map f v =
  let n = v.size in
  let rec search i =
    if i=n then None
    else match f v.vec.(i) with
      | None -> search (i+1)
      | Some _ as res -> res
  in
  search 0

(*$Q
  Q.(list small_int) (fun l -> \
    let v = of_list l in \
    let f x = x>30 && x < 35 in \
    find_map (fun x -> if f x then Some x else None) v = find f v)
*)

let filter_map f v =
  let v' = create () in
  iter
    (fun x -> match f x with
       | None -> ()
       | Some y -> push v' y)
    v;
  v'

(*$QR
  Q.(pair (fun1 Observable.int (option bool)) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    to_list (filter_map f v) = CCList.filter_map f l)
*)

let filter_map_in_place f v =
  let i = ref 0 in (* cur element *)
  let j = ref 0 in  (* cur insertion point *)
  let n = v.size in
  while !i < n do
    match f v.vec.(!i) with
      | None -> incr i (* drop *)
      | Some y ->
        (* move element i at the first empty slot.
           invariant: i >= j*)
        v.vec.(!j) <- y;
        incr i;
        incr j
  done;
  (* free elements *)
  fill_with_junk_ v.vec !j (v.size - !j);
  v.size <- !j

(*$QR
  Q.(pair (fun1 Observable.int (option small_int)) (small_list small_int)) (fun (Q.Fun (_,f),l) ->
    let v = of_list l in
    filter_map_in_place f v;
    to_list v = CCList.filter_map f l)
*)

(* check it frees memory properly *)
(*$R
  let s = "coucou" ^ "lol" in
  let w = Weak.create 1 in
  Weak.set w 0 (Some s);
  let v = of_list ["a"; s] in
  filter_in_place (fun s -> String.length s <= 1) v;
  assert_equal 1 (length v);
  assert_equal "a" (get v 0);
  Gc.major();
  assert_equal None (Weak.get w 0);
*)

let flat_map f v =
  let v' = create () in
  iter (fun x -> iter (push v') (f x)) v;
  v'

let flat_map_iter f v =
  let v' = create () in
  iter
    (fun x ->
       let seq = f x in
       append_iter v' seq)
    v;
  v'

let flat_map_std_seq f v =
  let v' = create () in
  iter
    (fun x ->
       let seq = f x in
       append_std_seq v' seq)
    v;
  v'

let flat_map_list f v =
  let v' = create () in
  iter
    (fun x ->
       let l = f x in
       append_list v' l)
    v;
  v'

let monoid_product f a1 a2 : _ t =
  let na1 = a1.size in
  init (na1 * a2.size)
    (fun i_prod ->
       let i = i_prod mod na1 in
       let j = i_prod / na1 in
       f a1.vec.(i) a2.vec.(j))

(*$= & ~cmp:(=) ~printer:Q.Print.(list int)
  [ 11; 12; 21; 22 ] (List.sort CCInt.compare @@ \
                      to_list @@ monoid_product (+) (of_list [10; 20]) (of_list [1; 2]))
  [ 11; 12; 13; 14 ] (List.sort CCInt.compare @@ \
                      to_list @@ monoid_product (+) (of_list [10]) (of_list [1; 2; 3; 4]))
*)

let (>>=) x f = flat_map f x

let (>|=) x f = map f x

let rev_in_place v =
  if v.size > 0
  then (
    let n = v.size in
    let vec = v.vec in
    for i = 0 to (n-1)/2 do
      let x = Array.unsafe_get vec i in
      let y = Array.unsafe_get vec (n-i-1) in
      Array.unsafe_set vec i y;
      Array.unsafe_set vec (n-i-1) x;
    done
  )

(*$QR
  Q.(small_list small_int) (fun l ->
    let v = of_list l in
    rev_in_place v;
    to_list v = List.rev l)
*)

let rev v =
  let v' = copy v in
  rev_in_place v';
  v'

(*$T
  rev (of_list [1;2;3;4]) |> to_list = [4;3;2;1]
  rev (of_list [1;2;3;4;5]) |> to_list = [5;4;3;2;1]
  rev (create ()) |> to_list = []
*)

(*$QR
  Q.(small_list small_int) (fun l ->
    let v = of_list l in
    to_list (rev v) = List.rev l)
*)

let rev_iter f v =
  let n = v.size in
  for i = n-1 downto 0 do
    f (Array.unsafe_get v.vec i)
  done

(*$T
  let v = of_list [1;2;3] in (fun f->rev_iter f v) |> Iter.to_list = [3;2;1]
*)

(*$Q
  Q.(list int) (fun l -> \
    let v = of_list l in \
    (fun f->rev_iter f v) |> Iter.to_list = List.rev l)
*)

let size v = v.size

let length v = v.size

let capacity v = Array.length v.vec

let unsafe_get_array v = v.vec

let of_iter ?(init=create ()) seq =
  append_iter init seq;
  init

let of_std_seq ?(init=create ()) seq =
  append_std_seq init seq;
  init

(*$T
  of_iter Iter.(1 -- 10) |> to_list = CCList.(1 -- 10)
*)

let to_iter v k = iter k v

let to_iter_rev v k =
  let n = v.size in
  for i = n - 1 downto 0 do
    k (Array.unsafe_get v.vec i)
  done

let to_std_seq v =
  let rec aux i () =
    if i>= size v then Seq.Nil
    else Seq.Cons (v.vec.(i), aux (i+1))
  in
  aux 0

let to_std_seq_rev v =
  let rec aux i () =
    if i<0 || i > size v then Seq.Nil
    else Seq.Cons (v.vec.(i), aux (i-1))
  in
  aux (size v-1)

(*$Q
  Q.(list int) (fun l -> \
    let v= of_list l in v |> to_iter_rev |> Iter.to_rev_list = l)
*)

let slice_iter v start len =
  assert (start >= 0 && len >= 0);
  fun k ->
    assert (start+len <= v.size);
    for i = start to start+len-1 do
      let x = Array.unsafe_get v.vec i in
      k x
    done

(*$T
  slice_iter (of_list [0;1;2;3;4]) 1 3 |> CCList.of_iter = [1;2;3]
  slice_iter (of_list [0;1;2;3;4]) 1 4 |> CCList.of_iter = [1;2;3;4]
  slice_iter (of_list [0;1;2;3;4]) 0 5 |> CCList.of_iter = [0;1;2;3;4]
*)

let slice v = (v.vec, 0, v.size)

let (--) i j =
  if i>j
  then init (i-j+1) (fun k -> i-k)
  else init (j-i+1) (fun k -> i+k)

(*$T
  (1 -- 4) |> to_list = [1;2;3;4]
  (4 -- 1) |> to_list = [4;3;2;1]
  (0 -- 0) |> to_list = [0]
*)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    (a -- b) |> to_list = CCList.(a -- b))
*)

let (--^) i j =
  if i=j then create()
  else if i>j
  then init (i-j) (fun k -> i-k)
  else init (j-i) (fun k -> i+k)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    (a --^ b) |> to_list = CCList.(a --^ b))
*)

let of_array a =
  if Array.length a = 0
  then create ()
  else {
    size=Array.length a;
    vec=Array.copy a;
  }

let of_list l = match l with
  | [] -> create()
  | [x] -> return x
  | [x;y] -> {size=2; vec=[| x; y |]}
  | x::_ ->
    let v = create_with ~capacity:(List.length l) x in
    List.iter (push_unsafe_ v) l;
    v

(*$T
  of_list CCList.(1--300_000) |> to_list = CCList.(1--300_000)
*)

let to_array v =
  Array.sub v.vec 0 v.size

let to_list v =
  List.rev (fold (fun acc x -> x::acc) [] v)

let of_gen ?(init=create ()) g =
  let rec aux g = match g() with
    | None -> init
    | Some x -> push init x; aux g
  in aux g

let to_gen v =
  let i = ref 0 in
  fun () ->
    if !i < v.size
    then (
      let x = v.vec.( !i ) in
      incr i;
      Some x
    ) else None

(*$T
  let v = (1--10) in to_list v = Gen.to_list (to_gen v)
*)

let of_klist ?(init=create ()) l =
  let rec aux l = match l() with
    | `Nil -> init
    | `Cons (x,l') -> push init x; aux l'
  in aux l

let to_klist v =
  let rec aux i () =
    if i=v.size then `Nil
    else `Cons (v.vec.(i), aux (i+1))
  in aux 0

let to_string ?(start="") ?(stop="") ?(sep=", ") item_to_string v =
  start ^ (to_list v |> List.map item_to_string |> String.concat sep) ^ stop

let pp ?(start="") ?(stop="") ?(sep=", ") pp_item fmt v =
  Format.pp_print_string fmt start;
  iteri
    (fun i x ->
       if i > 0 then (Format.pp_print_string fmt sep; Format.pp_print_cut fmt());
       pp_item fmt x
    ) v;
  Format.pp_print_string fmt stop

include CCShimsMkLet_.Make2(struct
    type nonrec ('a,'e) t = ('a,'e) t
    let (>|=) = (>|=)
    let (>>=) = (>>=)
    let monoid_product a1 a2 = monoid_product (fun x y->x,y) a1 a2
  end)
