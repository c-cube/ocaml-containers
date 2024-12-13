(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Growable, mutable vector} *)

type rw = [ `RW ]
type ro = [ `RO ]
type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

type ('a, 'mut) t = {
  mutable size: int;
  mutable vec: 'a array;
}
(** A vector of 'a. *)

type 'a vector = ('a, rw) t
type 'a ro_vector = ('a, ro) t

external as_float_arr : 'a array -> float array = "%identity"
external as_obj_arr : 'a array -> Obj.t array = "%identity"

let fill_with_junk_ (a : _ array) i len : unit =
  if Obj.(tag (repr a) = double_array_tag) then
    Array.fill (as_float_arr a) i len 0.
  else
    Array.fill (as_obj_arr a) i len (Obj.repr ())

let freeze v = { size = v.size; vec = v.vec }
let freeze_copy v = { size = v.size; vec = Array.sub v.vec 0 v.size }
let create () = { size = 0; vec = [||] }

let create_with ?(capacity = 128) x =
  let vec = Array.make capacity x in
  fill_with_junk_ vec 0 capacity;
  { size = 0; vec }

let return x = { size = 1; vec = [| x |] }
let make n x = { size = n; vec = Array.make n x }
let init n f = { size = n; vec = Array.init n f }

(* is the underlying array empty? *)
let[@inline] array_is_empty_ v = Array.length v.vec = 0

(* next capacity, if current one is [n] *)
let[@inline] next_grow_ n = min Sys.max_array_length (n + (n lsr 1) + 2)

(* resize the underlying array using x to temporarily fill the array *)
let resize_ v newcapacity x =
  assert (newcapacity >= v.size);
  assert (not (array_is_empty_ v));
  let new_vec = Array.make newcapacity x in
  Array.blit v.vec 0 new_vec 0 v.size;
  fill_with_junk_ new_vec v.size (newcapacity - v.size);
  v.vec <- new_vec;
  ()

(* grow the array, using [x] as a filler if required *)
let grow_with_ v ~filler:x =
  if array_is_empty_ v then (
    let len = 4 in
    v.vec <- Array.make len x;
    (* do not really use [x], it was just for knowing the type *)
    fill_with_junk_ v.vec 0 len
  ) else (
    let n = Array.length v.vec in
    let size = next_grow_ n in
    if size = n then invalid_arg "vec: can't grow any further";
    resize_ v size v.vec.(0)
  )

(* v is not empty; ensure it has at least [size] slots.

   Use a doubling-size strategy so that calling many times [ensure] will
   behave well *)
let ensure_assuming_not_empty_ v ~size =
  if size > Sys.max_array_length then
    invalid_arg "vec.ensure: size too big"
  else if size < Array.length v.vec then
    ()
  (* nothing to do *)
  else (
    let n = ref (Array.length v.vec) in
    while !n < size do
      n := next_grow_ !n
    done;
    resize_ v !n v.vec.(0)
  )

let ensure_with ~init v size =
  if array_is_empty_ v then (
    v.vec <- Array.make size init;
    fill_with_junk_ v.vec 0 size
  ) else
    ensure_assuming_not_empty_ v ~size

let ensure v size =
  if not (array_is_empty_ v) then ensure_assuming_not_empty_ v ~size

let[@inline] clear v = v.size <- 0

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
  assert_bool "is alive" (Weak.check a 0);
  Gc.full_major(); Gc.compact();
  assert_equal None (Weak.get a 0);
*)

let[@inline] is_empty v = v.size = 0

[@@@ifge 4.13]

let[@inline] push_unsafe_ v x =
  Sys.opaque_identity (Array.unsafe_set v.vec v.size x);
  v.size <- v.size + 1

[@@@else_]

let[@inline never] push_unsafe_ v x =
  Array.unsafe_set v.vec v.size x;
  v.size <- v.size + 1

[@@@endif]

let push v x =
  if v.size = Array.length v.vec then grow_with_ v ~filler:x;
  push_unsafe_ v x

let resize_with v f size =
  if size < 0 then invalid_arg "Vec.resize_with";
  if Array.length v.vec = 0 then (
    let new_vec = Array.init size f in
    v.vec <- new_vec;
    v.size <- size
  ) else (
    ensure_assuming_not_empty_ v ~size;
    let { size = cur_size; vec } = v in
    for i = cur_size to size - 1 do
      Array.unsafe_set vec i (f i)
    done;
    assert (size <= Array.length v.vec);
    v.size <- size
  )

let resize_with_init v ~init size =
  if size < 0 then invalid_arg "Vec.resize_with_init";
  if Array.length v.vec = 0 then (
    let vec = Array.make size init in
    v.vec <- vec;
    v.size <- size
  ) else (
    ensure_assuming_not_empty_ v ~size;
    (* nothing will change [v] *)
    for i = v.size to size - 1 do
      Array.unsafe_set v.vec i init
    done;
    v.size <- size
  )

(** Add all elements of b to a *)
let append a b =
  if array_is_empty_ a then
    if array_is_empty_ b then
      ()
    else (
      a.vec <- Array.copy b.vec;
      a.size <- b.size
    )
  else (
    ensure_assuming_not_empty_ a ~size:(a.size + b.size);
    assert (Array.length a.vec >= a.size + b.size);
    Array.blit b.vec 0 a.vec a.size b.size;
    a.size <- a.size + b.size
  )

[@@@ifge 4.13]

let[@inline] get v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.get";
  (* NOTE: over eager inlining seems to miscompile for int32 at least (#454) *)
  Sys.opaque_identity (Array.unsafe_get v.vec i)

let[@inline] set v i x =
  if i < 0 || i >= v.size then invalid_arg "CCVector.set";
  Array.unsafe_set v.vec i x

[@@@else_]

let[@inline never] get v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.get";
  Array.unsafe_get v.vec i

let[@inline never] set v i x =
  if i < 0 || i >= v.size then invalid_arg "CCVector.set";
  Array.unsafe_set v.vec i x

[@@@endif]

let remove_and_shift v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.remove";
  (* if v.(i) not the last element, then put last element at index i *)
  if i < v.size - 1 then Array.blit v.vec (i + 1) v.vec i (v.size - i - 1);
  (* remove one element *)
  v.size <- v.size - 1;
  fill_with_junk_ v.vec v.size 1

let remove_unordered v i =
  if i < 0 || i >= v.size then invalid_arg "CCVector.remove_unordered";
  (* if v.(i) not the last element, then put last element at index i *)
  if i < v.size - 1 then v.vec.(i) <- v.vec.(v.size - 1);
  (* remove one element *)
  v.size <- v.size - 1;
  fill_with_junk_ v.vec v.size 1

let insert v i x =
  (* Note that we can insert at i=v.size *)
  if i < 0 || i > v.size then invalid_arg "CCVector.insert";
  if v.size = Array.length v.vec then grow_with_ v ~filler:x;
  (* Shift the following elements, then put the element at i *)
  if i < v.size then Array.blit v.vec i v.vec (i + 1) (v.size - i);
  v.vec.(i) <- x;
  v.size <- v.size + 1

let[@inline] append_iter a i = i (fun x -> push a x)
let append_seq a seq = Seq.iter (fun x -> push a x) seq

let append_array a b =
  let len_b = Array.length b in
  if array_is_empty_ a then (
    a.vec <- Array.copy b;
    a.size <- len_b
  ) else (
    ensure_assuming_not_empty_ a ~size:(a.size + len_b);
    Array.blit b 0 a.vec a.size len_b;
    a.size <- a.size + len_b
  )

let append_list a b =
  match b with
  | [] -> ()
  | x :: _ ->
    (* need to push at least one elem *)
    let len_a = a.size in
    let len_b = List.length b in
    ensure_with ~init:x a (len_a + len_b);
    List.iter (push_unsafe_ a) b;
    ()

let rec append_gen a b =
  match b () with
  | None -> ()
  | Some x ->
    push a x;
    append_gen a b

let equal eq v1 v2 =
  v1.size = v2.size
  &&
  let n = v1.size in
  let rec check i = i = n || (eq (get v1 i) (get v2 i) && check (i + 1)) in
  check 0

let compare cmp v1 v2 =
  let n = min v1.size v2.size in
  let rec check i =
    if i = n then
      compare v1.size v2.size
    else (
      let c = cmp (get v1 i) (get v2 i) in
      if c = 0 then
        check (i + 1)
      else
        c
    )
  in
  check 0

exception Empty

let pop_exn v =
  if v.size = 0 then raise Empty;
  let new_size = v.size - 1 in
  v.size <- new_size;
  let x = v.vec.(new_size) in
  (* free last element *)
  fill_with_junk_ v.vec new_size 1;
  x

let pop v = try Some (pop_exn v) with Empty -> None

let[@inline] top v =
  if v.size = 0 then
    None
  else
    Some (Array.unsafe_get v.vec (v.size - 1))

let[@inline] top_exn v =
  if v.size = 0 then raise Empty;
  (* NOTE: over eager inlining seems to miscompile for int32 at least (#454) *)
  Sys.opaque_identity (Array.unsafe_get v.vec (v.size - 1))

let[@inline] copy v = { size = v.size; vec = Array.sub v.vec 0 v.size }

let truncate v n =
  let old_size = v.size in
  if n < old_size then (
    v.size <- n;
    (* free elements by erasing them *)
    fill_with_junk_ v.vec n (old_size - n)
  )

let shrink_to_fit v : unit =
  if v.size = 0 then
    v.vec <- [||]
  else if v.size < Array.length v.vec then
    v.vec <- Array.sub v.vec 0 v.size

let sort' cmp v =
  (* possibly copy array (to avoid junk at its end), then sort the array *)
  let a =
    if Array.length v.vec = v.size then
      v.vec
    else
      Array.sub v.vec 0 v.size
  in
  Array.fast_sort cmp a;
  v.vec <- a

let sort cmp v =
  let v' = { size = v.size; vec = Array.sub v.vec 0 v.size } in
  Array.sort cmp v'.vec;
  v'

let uniq_sort cmp v =
  sort' cmp v;
  let n = v.size in
  (* traverse to remove duplicates. i= current index,
     j=current append index, j<=i. new_size is the size
     the vector will have after removing duplicates. *)
  let rec traverse prev i j =
    if i >= n then
      ()
    (* done traversing *)
    else if cmp prev v.vec.(i) = 0 then (
      v.size <- v.size - 1;
      traverse prev (i + 1) j (* duplicate, remove it *)
    ) else (
      v.vec.(j) <- v.vec.(i);
      traverse v.vec.(i) (i + 1) (j + 1)
    )
    (* keep it *)
  in
  if v.size > 0 then traverse v.vec.(0) 1 1
(* start at 1, to get the first element in hand *)

let iter k v =
  let { vec; size = n } = v in
  for i = 0 to n - 1 do
    k (Array.unsafe_get vec i)
  done

let iteri k v =
  let { vec; size = n } = v in
  for i = 0 to n - 1 do
    k i (Array.unsafe_get vec i)
  done

let map f v =
  if array_is_empty_ v then
    create ()
  else (
    let { vec; size } = v in
    let vec = Array.init size (fun i -> f (Array.unsafe_get vec i)) in
    { size; vec }
  )

let mapi f v =
  if array_is_empty_ v then
    create ()
  else (
    let { vec; size } = v in
    let vec = Array.init size (fun i -> f i (Array.unsafe_get vec i)) in
    { size; vec }
  )

let map_in_place f v =
  let { vec; size = n } = v in
  for i = 0 to n - 1 do
    Array.unsafe_set vec i (f (Array.unsafe_get vec i))
  done

let filter_in_place p v =
  let i = ref 0 in
  (* cur element *)
  let j = ref 0 in
  (* cur insertion point *)
  let n = v.size in
  while !i < n do
    if p v.vec.(!i) then (
      (* move element i at the first empty slot.
         invariant: i >= j*)
      if !i > !j then v.vec.(!j) <- v.vec.(!i);
      incr i;
      incr j
    ) else
      incr i
  done;
  (* free elements *)
  fill_with_junk_ v.vec !j (v.size - !j);
  v.size <- !j

let filter p v =
  if array_is_empty_ v then
    create ()
  else (
    let v' = create_with ~capacity:v.size v.vec.(0) in
    iter (fun x -> if p x then push_unsafe_ v' x) v;
    v'
  )

let fold f acc v =
  let { vec; size } = v in
  let rec fold acc i =
    if i = size then
      acc
    else (
      let x = Array.unsafe_get vec i in
      fold (f acc x) (i + 1)
    )
  in
  fold acc 0

let foldi f acc v =
  let { vec; size } = v in
  let rec fold acc i =
    if i = size then
      acc
    else (
      let x = Array.unsafe_get vec i in
      fold (f i acc x) (i + 1)
    )
  in
  fold acc 0

let exists p v =
  let n = v.size in
  let rec check i =
    if i = n then
      false
    else
      p v.vec.(i) || check (i + 1)
  in
  check 0

let for_all p v =
  let n = v.size in
  let rec check i =
    if i = n then
      true
    else
      p v.vec.(i) && check (i + 1)
  in
  check 0

let member ~eq x v = exists (eq x) v

let find_internal_i_ p v =
  let n = v.size in
  let rec check i =
    if i = n then
      raise_notrace Not_found
    else (
      let x = v.vec.(i) in
      if p x then
        i, x
      else
        check (i + 1)
    )
  in
  check 0

let find_exn p v =
  try snd (find_internal_i_ p v) with Not_found -> raise Not_found

let find p v = try Some (snd @@ find_internal_i_ p v) with Not_found -> None
let findi p v = try Some (find_internal_i_ p v) with Not_found -> None

let find_map f v =
  let n = v.size in
  let rec search i =
    if i = n then
      None
    else (
      match f v.vec.(i) with
      | None -> search (i + 1)
      | Some _ as res -> res
    )
  in
  search 0

let filter_map f v =
  let v' = create () in
  iter
    (fun x ->
      match f x with
      | None -> ()
      | Some y -> push v' y)
    v;
  v'

let filter_map_in_place f v =
  let i = ref 0 in
  (* cur element *)
  let j = ref 0 in
  (* cur insertion point *)
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

let flat_map f v =
  let v' = create () in
  iter (fun x -> iter (push v') (f x)) v;
  v'

let flat_map_seq f v =
  let v' = create () in
  iter
    (fun x ->
      let seq = f x in
      append_seq v' seq)
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
  init (na1 * a2.size) (fun i_prod ->
      let i = i_prod mod na1 in
      let j = i_prod / na1 in
      f a1.vec.(i) a2.vec.(j))

let ( >>= ) x f = flat_map f x
let ( >|= ) x f = map f x

let rev_in_place v =
  if v.size > 0 then (
    let n = v.size in
    let vec = v.vec in
    for i = 0 to (n - 1) / 2 do
      let x = Array.unsafe_get vec i in
      let y = Array.unsafe_get vec (n - i - 1) in
      Array.unsafe_set vec i y;
      Array.unsafe_set vec (n - i - 1) x
    done
  )

let rev v =
  let v' = copy v in
  rev_in_place v';
  v'

let rev_iter f v =
  let { vec; size = n } = v in
  for i = n - 1 downto 0 do
    f (Array.unsafe_get vec i)
  done

let size v = v.size
let length v = v.size
let capacity v = Array.length v.vec
let unsafe_get_array v = v.vec

let of_iter ?(init = create ()) seq =
  append_iter init seq;
  init

let of_seq ?(init = create ()) seq =
  append_seq init seq;
  init

let to_iter v k = iter k v

let to_iter_rev v k =
  let { vec; size = n } = v in
  for i = n - 1 downto 0 do
    k (Array.unsafe_get vec i)
  done

let to_seq v =
  let { size; vec } = v in
  let rec aux i () =
    if i >= size then
      Seq.Nil
    else
      Seq.Cons (vec.(i), aux (i + 1))
  in
  aux 0

let to_seq_rev v =
  let { size; vec } = v in
  let rec aux i () =
    if i < 0 then
      Seq.Nil
    else
      Seq.Cons (vec.(i), aux (i - 1))
  in
  aux (size - 1)

let slice_iter v start len =
  assert (start >= 0 && len >= 0);
  fun k ->
    let { size; vec } = v in
    assert (start + len <= size);
    for i = start to start + len - 1 do
      let x = Array.unsafe_get vec i in
      k x
    done

let slice v = v.vec, 0, v.size

let ( -- ) i j =
  if i > j then
    init (i - j + 1) (fun k -> i - k)
  else
    init (j - i + 1) (fun k -> i + k)

let ( --^ ) i j =
  if i = j then
    create ()
  else if i > j then
    init (i - j) (fun k -> i - k)
  else
    init (j - i) (fun k -> i + k)

let of_array a =
  if Array.length a = 0 then
    create ()
  else
    { size = Array.length a; vec = Array.copy a }

let of_list l =
  match l with
  | [] -> create ()
  | [ x ] -> return x
  | [ x; y ] -> { size = 2; vec = [| x; y |] }
  | x :: _ ->
    let v = create_with ~capacity:(List.length l) x in
    List.iter (push_unsafe_ v) l;
    v

let to_array v = Array.sub v.vec 0 v.size
let to_list v = List.rev (fold (fun acc x -> x :: acc) [] v)

let of_gen ?(init = create ()) g =
  let rec aux g =
    match g () with
    | None -> init
    | Some x ->
      push init x;
      aux g
  in
  aux g

let to_gen v =
  let { size; vec } = v in
  let i = ref 0 in
  fun () ->
    if !i < size then (
      let x = vec.(!i) in
      incr i;
      Some x
    ) else
      None

let to_string ?(start = "") ?(stop = "") ?(sep = ", ") item_to_string v =
  start ^ (to_list v |> List.map item_to_string |> String.concat sep) ^ stop

let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
    ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_item fmt v =
  pp_start fmt ();
  iteri
    (fun i x ->
      if i > 0 then pp_sep fmt ();
      pp_item fmt x)
    v;
  pp_stop fmt ()

let ( let+ ) = ( >|= )
let ( let* ) = ( >>= )
let[@inline] ( and+ ) a1 a2 = monoid_product (fun x y -> x, y) a1 a2
let ( and* ) = ( and+ )
