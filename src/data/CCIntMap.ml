(* "Fast Mergeable Integer Maps", Okasaki & Gill.
   We use big-endian trees. *)

(** Masks with exactly one bit active *)
module Bit : sig
  type t = private int

  val highest : int -> t
  val min_int : t
  val equal : t -> t -> bool
  val is_0 : bit:t -> int -> bool
  val is_1 : bit:t -> int -> bool
  val mask : mask:t -> int -> int (* zeroes the bit, puts all lower bits to 1 *)
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val equal_int : int -> t -> bool
end = struct
  type t = int

  let min_int = min_int
  let equal : t -> t -> bool = Stdlib.( = )

  let rec highest_bit_naive x m =
    if x = m then
      m
    else
      highest_bit_naive (x land lnot m) (2 * m)

  let mask_20_ = 1 lsl 20
  let mask_40_ = 1 lsl 40

  let highest x =
    if x < 0 then
      min_int
    else if Sys.word_size > 40 && x > mask_40_ then (
      (* remove least significant 40 bits *)
      let x' = x land lnot (mask_40_ - 1) in
      highest_bit_naive x' mask_40_
    ) else if x > mask_20_ then (
      (* small shortcut: remove least significant 20 bits *)
      let x' = x land lnot (mask_20_ - 1) in
      highest_bit_naive x' mask_20_
    ) else
      highest_bit_naive x 1

  let[@inline] is_0 ~bit x = x land bit = 0
  let[@inline] is_1 ~bit x = x land bit = bit
  let mask ~mask x = x lor (mask - 1) land lnot mask
  (* small endian: let mask_ x ~mask = x land (mask - 1) *)

  let gt a b = b != min_int && (a = min_int || a > b)
  let lt a b = gt b a
  let equal_int : int -> int -> bool = Stdlib.( = )
end

type +'a t =
  | E (* empty *)
  | L of int * 'a (* leaf *)
  | N of int (* common prefix *) * Bit.t (* bit switch *) * 'a t * 'a t

let empty = E

let[@inline] is_empty = function
  | E -> true
  | _ -> false

let[@inline] is_prefix_ ~prefix y ~bit = prefix = Bit.mask y ~mask:bit

(* small endian: let branching_bit_ a _ b _ = lowest_bit_ (a lxor b) *)
let branching_bit_ a b = Bit.highest (a lxor b)

(* TODO use hint in branching_bit_ *)

let check_invariants t =
  (* check that keys are prefixed by every node in their path *)
  let rec check_keys path t =
    match t with
    | E -> true
    | L (k, _) ->
      List.for_all
        (fun (prefix, switch, side) ->
          is_prefix_ ~prefix k ~bit:switch
          &&
          match side with
          | `Left -> Bit.is_0 k ~bit:switch
          | `Right -> Bit.is_1 k ~bit:switch)
        path
    | N (prefix, switch, l, r) ->
      check_keys ((prefix, switch, `Left) :: path) l
      && check_keys ((prefix, switch, `Right) :: path) r
  in
  check_keys [] t

let rec find_exn k t =
  match t with
  | E -> raise Not_found
  | L (k', v) when k = k' -> v
  | L _ -> raise Not_found
  | N (prefix, m, l, r) ->
    if is_prefix_ ~prefix k ~bit:m then
      if Bit.is_0 k ~bit:m then
        find_exn k l
      else
        find_exn k r
    else
      raise Not_found

let find k t = try Some (find_exn k t) with Not_found -> None

let mem k t =
  try
    ignore (find_exn k t);
    true
  with Not_found -> false

let mk_node_ prefix switch l r =
  match l, r with
  | E, o | o, E -> o
  | _ -> N (prefix, switch, l, r)

(* join trees t1 and t2 with prefix p1 and p2 respectively
   (p1 and p2 do not overlap) *)
let join_ t1 p1 t2 p2 =
  let switch = branching_bit_ p1 p2 in
  let prefix = Bit.mask p1 ~mask:switch in
  if Bit.is_0 p1 ~bit:switch then (
    assert (Bit.is_1 p2 ~bit:switch);
    mk_node_ prefix switch t1 t2
  ) else (
    assert (Bit.is_0 p2 ~bit:switch);
    mk_node_ prefix switch t2 t1
  )

let singleton k v = L (k, v)

(* c: conflict function *)
let rec insert_ c k v t =
  match t with
  | E -> L (k, v)
  | L (k', v') ->
    if k = k' then
      L (k, c ~old:v' v)
    else
      join_ t k' (L (k, v)) k
  | N (prefix, switch, l, r) ->
    if is_prefix_ ~prefix k ~bit:switch then
      if Bit.is_0 k ~bit:switch then
        N (prefix, switch, insert_ c k v l, r)
      else
        N (prefix, switch, l, insert_ c k v r)
    else
      join_ (L (k, v)) k t prefix

let add k v t = insert_ (fun ~old:_ v -> v) k v t

let rec remove k t =
  match t with
  | E -> E
  | L (k', _) ->
    if k = k' then
      E
    else
      t
  | N (prefix, switch, l, r) ->
    if is_prefix_ ~prefix k ~bit:switch then
      if Bit.is_0 k ~bit:switch then
        mk_node_ prefix switch (remove k l) r
      else
        mk_node_ prefix switch l (remove k r)
    else
      t
(* not present *)

let update k f t =
  try
    let v = find_exn k t in
    match f (Some v) with
    | None -> remove k t
    | Some v' -> add k v' t
  with Not_found ->
    (match f None with
    | None -> t
    | Some v -> add k v t)

let doubleton k1 v1 k2 v2 = add k1 v1 (singleton k2 v2)

let rec equal ~eq a b =
  Stdlib.( == ) a b
  ||
  match a, b with
  | E, E -> true
  | L (ka, va), L (kb, vb) -> ka = kb && eq va vb
  | N (pa, sa, la, ra), N (pb, sb, lb, rb) ->
    pa = pb && Bit.equal sa sb && equal ~eq la lb && equal ~eq ra rb
  | E, _ | N _, _ | L _, _ -> false

let rec iter f t =
  match t with
  | E -> ()
  | L (k, v) -> f k v
  | N (_, _, l, r) ->
    iter f l;
    iter f r

let rec fold f t acc =
  match t with
  | E -> acc
  | L (k, v) -> f k v acc
  | N (_, _, l, r) ->
    let acc = fold f l acc in
    fold f r acc

let cardinal t = fold (fun _ _ n -> n + 1) t 0

let rec mapi f t =
  match t with
  | E -> E
  | L (k, v) -> L (k, f k v)
  | N (p, s, l, r) -> N (p, s, mapi f l, mapi f r)

let rec map f t =
  match t with
  | E -> E
  | L (k, v) -> L (k, f v)
  | N (p, s, l, r) -> N (p, s, map f l, map f r)

let rec choose_exn = function
  | E -> raise Not_found
  | L (k, v) -> k, v
  | N (_, _, l, _) -> choose_exn l

let choose t = try Some (choose_exn t) with Not_found -> None

(** {2 Whole-collection operations} *)

let rec union f t1 t2 =
  match t1, t2 with
  | E, o | o, E -> o
  | L (k, v1), o2 ->
    insert_ (fun ~old v -> f k v old) k v1 o2 (* insert k, v into o *)
  | o1, L (k, v2) ->
    insert_ (fun ~old v -> f k old v) k v2 o1 (* insert k, v into o *)
  | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
    if p1 = p2 && Bit.equal m1 m2 then
      mk_node_ p1 m1 (union f l1 l2) (union f r1 r2)
    else if Bit.gt m1 m2 && is_prefix_ ~prefix:p1 p2 ~bit:m1 then
      if Bit.is_0 p2 ~bit:m1 then
        N (p1, m1, union f l1 t2, r1)
      else
        N (p1, m1, l1, union f r1 t2)
    else if Bit.lt m1 m2 && is_prefix_ ~prefix:p2 p1 ~bit:m2 then
      if Bit.is_0 p1 ~bit:m2 then
        N (p2, m2, union f t1 l2, r2)
      else
        N (p2, m2, l2, union f t1 r2)
    else
      join_ t1 p1 t2 p2

let rec inter f a b =
  match a, b with
  | E, _ | _, E -> E
  | L (k, v1), o2 ->
    (try
       let v2' = find_exn k o2 in
       L (k, f k v1 v2')
     with Not_found -> E)
  | o1, L (k, v2) ->
    (try
       let v1' = find_exn k o1 in
       L (k, f k v1' v2)
     with Not_found -> E)
  | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
    if p1 = p2 && Bit.equal m1 m2 then
      mk_node_ p1 m1 (inter f l1 l2) (inter f r1 r2)
    else if Bit.gt m1 m2 && is_prefix_ ~prefix:p1 p2 ~bit:m1 then
      if Bit.is_0 p2 ~bit:m1 then
        inter f l1 b
      else
        inter f r1 b
    else if Bit.lt m1 m2 && is_prefix_ ~prefix:p2 p1 ~bit:m2 then
      if Bit.is_0 p1 ~bit:m2 then
        inter f a l2
      else
        inter f a r2
    else
      E

let rec disjoint_union_ t1 t2 : _ t =
  match t1, t2 with
  | E, o | o, E -> o
  | L (k, v), o | o, L (k, v) -> insert_ (fun ~old:_ _ -> assert false) k v o
  | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
    if p1 = p2 && Bit.equal m1 m2 then
      mk_node_ p1 m1 (disjoint_union_ l1 l2) (disjoint_union_ r1 r2)
    else if Bit.gt m1 m2 && is_prefix_ ~prefix:p1 p2 ~bit:m1 then
      if Bit.is_0 p2 ~bit:m1 then
        mk_node_ p1 m1 (disjoint_union_ l1 t2) r1
      else
        mk_node_ p1 m1 l1 (disjoint_union_ r1 t2)
    else if Bit.lt m1 m2 && is_prefix_ ~prefix:p2 p1 ~bit:m2 then
      if Bit.is_0 p1 ~bit:m2 then
        mk_node_ p2 m2 (disjoint_union_ t1 l2) r2
      else
        mk_node_ p2 m2 l2 (disjoint_union_ t1 r2)
    else
      join_ t1 p1 t2 p2

let rec filter f m =
  match m with
  | E -> E
  | L (k, v) ->
    if f k v then
      m
    else
      E
  | N (_, _, l, r) -> disjoint_union_ (filter f l) (filter f r)

let rec filter_map f m =
  match m with
  | E -> E
  | L (k, v) ->
    (match f k v with
    | None -> E
    | Some v' -> L (k, v'))
  | N (_, _, l, r) -> disjoint_union_ (filter_map f l) (filter_map f r)

let rec merge ~f t1 t2 : _ t =
  let merge1 t = filter_map (fun k v -> f k (`Left v)) t
  and merge2 t = filter_map (fun k v -> f k (`Right v)) t
  and add_some k opt m =
    match opt with
    | None -> m
    | Some v -> insert_ (fun ~old:_ _ -> assert false) k v m
  in
  match t1, t2 with
  | E, o -> merge2 o
  | o, E -> merge1 o
  | L (k, v), o ->
    let others = merge2 (remove k o) in
    add_some k
      (try f k (`Both (v, find_exn k o)) with Not_found -> f k (`Left v))
      others
  | o, L (k, v) ->
    let others = merge1 (remove k o) in
    add_some k
      (try f k (`Both (find_exn k o, v)) with Not_found -> f k (`Right v))
      others
  | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
    if p1 = p2 && Bit.equal m1 m2 then
      mk_node_ p1 m1 (merge ~f l1 l2) (merge ~f r1 r2)
    else if Bit.gt m1 m2 && is_prefix_ ~prefix:p1 p2 ~bit:m1 then
      if Bit.is_0 p2 ~bit:m1 then
        mk_node_ p1 m1 (merge ~f l1 t2) (merge1 r1)
      else
        mk_node_ p1 m1 (merge1 l1) (merge ~f r1 t2)
    else if Bit.lt m1 m2 && is_prefix_ ~prefix:p2 p1 ~bit:m2 then
      if Bit.is_0 p1 ~bit:m2 then
        mk_node_ p2 m2 (merge ~f t1 l2) (merge2 r2)
      else
        mk_node_ p2 m2 (merge2 l2) (merge ~f t1 r2)
    else
      join_ (merge1 t1) p1 (merge2 t2) p2

(** {2 Conversions} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_list t l = List.fold_left (fun t (k, v) -> add k v t) t l
let of_list l = add_list empty l
let to_list t = fold (fun k v l -> (k, v) :: l) t []

let add_iter t iter =
  let t = ref t in
  iter (fun (k, v) -> t := add k v !t);
  !t

let of_iter iter = add_iter empty iter
let to_iter t yield = iter (fun k v -> yield (k, v)) t
let keys t yield = iter (fun k _ -> yield k) t
let values t yield = iter (fun _ v -> yield v) t

let rec add_gen m g =
  match g () with
  | None -> m
  | Some (k, v) -> add_gen (add k v m) g

let of_gen g = add_gen empty g

let to_gen m =
  let st = Stack.create () in
  Stack.push m st;
  let rec next () =
    if Stack.is_empty st then
      None
    else
      explore (Stack.pop st)
  and explore n =
    match n with
    | E -> next () (* backtrack *)
    | L (k, v) -> Some (k, v)
    | N (_, _, l, r) ->
      Stack.push r st;
      explore l
  in
  next

(* E < L < N; arbitrary order for switches *)
let compare ~cmp a b =
  let rec cmp_gen cmp a b =
    match a (), b () with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some (ka, va), Some (kb, vb) ->
      if ka = kb then (
        let c = cmp va vb in
        if c = 0 then
          cmp_gen cmp a b
        else
          c
      ) else
        compare ka kb
  in
  cmp_gen cmp (to_gen a) (to_gen b)

let rec add_seq m l =
  match l () with
  | Seq.Nil -> m
  | Seq.Cons ((k, v), tl) -> add_seq (add k v m) tl

let of_seq l = add_seq empty l

let to_seq m =
  (* [st]: stack of alternatives *)
  let rec explore st m () =
    match m with
    | E -> next st ()
    | L (k, v) -> Seq.Cons ((k, v), next st)
    | N (_, _, l, r) -> explore (r :: st) l ()
  and next st () =
    match st with
    | [] -> Seq.Nil
    | x :: st' -> explore st' x ()
  in
  next [ m ]

type 'a tree = unit -> [ `Nil | `Node of 'a * 'a tree list ]

let rec as_tree t () =
  match t with
  | E -> `Nil
  | L (k, v) -> `Node (`Leaf (k, v), [])
  | N (prefix, switch, l, r) ->
    `Node (`Node (prefix, (switch :> int)), [ as_tree l; as_tree r ])

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let pp pp_x out m =
  Format.fprintf out "@[<hov2>intmap {@,";
  let first = ref true in
  iter
    (fun k v ->
      if !first then
        first := false
      else
        Format.pp_print_string out ", ";
      Format.fprintf out "%d -> " k;
      pp_x out v;
      Format.pp_print_cut out ())
    m;
  Format.fprintf out "}@]"

(* Some thorough tests from Jan Midtgaar
   https://github.com/jmid/qc-ptrees
*)
