(* Persistent vector structure with fast get/push/pop.
   We follow https://hypirion.com/musings/understanding-persistent-vector-pt-1
   and following posts. *)

type 'a iter = ('a -> unit) -> unit

let num_bits = 4
let branching_factor = 1 lsl num_bits
let bitmask = branching_factor - 1

(** Short array with functional semantics *)
module A = struct
  open Array

  type 'a t = 'a array

  let length = length
  let get = get
  let[@inline] is_empty self = self = [||]
  let[@inline] return self = [| self |]
  let[@inline] is_full self = length self = branching_factor

  let equal eq a b =
    length a = length b
    &&
    try
      for i = 0 to length a - 1 do
        if not (eq (unsafe_get a i) (unsafe_get b i)) then raise_notrace Exit
      done;
      true
    with Exit -> false

  let[@inline] push (self : _ t) x =
    let n = length self in
    if n = branching_factor then invalid_arg "Pvec.push";
    let arr = Array.make (n + 1) x in
    Array.blit self 0 arr 0 n;
    arr

  let[@inline] pop self : _ t =
    let n = length self in
    if n = 0 then invalid_arg "Pvec.pop";
    Array.sub self 0 (n - 1)

  let set (self : _ t) i x : _ t =
    if i < 0 || i > length self || i >= branching_factor then
      invalid_arg "Pvec.set";
    if i = length self then (
      (* insert in a longer copy *)
      let arr = Array.make (i + 1) x in
      Array.blit self 0 arr 0 i;
      arr
    ) else (
      (* replace element at [i] in copy *)
      let arr = Array.copy self in
      arr.(i) <- x;
      arr
    )
end

type 'a tree =
  | Empty
  | Node of 'a tree A.t
  | Leaf of 'a A.t

type 'a t = {
  t: 'a tree;  (** The 32-way tree *)
  size: int;  (** Exact number of elements *)
  shift: int;  (** num_bits*(depth of tree) *)
  tail: 'a A.t;  (** Tail array, for fast push/pop *)
}
(* invariants:
   - if size>0 then [not (is_empty tail)]
   - all leaves in [t] are at depth shift/5
*)

let empty_tree = Empty
let empty = { t = empty_tree; size = 0; shift = 0; tail = [||] }

let[@inline] is_empty_tree = function
  | Empty -> true
  | _ -> false

let[@inline] is_empty (self : _ t) = self.size = 0
let[@inline] length (self : _ t) = self.size
let[@inline] return x = { empty with size = 1; tail = A.return x }
let[@inline] tail_off (self : _ t) : int = self.size - A.length self.tail

let[@unroll 2] rec get_tree_ (self : 'a tree) (shift : int) i : 'a =
  match self with
  | Empty -> invalid_arg "pvec.get"
  | Leaf a -> A.get a (i land bitmask)
  | Node a ->
    let idx = (i lsr shift) land bitmask in
    get_tree_ (A.get a idx) (shift - num_bits) i

let get (self : 'a t) (i : int) : 'a =
  if i < 0 then
    invalid_arg "pvec.get"
  else (
    let tail_off = self.size - A.length self.tail in
    if i >= tail_off then
      A.get self.tail (i - tail_off)
    else
      get_tree_ self.t self.shift i
  )

let[@inline] get_opt self i =
  try Some (get self i) with Invalid_argument _ -> None

(** Build a tree leading to [tail] with indices 0 at each node *)
let rec build_new_tail_spine_ shift tail : _ tree =
  if shift = 0 then
    Leaf tail
  else
    Node [| build_new_tail_spine_ (shift - num_bits) tail |]

let rec insert_tail_ (self : _ tree) shift i (tail : _ A.t) : _ tree =
  match self with
  | Empty ->
    if shift = 0 then
      Leaf tail
    else (
      assert ((i lsl shift) land bitmask = 0);
      Node [| insert_tail_ Empty (shift - num_bits) i tail |]
    )
  | Leaf _ -> assert false
  | Node a ->
    (* would be in the {!build_new_tail_spine_} case *)
    assert (i <> 0);
    let idx = (i lsr shift) land bitmask in
    let sub, must_push =
      if idx < A.length a then
        A.get a idx, false
      else
        Empty, true
    in
    let new_sub = insert_tail_ sub (shift - num_bits) i tail in
    let a =
      if must_push then
        A.push a new_sub
      else
        A.set a idx new_sub
    in
    Node a

let[@inline never] push_full_ self x : _ t =
  if 1 lsl (self.shift + num_bits) = self.size - A.length self.tail then (
    (* tree is full, add a level *)
    let t = Node [| self.t; build_new_tail_spine_ self.shift self.tail |] in
    { t; size = self.size + 1; shift = self.shift + num_bits; tail = [| x |] }
  ) else (
    (* insert at the end of the current tree *)
    let idx = self.size - A.length self.tail in
    let t = insert_tail_ self.t self.shift idx self.tail in
    { t; size = self.size + 1; shift = self.shift; tail = [| x |] }
  )

let[@inline] push (self : _ t) x : _ t =
  if A.is_full self.tail then
    push_full_ self x
  else
    { self with tail = A.push self.tail x; size = self.size + 1 }

let rec pop_tail_from_tree_ (self : _ tree) shift i : 'a A.t * 'a tree =
  match self with
  | Empty -> assert false
  | Leaf tail ->
    assert (shift = 0);
    tail, Empty
  | Node a ->
    let idx = (i lsr shift) land bitmask in
    let sub = A.get a idx in
    let tail, new_sub = pop_tail_from_tree_ sub (shift - num_bits) i in
    let new_tree =
      if is_empty_tree new_sub then (
        let a = A.pop a in
        if A.is_empty a then
          Empty
        else
          Node a
      ) else
        Node (A.set a idx new_sub)
    in
    tail, new_tree

let[@inline never] move_last_leaf_to_tail (self : _ t) : _ t =
  assert (A.length self.tail = 1);
  if self.size = 1 then
    (* back to empty *)
    empty
  else (
    (* idx of the beginning of the tail *)
    let idx = self.size - 1 - branching_factor in
    let tail, t = pop_tail_from_tree_ self.t self.shift idx in
    let t, shift =
      match t with
      | Node [| t' |] ->
        (* all indices have 00000 as MSB, remove one level *)
        t', self.shift - num_bits
      | _ -> t, self.shift
    in
    { tail; size = self.size - 1; shift; t }
  )

let pop (self : 'a t) : 'a * 'a t =
  if self.size = 0 then invalid_arg "pvec.pop";
  let x = A.get self.tail (A.length self.tail - 1) in
  let new_tail = A.pop self.tail in
  if A.is_empty new_tail then (
    let new_self = move_last_leaf_to_tail self in
    x, new_self
  ) else (
    let new_self = { self with size = self.size - 1; tail = new_tail } in
    x, new_self
  )

let pop_opt (self : 'a t) : ('a * 'a t) option =
  if self.size = 0 then
    None
  else
    Some (pop self)

let[@inline] last self =
  if self.size = 0 then invalid_arg "pvec.last";
  A.get self.tail (A.length self.tail - 1)

let last_opt self =
  if self.size = 0 then
    None
  else
    Some (A.get self.tail (A.length self.tail - 1))

let drop_last self =
  if self.size = 0 then
    self
  else
    snd (pop self)

let rec iter_rec_ f (self : _ tree) =
  match self with
  | Empty -> ()
  | Leaf a ->
    for i = 0 to A.length a - 1 do
      f (Array.unsafe_get a i)
    done
  | Node a ->
    for i = 0 to A.length a - 1 do
      iter_rec_ f (Array.unsafe_get a i)
    done

let iter f self =
  iter_rec_ f self.t;
  for i = 0 to A.length self.tail - 1 do
    f (Array.unsafe_get self.tail i)
  done

let fold_left f x m =
  let acc = ref x in
  iter (fun x -> acc := f !acc x) m;
  !acc

let rec iteri_rec_ f idx (self : _ tree) =
  match self with
  | Empty -> ()
  | Leaf a ->
    for i = 0 to A.length a - 1 do
      let j = idx lor i in
      f j (Array.unsafe_get a i)
    done
  | Node a ->
    for i = 0 to A.length a - 1 do
      let idx = (idx lsl num_bits) lor i in
      iteri_rec_ f idx (Array.unsafe_get a i)
    done

let iteri f (self : 'a t) : unit =
  iteri_rec_ f 0 self.t;
  let tail_off = tail_off self in
  for i = 0 to A.length self.tail - 1 do
    f (i + tail_off) (Array.unsafe_get self.tail i)
  done

let rec iter_rev_rec_ f (self : _ tree) =
  match self with
  | Empty -> ()
  | Leaf a ->
    for i = A.length a - 1 downto 0 do
      f (Array.unsafe_get a i)
    done
  | Node a ->
    for i = A.length a - 1 downto 0 do
      iter_rev_rec_ f (Array.unsafe_get a i)
    done

let iter_rev f (self : 'a t) : unit =
  for i = A.length self.tail - 1 downto 0 do
    f (Array.unsafe_get self.tail i)
  done;
  iter_rev_rec_ f self.t

let rec iteri_rev_rec_ f idx (self : _ tree) =
  match self with
  | Empty -> ()
  | Leaf a ->
    for i = A.length a - 1 downto 0 do
      let j = idx lor i in
      f j (Array.unsafe_get a i)
    done
  | Node a ->
    for i = A.length a - 1 downto 0 do
      let idx = (idx lsl num_bits) lor i in
      iteri_rev_rec_ f idx (Array.unsafe_get a i)
    done

let iteri_rev f (self : 'a t) : unit =
  let tail_off = tail_off self in
  for i = A.length self.tail - 1 downto 0 do
    f (i + tail_off) (Array.unsafe_get self.tail i)
  done;
  iteri_rev_rec_ f (tail_off - 1) self.t

let fold_lefti f x m =
  let acc = ref x in
  iteri (fun i x -> acc := f !acc i x) m;
  !acc

let fold_revi f x m =
  let acc = ref x in
  iteri_rev (fun i x -> acc := f !acc i x) m;
  !acc

let fold_rev f x m =
  let acc = ref x in
  iter_rev (fun x -> acc := f !acc x) m;
  !acc

let rec map_t f (self : _ tree) : _ tree =
  match self with
  | Empty -> Empty
  | Node a ->
    let a = Array.map (map_t f) a in
    Node a
  | Leaf a -> Leaf (Array.map f a)

let map f (self : _ t) : _ t =
  { self with t = map_t f self.t; tail = Array.map f self.tail }

let append a b =
  if is_empty b then
    a
  else
    fold_left push a b

let flat_map f v : _ t = fold_left (fun acc x -> append acc (f x)) empty v

let rec equal_tree eq t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node a, Node b -> A.equal (equal_tree eq) a b
  | Leaf a, Leaf b -> A.equal eq a b
  | (Empty | Leaf _ | Node _), _ -> false

let equal eq (a : _ t) (b : _ t) : bool =
  a.size = b.size && A.equal eq a.tail b.tail && equal_tree eq a.t b.t

let add_list v l = List.fold_left push v l
let of_list l = add_list empty l
let to_list m = fold_rev (fun acc x -> x :: acc) [] m

let add_iter v seq =
  let v = ref v in
  seq (fun x -> v := push !v x);
  !v

let of_iter s = add_iter empty s
let to_iter m yield = iteri (fun _ v -> yield v) m

let make n x : _ t =
  (* TODO: probably we can optimize that? *)
  of_iter (fun k ->
      for _i = 1 to n do
        k x
      done)

let rec add_seq self seq =
  match seq () with
  | Seq.Nil -> self
  | Seq.Cons (x, tl) -> add_seq (push self x) tl

let of_seq seq = add_seq empty seq

let to_seq self : _ Seq.t =
  let rec to_seq (stack : ('a tree * int) list) () =
    match stack with
    | [] -> Seq.Nil
    | (Empty, _) :: tl -> to_seq tl ()
    | (Leaf a, i) :: tl when i < Array.length a ->
      Seq.Cons (A.get a i, to_seq ((Leaf a, i + 1) :: tl))
    | (Leaf _, _) :: tl -> to_seq tl ()
    | (Node a, i) :: tl when i < A.length a ->
      to_seq ((A.get a i, 0) :: (Node a, i + 1) :: tl) ()
    | (Node _, _) :: tl -> to_seq tl ()
  in
  to_seq [ self.t, 0; Leaf self.tail, 0 ]

let choose self =
  if self.size = 0 then
    None
  else
    Some (A.get self.tail 0)

module Private_ = struct
  type 'a printer = Format.formatter -> 'a -> unit

  let fpf = Format.fprintf

  let pp_array ppx out a =
    fpf out "[@[%a@]]"
      (Format.pp_print_list
         ~pp_sep:(fun out () -> Format.fprintf out ";@ ")
         ppx)
      (Array.to_list a)

  let rec debugtree ppx out (self : _ tree) : unit =
    match self with
    | Empty -> fpf out "()"
    | Leaf a -> fpf out "leaf(%a)" (pp_array ppx) a
    | Node a -> fpf out "node(%a)" (pp_array @@ debugtree ppx) a

  let debug ppx out self =
    fpf out
      "@[<v>pvec {@ size: %d; shift: %d;@ @[<2>tree:@ %a@];@ @[<2>tail:@ \
       %a@]@]}"
      self.size self.shift (debugtree ppx) self.t (pp_array ppx) self.tail
end
