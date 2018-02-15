(* This file is free software, part of containers. See file "license" for more details. *)

(*$inject

  let _listuniq =
    let g = Q.(small_list (pair small_int small_int)) in
    Q.map_same_type
      (fun l ->
        CCList.sort_uniq ~cmp:(fun a b -> Pervasives.compare (fst a)(fst b)) l
      ) g
  ;;
*)

(** {1 Hash Tries} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Transient IDs} *)
module Transient = struct
  type state = { mutable frozen: bool }
  type t = Nil | St of state
  let empty = Nil
  let equal a b = Pervasives.(==) a b
  let create () = St {frozen=false}
  let active = function Nil -> false | St st -> not st.frozen
  let frozen = function Nil -> true | St st -> st.frozen
  let freeze = function Nil -> () | St st -> st.frozen <- true
  let with_ f =
    let r = create() in
    try
      let x = f r in
      freeze r;
      x
    with e ->
      freeze r;
      raise e
  exception Frozen
end

(* function array *)
module A = struct
  type 'a t = {
    arr: 'a array;
    id: Transient.t;
  }

  let length_log = 5
  let max_length = 32
  let mask = max_length-1

  let () = assert (max_length = 1 lsl length_log)

  let length a = Array.length a.arr

  let create ~id = { arr= [| |]; id; }

  let empty = {arr=[| |]; id=Transient.empty}
  let is_empty a = length a = 0

  let return x = { arr=[| x |]; id=Transient.empty}

  let owns ~id a =
    Transient.active id && Transient.equal id a.id

  let get a i =
    if i<0 || i >= length a then invalid_arg "A.get";
    Array.unsafe_get a.arr i

  (* push at the back *)
  let push x a =
    let n = length a in
    if n = max_length then invalid_arg "A.push";
    let arr = Array.make (n+1) x in
    Array.blit a.arr 0 arr 0 n;
    {a with arr;}

  let pop a =
    let n = length a in
    if n=0 then invalid_arg "A.pop";
    let arr = Array.sub a.arr 0 (n-1) in
    {a with arr}

  let append a b =
    let n_a = length a in
    let n_b = length b in
    if n_a + n_b > max_length then invalid_arg "A.append";
    if n_a = 0 then b
    else if n_b = 0 then a
    else (
      let arr = Array.make (n_a+n_b) (a.arr.(0)) in
      Array.blit a.arr 0 arr 0 n_a;
      Array.blit b.arr 0 arr n_a n_b;
      {id=Transient.empty; arr}
    )

  let set ~mut a i x =
    if i<0 || i > length a || i >= max_length then invalid_arg "A.set";
    if i=length a then (
      (* insert in a longer copy *)
      let arr = Array.make (i+1) x in
      Array.blit a.arr 0 arr 0 i;
      {a with arr}
    ) else if mut then (
      (* replace element at [i] in place *)
      a.arr.(i) <- x;
      a
    ) else (
      (* replace element at [i] in copy *)
      let arr = Array.copy a.arr in
      arr.(i) <- x;
      {a with arr}
    )

  let iteri f a = Array.iteri f a.arr

  let fold f acc a = Array.fold_left f acc a.arr
end

(** {2 Functors} *)

type 'a t = {
  size: int;
  leaves: 'a A.t;
  subs: 'a t A.t;
}
(* invariant:
   - [A.length leaves < A.max_length ==> A.is_empty subs]
   - either:
    * [exists n. forall i. subs[i].size = n] (all subtrees of same size)
    * [exists n i.
        (forall j<i. sub[j].size=32^{n+1}-1) &
        (forall j>=i, sub[j].size<32^{n+1}-1)]
    (prefix of subs has size of complete binary tree; suffix has
     smaller size (actually decreasing))
  *)


let empty = {size=0; leaves=A.empty; subs=A.empty}

let is_empty {size;_} = size=0

(*$T
  is_empty empty
*)

let length {size;_} = size

(*$T
  not (is_empty (return 2))
  length (return 2) = 1
*)

let return x = {leaves=A.return x; subs=A.empty; size=1}

type idx_l =
  | L1 of int
  | L2 of int * int
  | L3 of int * int * int
  | L4 of int * int * int * int
  | L_cons of int * idx_l

let cons_idx x1 l = match l with
  | L1 x2 -> L2 (x1,x2)
  | L2 (x2,x3) -> L3 (x1,x2,x3)
  | L3 (x2,x3,x4) -> L4 (x1,x2,x3,x4)
  | L4 _ | L_cons _ -> L_cons (x1, l)

(* split an index into a low and high parts *)
let low_idx_ i = i land A.mask

let high_idx_ i = i lsr A.length_log

let combine_idx i j = (i lsl A.length_log) lor j

(* split an index into a high part, < 32, and a low part *)
let split_idx i : idx_l =
  let rec aux high low =
    if high = 0 then low
    else if high < A.max_length then cons_idx (high-1) low
    else aux (high_idx_ high) (cons_idx (low_idx_ high) low)
  in
  aux (high_idx_ i) (L1 (low_idx_ i))

let get_ (i:int) (m:'a t) : 'a =
  let rec aux l m = match l with
    | L1 x1 ->
      assert (x1 < A.length m.leaves);
      A.get m.leaves x1
    | L2 (x1,x2) -> aux (L1 x2) (A.get m.subs x1)
    | L3 (x1,x2,x3) -> aux (L2 (x2,x3)) (A.get m.subs x1)
    | L4 (x1,x2,x3,x4) -> aux (L3 (x2,x3,x4)) (A.get m.subs x1)
    | L_cons (x1,x2) -> aux x2 (A.get m.subs x1)
  in
  aux (split_idx i) m

(*$Q
   _listuniq (fun l -> \
    let m = of_list l in \
    List.for_all (fun (i,y) -> get_exn i m = y) @@ List.mapi CCPair.make l)
*)

let get_exn i v =
  if i >= 0 && i < length v then get_ i v else raise Not_found

let get i v =
  if i >= 0 && i < length v then Some (get_ i v) else None

let push_ (i:int) (x:'a) (m:'a t) : 'a t =
  let rec aux l m = match l with
    | L1 x1 ->
      assert (x1=A.length m.leaves);
      assert (A.length m.leaves < A.max_length);
      assert (A.is_empty m.subs);
      {m with size=m.size+1; leaves=A.push x m.leaves}
    | L2 (x1,x2) -> aux_replace_sub (L1 x2) m x1
    | L3 (x1,x2,x3) -> aux_replace_sub (L2 (x2,x3)) m x1
    | L4 (x1,x2,x3,x4) -> aux_replace_sub (L3 (x2,x3,x4)) m x1
    | L_cons (x1,x2) -> aux_replace_sub x2 m x1
  and aux_replace_sub l m x =
    assert (x <= A.length m.subs);
    (* insert in subtree, possibly a new one *)
    let sub_m =
      if x < A.length m.subs then A.get m.subs x else empty
    in
    let sub_m = aux l sub_m in
    {m with size=m.size+1; subs=A.set ~mut:false m.subs x sub_m}
  in
  aux (split_idx i) m

let push x (v:_ t) : _ t = push_ v.size x v

let pop_ i (m:'a t) : 'a * 'a t =
  let rec aux l m = match l with
    | L1 x1 ->
      assert (x1+1 = A.length m.leaves); (* last one *)
      let x = A.get m.leaves x1 in
      x, {m with size=m.size-1; leaves=A.pop m.leaves}
    | L2 (x1,x2) -> aux_remove_sub (L1 x2) m x1
    | L3 (x1,x2,x3) -> aux_remove_sub (L2 (x2,x3)) m x1
    | L4 (x1,x2,x3,x4) -> aux_remove_sub (L3 (x2,x3,x4)) m x1
    | L_cons (x1,x2) -> aux_remove_sub x2 m x1
  and aux_remove_sub l m x =
    let sub = A.get m.subs x in
    let y, sub' = aux l sub in
    if is_empty sub' then (
      assert (i+1 = A.length m.subs); (* last one *)
      y, {m with size=m.size-1; subs=A.pop m.subs}
    ) else (
      y, {m with size=m.size-1; subs=A.set ~mut:false m.subs x sub}
    )
  in
  aux (split_idx i) m

let pop_exn (v:'a t) : 'a * 'a t =
  if v.size=0 then failwith "Fun_vec.pop_exn";
  pop_ v.size v

let pop (v:'a t) : ('a * 'a t) option =
  if v.size=0 then None else Some (pop_ v.size v)

let iteri ~f (m : 'a t) : unit =
  (* basically, a 32-way BFS traversal.
     The queue contains subtrees to explore, along with their high_idx_ offsets *)
  let q : (int * 'a t) Queue.t = Queue.create() in
  Queue.push (0,m) q;
  while not (Queue.is_empty q) do
    let high, m = Queue.pop q in
    A.iteri (fun i x -> f (combine_idx high i) x) m.leaves;
    A.iteri (fun i sub -> Queue.push (combine_idx i high, sub) q) m.subs;
  done

let foldi ~f ~x m =
  let acc = ref x in
  iteri m
    ~f:(fun i x -> acc := f !acc i x);
  !acc

let iter ~f m = iteri ~f:(fun _ x -> f x) m

let fold ~f ~x m = foldi ~f:(fun acc _ x -> f acc x) ~x m

let of_list l = List.fold_left (fun v x -> push x v) empty l

let to_list m = fold m ~f:(fun acc x -> x::acc) ~x:[] |> List.rev

(* TODO

(* add [k,v] to the list [l], removing old binding if any *)
let rec add_list_ k v l = match l with
  | Nil -> One (k,v)
  | One (k1, v1) ->
    if Key.equal k k1 then One (k, v) else Two (k,v,k1,v1)
  | Two (k1, v1, k2, v2) ->
    if Key.equal k k1 then Two (k, v, k2, v2)
    else if Key.equal k k2 then Two (k, v, k1, v1)
    else Cons (k, v, l)
  | Cons (k', v', tail) ->
    if Key.equal k k'
    then Cons (k, v, tail) (* replace *)
    else Cons (k', v', add_list_ k v tail)

let node_ leaf a = N (leaf, a)


(* [h]: hash, with the part required to reach this leaf removed
    [id] is the transient ID used for mutability *)
let rec add_ ~id k v ~h m = match m with
  | E -> S (h, k, v)
  | S (h', k', v') ->
    if Hash.equal h h'
    then if Key.equal k k'
      then S (h, k, v)  (* replace *)
      else L (h, Cons (k, v, Cons (k', v', Nil)))
    else
      make_array_ ~id ~leaf:(Cons (k', v', Nil)) ~h_leaf:h' k v ~h
  | L (h', l) ->
    if Hash.equal h h'
    then L (h, add_list_ k v l)
    else (* split into N *)
      make_array_ ~id ~leaf:l ~h_leaf:h' k v ~h
  | N (leaf, a) ->
    if Hash.is_0 h
    then node_ (add_list_ k v leaf) a
    else
      let mut = A.owns ~id a in (* can we modify [a] in place? *)
      node_ leaf (add_to_array_ ~id ~mut k v ~h a)

(* make an array containing a leaf, and insert (k,v) in it *)
and make_array_ ~id ~leaf ~h_leaf:h' k v ~h =
  let a = A.create ~id in
  let a, leaf =
    if Hash.is_0 h' then a, leaf
    else
      (* put leaf in the right bucket *)
      let i = Hash.rem h' in
      let h'' = Hash.quotient h' in
      A.set ~mut:true a i (L (h'', leaf)), Nil
  in
  (* then add new node *)
  let a, leaf =
    if Hash.is_0 h then a, add_list_ k v leaf
    else add_to_array_ ~id ~mut:true k v ~h a, leaf
  in
  N (leaf, a)

(* add k->v to [a] *)
and add_to_array_ ~id ~mut k v ~h a =
  (* insert in a bucket *)
  let i = Hash.rem h in
  let h' = Hash.quotient h in
  A.update ~default:E ~mut a i (fun x -> add_ ~id k v ~h:h' x)

let add k v m = add_ ~id:Transient.empty k v ~h:(hash_ k) m

(*$Q
   _listuniq (fun l -> \
      let m = List.fold_left (fun m (x,y) -> add x y m) empty l in \
      List.for_all (fun (x,y) -> get_exn x m = y) l)
*)

let add_mut ~id k v m =
  if Transient.frozen id then raise Transient.Frozen;
  add_ ~id k v ~h:(hash_ k) m

(*$R
  let lsort = List.sort Pervasives.compare in
  let m = of_list [1, 1; 2, 2] in
  let id = Transient.create() in
  let m' = add_mut ~id 3 3 m in
  let m' = add_mut ~id 4 4 m' in
  assert_equal [1, 1; 2, 2] (to_list m |> lsort);
  assert_equal [1, 1; 2, 2; 3,3; 4,4] (to_list m' |> lsort);
  Transient.freeze id;
  assert_bool "must raise"
    (try ignore(add_mut ~id 5 5 m'); false with Transient.Frozen -> true)
*)


exception LocalExit

let is_empty_arr_ a =
  try
    A.iter (fun t -> if not (is_empty t) then raise LocalExit) a;
    true
  with LocalExit -> false

let is_empty_list_ = function
  | Nil -> true
  | One _
  | Two _
  | Cons _ -> false

let rec remove_list_ k l = match l with
  | Nil -> Nil
  | One (k', _) ->
    if Key.equal k k' then Nil else l
  | Two (k1, v1, k2, v2) ->
    if Key.equal k k1 then One (k2, v2)
    else if Key.equal k k2 then One (k1, v1)
    else l
  | Cons (k', v', tail) ->
    if Key.equal k k'
    then tail
    else Cons (k', v', remove_list_ k tail)

let rec remove_rec_ ~id k ~h m = match m with
  | E -> E
  | S (_, k', _) ->
    if Key.equal k k' then E else m
  | L (h, l) ->
    let l = remove_list_ k l in
    if is_empty_list_ l then E else L (h, l)
  | N (leaf, a) ->
    let leaf, a =
      if Hash.is_0 h
      then remove_list_ k leaf, a
      else
        let i = Hash.rem h in
        let h' = Hash.quotient h in
        let new_t = remove_rec_ ~id k ~h:h' (A.get ~default:E a i) in
        if is_empty new_t
        then leaf, A.remove a i (* remove sub-tree *)
        else
          let mut = A.owns ~id a in
          leaf, A.set ~mut a i new_t
    in
    if is_empty_list_ leaf && is_empty_arr_ a
    then E
    else N (leaf, a)

let remove k m = remove_rec_ ~id:Transient.empty k ~h:(hash_ k) m

let remove_mut ~id k m =
  if Transient.frozen id then raise Transient.Frozen;
  remove_rec_ ~id k ~h:(hash_ k) m

(*$QR
  _listuniq (fun l ->
    let m = of_list l in
    List.for_all
      (fun (x,_) ->
        let m' = remove x m in
        not (mem x m') &&
        cardinal m' = cardinal m - 1 &&
        List.for_all
          (fun (y,v) -> y = x || get_exn y m' = v)
          l
    ) l
  )
*)

let update_ ~id k f m =
  let h = hash_ k in
  let opt_v = try Some (get_ k ~h m) with Not_found -> None in
  match opt_v, f opt_v with
    | None, None -> m
    | Some _, Some v
    | None, Some v -> add_ ~id k v ~h m
    | Some _, None -> remove_rec_ ~id k ~h m

let update k ~f m = update_ ~id:Transient.empty k f m

let update_mut ~id k ~f m =
  if Transient.frozen id then raise Transient.Frozen;
  update_ ~id k f m

(*$R
  let m = of_list [1, 1; 2, 2; 5, 5] in
  let m' = update 4
    (function
    | None -> Some 4
    | Some _ -> Some 0
    ) m
  in
  assert_equal [1,1; 2,2; 4,4; 5,5] (to_list m' |> List.sort Pervasives.compare);
*)

let iter ~f t =
  let rec aux = function
    | E -> ()
    | S (_, k, v) -> f k v
    | L (_,l) -> aux_list l
    | N (l,a) -> aux_list l; A.iter aux a
  and aux_list = function
    | Nil -> ()
    | One (k,v) -> f k v
    | Two (k1,v1,k2,v2) -> f k1 v1; f k2 v2
    | Cons (k, v, tl) -> f k v; aux_list tl
  in
  aux t

let fold ~f ~x:acc t =
  let rec aux acc t = match t with
    | E -> acc
    | S (_,k,v) -> f acc k v
    | L (_,l) -> aux_list acc l
    | N (l,a) -> let acc = aux_list acc l in A.fold aux acc a
  and aux_list acc l = match l with
    | Nil -> acc
    | One (k,v) -> f acc k v
    | Two (k1,v1,k2,v2) -> f (f acc k1 v1) k2 v2
    | Cons (k, v, tl) -> let acc = f acc k v in aux_list acc tl
  in
  aux acc t

(*$T
  let l = CCList.(1 -- 10 |> map (fun x->x,x)) in  \
  of_list l \
    |> fold ~f:(fun acc x y -> (x,y)::acc) ~x:[] \
    |> List.sort Pervasives.compare = l
*)

let cardinal m = fold ~f:(fun n _ _ -> n+1) ~x:0 m

let to_list m = fold ~f:(fun acc k v -> (k,v)::acc) ~x:[] m

let add_list_mut ~id m l =
  List.fold_left (fun acc (k,v) -> add_mut ~id k v acc) m l

let add_list m l =
  Transient.with_ (fun id -> add_list_mut ~id m l)

let of_list l = add_list empty l

let add_seq_mut ~id m seq =
  let m = ref m in
  seq (fun (k,v) -> m := add_mut ~id k v !m);
  !m

let add_seq m seq =
  Transient.with_ (fun id -> add_seq_mut ~id m seq)

let of_seq s = add_seq empty s

let to_seq m yield = iter ~f:(fun k v -> yield (k,v)) m

(*$Q
  _listuniq (fun l -> \
    (List.sort Pervasives.compare l) = \
      (l |> Sequence.of_list |> of_seq |> to_seq |> Sequence.to_list \
        |> List.sort Pervasives.compare) )
*)

let rec add_gen_mut ~id m g = match g() with
  | None -> m
  | Some (k,v) -> add_gen_mut ~id (add_mut ~id k v m) g

let add_gen m g =
  Transient.with_ (fun id -> add_gen_mut ~id m g)

let of_gen g = add_gen empty g

(* traverse the tree by increasing hash order, where the order compares
   hashes lexicographically by A.length_log-wide chunks of bits,
   least-significant chunks first *)
let to_gen m =
  let st = Stack.create() in
  Stack.push m st;
  let rec next() =
    if Stack.is_empty st then None
    else match Stack.pop st with
      | E -> next ()
      | S (_,k,v) -> Some (k,v)
      | L (_, Nil) -> next()
      | L (_, One (k,v)) -> Some (k,v)
      | L (h, Two (k1,v1,k2,v2)) ->
        Stack.push (L (h, One (k2,v2))) st;
        Some (k1,v1)
      | L (h, Cons(k,v,tl)) ->
        Stack.push (L (h, tl)) st;  (* tail *)
        Some (k,v)
      | N (l, a) ->
        A.iter
          (fun sub -> Stack.push sub st)
          a;
        Stack.push (L (Hash.zero, l)) st;  (* leaf *)
        next()
  in
  next

(*$Q
  _listuniq (fun l -> \
    (List.sort Pervasives.compare l) = \
      (l |> Gen.of_list |> of_gen |> to_gen |> Gen.to_list \
        |> List.sort Pervasives.compare) )
*)

let choose m = to_gen m ()

(*$T
  choose empty = None
  choose (of_list [1,1; 2,2]) <> None
*)

let choose_exn m = match choose m with
  | None -> raise Not_found
  | Some (k,v) -> k, v

let pp ppk ppv out m =
  let first = ref true in
  iter m
    ~f:(fun k v ->
      if !first then first := false else Format.fprintf out ";@ ";
      ppk out k;
      Format.pp_print_string out " -> ";
      ppv out v
    )

let rec as_tree m () = match m with
  | E -> `Nil
  | S (h,k,v) -> `Node (`L ((h:>int), [k,v]), [])
  | L (h,l) -> `Node (`L ((h:>int), list_as_tree_ l), [])
  | N (l,a) -> `Node (`N, as_tree (L (Hash.zero, l)) :: array_as_tree_ a)
and list_as_tree_ l = match l with
  | Nil -> []
  | One (k,v) -> [k,v]
  | Two (k1,v1,k2,v2) -> [k1,v1; k2,v2]
  | Cons (k, v, tail) -> (k,v) :: list_as_tree_ tail
and array_as_tree_ a = A.fold (fun acc t -> as_tree t :: acc) [] a
   *)

(*   TODO: $R again?
  let m = of_list CCList.( (501 -- 1000) @ (500 -- 1) |> map (fun i->i,i)) in
  assert_equal ~printer:CCInt.to_string 1000 (length m);
  assert_bool "check all get"
    (Sequence.for_all (fun i -> i = get_exn i m) Sequence.(1 -- 1000));
  let m = Sequence.(501 -- 1000 |> fold (fun m i -> remove i m) m) in
  assert_equal ~printer:CCInt.to_string 500 (length m);
  assert_bool "check all get after remove"
    (Sequence.for_all (fun i -> i = get_exn i m) Sequence.(1 -- 500));
  assert_bool "check all get after remove"
    (Sequence.for_all (fun i -> None = get i m) Sequence.(501 -- 1000));
*)
