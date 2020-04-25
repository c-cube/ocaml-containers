(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Weight-Balanced Tree}

    Most of this comes from "implementing sets efficiently in a functional language",
    Stephen Adams.

    The coefficients 5/2, 3/2 for balancing come from "balancing weight-balanced trees"
*)

(*$inject
  module M = Make(CCInt)

  type op =
    | Add of int * int
    | Remove of int
    | Remove_min

  let gen_op = CCRandom.(choose_exn
    [ return Remove_min
    ; map (fun x->Remove x) small_int
    ; pure (fun x y->Add (x,y)) <*> small_int <*> small_int])
  and pp_op =let open Printf in
  function Add (x,y) -> sprintf "Add %d %d" x y
    | Remove x -> sprintf "Remove %d" x | Remove_min -> "Remove_min"

  let apply_ops l m = List.fold_left
    (fun m  -> function
      | Add (i,b) -> M.add i b m
      | Remove i -> M.remove i m
      | Remove_min ->
          try let _, _, m' = M.extract_min m in m' with Not_found -> m
    ) m l

  let op = Q.make ~print:pp_op gen_op

  let _list_uniq = CCList.sort_uniq ~cmp:(CCFun.compose_binop fst Stdlib.compare)
*)

(*$Q & ~count:200
  Q.(list op) (fun l -> let m = apply_ops l M.empty in M.balanced m)
*)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type KEY = sig
  include ORD
  val weight : t -> int
end

(** {2 Signature} *)

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val singleton : key -> 'a -> 'a t

  val mem : key -> _ t -> bool

  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if the key is not present *)

  val nth : int -> 'a t -> (key * 'a) option
  (** [nth i m] returns the [i]-th [key, value] in the ascending
      order. Complexity is [O(log (cardinal m))] *)

  val nth_exn : int -> 'a t -> key * 'a
  (** @raise Not_found if the index is invalid *)

  val get_rank : key -> 'a t -> [`At of int | `After of int | `First]
  (** [get_rank k m] looks for the rank of [k] in [m], i.e. the index
      of [k] in the sorted list of bindings of [m].
      [let (`At n) = get_rank k m in nth_exn n m = get m k] should hold.
      @since 1.4 *)

  val add : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [get k m = Some v], [f None]
      otherwise. Then, if [f] returns [Some v'] it binds [k] to [v'],
      if [f] returns [None] it removes [k] *)

  val cardinal : _ t -> int

  val weight : _ t -> int

  val fold : f:('b -> key -> 'a -> 'b) -> x:'b -> 'a t -> 'b

  val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map values, giving both key and value. Will use {!WORD.of_list} to rebuild keys.
      @since 0.17
  *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** Map values, giving only the value.
      @since 0.17
  *)

  val iter : f:(key -> 'a -> unit) -> 'a t -> unit

  val split : key -> 'a t -> 'a t * 'a option * 'a t
  (** [split k t] returns [l, o, r] where [l] is the part of the map
      with keys smaller than [k], [r] has keys bigger than [k],
      and [o = Some v] if [k, v] belonged to the map *)

  val merge : f:(key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  (** Like {!Map.S.merge} *)

  val extract_min : 'a t -> key * 'a * 'a t
  (** [extract_min m] returns [k, v, m'] where [k,v] is the pair with the
      smallest key in [m], and [m'] does not contain [k].
      @raise Not_found if the map is empty *)

  val extract_max : 'a t -> key * 'a * 'a t
  (** [extract_max m] returns [k, v, m'] where [k,v] is the pair with the
      highest key in [m], and [m'] does not contain [k].
      @raise Not_found if the map is empty *)

  val choose : 'a t -> (key * 'a) option

  val choose_exn : 'a t -> key * 'a
  (** @raise Not_found if the tree is empty *)

  val random_choose : Random.State.t -> 'a t -> key * 'a
  (** Randomly choose a (key,value) pair within the tree, using weights
      as probability weights
      @raise Not_found if the tree is empty *)

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val of_list : (key * 'a) list -> 'a t

  val to_list : 'a t -> (key * 'a) list

  val add_iter : 'a t -> (key * 'a) iter -> 'a t

  val of_iter : (key * 'a) iter -> 'a t

  val to_iter : 'a t -> (key * 'a) iter

  val add_gen : 'a t -> (key * 'a) gen -> 'a t

  val of_gen : (key * 'a) gen -> 'a t

  val to_gen : 'a t -> (key * 'a) gen

  val pp : key printer -> 'a printer -> 'a t printer

  (**/**)
  val node_ : key -> 'a -> 'a t -> 'a t -> 'a t
  val balanced : _ t -> bool
  (**/**)
end

module MakeFull(K : KEY) : S with type key = K.t = struct
  type key = K.t

  type weight = int

  type +'a t =
    | E
    | N of key * 'a * 'a t * 'a t * weight

  let empty = E

  let is_empty = function
    | E -> true
    | N _ -> false

  let rec get_exn k m = match m with
    | E -> raise Not_found
    | N (k', v, l, r, _) ->
      match K.compare k k' with
        | 0 -> v
        | n when n<0 -> get_exn k l
        | _ -> get_exn k r

  let get k m =
    try Some (get_exn k m)
    with Not_found -> None

  let mem k m =
    try ignore (get_exn k m); true
    with Not_found -> false

  let singleton k v =
    N (k, v, E, E, K.weight k)

  let weight = function
    | E -> 0
    | N (_, _, _, _, w) -> w

  (* balancing parameters.

     We take the parameters from "Balancing weight-balanced trees", as they
     are rational and efficient. *)

  (* delta=5/2
     delta × (weight l + 1) ≥ weight r + 1
  *)
  let is_balanced l r =
    5 * (weight l + 1) >= 2 * (weight r + 1)

  (* gamma = 3/2
      weight l + 1 < gamma × (weight r + 1) *)
  let is_single l r =
    2 * (weight l + 1) < 3 * (weight r + 1)

  (* debug function *)
  let rec balanced = function
    | E -> true
    | N (_, _, l, r, _) ->
      is_balanced l r &&
      is_balanced r l &&
      balanced l &&
      balanced r

  (* smart constructor *)
  let mk_node_ k v l r =
    N (k, v, l, r, weight l + weight r + K.weight k)

  let single_l k1 v1 t1 t2 = match t2 with
    | E -> assert false
    | N (k2, v2, t2, t3, _) ->
      mk_node_ k2 v2 (mk_node_ k1 v1 t1 t2) t3

  let double_l k1 v1 t1 t2 = match t2 with
    | N (k2, v2, N (k3, v3, t2, t3, _), t4, _) ->
      mk_node_ k3 v3 (mk_node_ k1 v1 t1 t2) (mk_node_ k2 v2 t3 t4)
    | _ -> assert false

  let rotate_l k v l r = match r with
    | E -> assert false
    | N (_, _, rl, rr, _) ->
      if is_single rl rr
      then single_l k v l r
      else double_l k v l r

  (* balance towards left *)
  let balance_l k v l r =
    if is_balanced l r then mk_node_ k v l r
    else rotate_l k v l r

  let single_r k1 v1 t1 t2 = match t1 with
    | E -> assert false
    | N (k2, v2, t11, t12, _) ->
      mk_node_ k2 v2 t11 (mk_node_ k1 v1 t12 t2)

  let double_r k1 v1 t1 t2 = match t1 with
    | N (k2, v2, t11, N (k3, v3, t121, t122, _), _) ->
      mk_node_ k3 v3 (mk_node_ k2 v2 t11 t121) (mk_node_ k1 v1 t122 t2)
    | _ -> assert false

  let rotate_r k v l r = match l with
    | E -> assert false
    | N (_, _, ll, lr, _) ->
      if is_single lr ll
      then single_r k v l r
      else double_r k v l r

  (* balance toward right *)
  let balance_r k v l r =
    if is_balanced r l then mk_node_ k v l r
    else rotate_r k v l r

  let rec add k v m = match m with
    | E -> singleton k v
    | N (k', v', l, r, _) ->
      match K.compare k k' with
        | 0 -> mk_node_ k v l r
        | n when n<0 -> balance_r k' v' (add k v l) r
        | _ -> balance_l k' v' l (add k v r)

  (*$Q
    Q.(list (pair small_int bool)) (fun l -> \
      let m = M.of_list l in \
      M.balanced m)
    Q.(list (pair small_int small_int)) (fun l -> \
      let l = _list_uniq l in let m = M.of_list l in \
      List.for_all (fun (k,v) -> M.get_exn k m = v) l)
    Q.(list (pair small_int small_int)) (fun l -> \
      let l = _list_uniq l in let m = M.of_list l in \
      M.cardinal m = List.length l)
  *)

  (* extract min binding of the tree *)
  let rec extract_min m = match m with
    | E -> raise Not_found
    | N (k, v, E, r, _) -> k, v, r
    | N (k, v, l, r, _) ->
      let k', v', l' = extract_min l in
      k', v', balance_l k v l' r

  (* extract max binding of the tree *)
  let rec extract_max m = match m with
    | E -> raise Not_found
    | N (k, v, l, E, _) -> k, v, l
    | N (k, v, l, r, _) ->
      let k', v', r' = extract_max r in
      k', v', balance_r k v l r'

  let rec remove k m = match m with
    | E -> E
    | N (k', v', l, r, _) ->
      match K.compare k k' with
        | 0 ->
          begin match l, r with
            | E, E -> E
            | E, o
            | o, E -> o
            | _, _ ->
              if weight l > weight r
              then
                (* remove max element of [l] and put it at the root,
                   then rebalance towards the left if needed *)
                let k', v', l' = extract_max l in
                balance_l k' v' l' r
              else
                (* remove min element of [r] and rebalance *)
                let k', v', r' = extract_min r in
                balance_r k' v' l r'
          end
        | n when n<0 -> balance_l k' v' (remove k l) r
        | _ -> balance_r k' v' l (remove k r)

  (*$Q
    Q.(list_of_size Gen.(0 -- 30) (pair small_int small_int)) (fun l -> \
      let m = M.of_list l in \
      List.for_all (fun (k,_) -> \
        M.mem k m && (let m' = M.remove k m in  not (M.mem k m'))) l)
    Q.(list_of_size Gen.(0 -- 30) (pair small_int small_int)) (fun l -> \
      let m = M.of_list l in \
      List.for_all (fun (k,_) -> let m' = M.remove k m in M.balanced m') l)
  *)

  let update k f m =
    let maybe_v = get k m in
    match maybe_v, f maybe_v with
      | None, None -> m
      | Some _, None -> remove k m
      | _, Some v -> add k v m

  let rec nth_exn i m = match m with
    | E -> raise Not_found
    | N (k, v, l, r, w) ->
      let c = i - weight l in
      match c with
        | 0 -> k, v
        | n when n<0 -> nth_exn i l  (* search left *)
        | _ ->
          (* means c< K.weight k *)
          if i<w-weight r then k,v else nth_exn (i+weight r-w) r

  let nth i m =
    try Some (nth_exn i m)
    with Not_found -> None

  (*$T
    let m = CCList.(0 -- 1000 |> map (fun i->i,i) |> M.of_list) in \
    List.for_all (fun i -> M.nth_exn i m = (i,i)) CCList.(0--1000)
  *)

  let get_rank k m =
    let rec aux i k m = match m with
      | E -> if i=0 then `First else `After i
      | N (k', _, l, r, _) ->
        match K.compare k k' with
          | 0 -> `At (i + weight l)
          | n when n<0 -> aux i k l
          | _ -> aux (1 + weight l + i) k r
    in
    aux 0 k m

  (*$QR & ~count:1_000
    Q.(list_of_size Gen.(0 -- 30) (pair small_int small_int)) (fun l ->
      let l = CCList.sort_uniq ~cmp:(CCFun.compose_binop fst compare) l in
      let m = M.of_list l in
      List.for_all
        (fun (k,v) -> match M.get_rank k m with
          | `First | `After _ -> true
          | `At n -> (k,v) = M.nth_exn n m)
        l)
  *)

  let rec fold ~f ~x:acc m = match m with
    | E -> acc
    | N (k, v, l, r, _) ->
      let acc = fold ~f ~x:acc l in
      let acc = f acc k v in
      fold ~f ~x:acc r

  let rec mapi ~f = function
    | E -> E
    | N (k, v, l, r, w) ->
      N (k, f k v, mapi ~f l, mapi ~f r, w)

  let rec map ~f = function
    | E -> E
    | N (k, v, l, r, w) ->
      N (k, f v, map ~f l, map ~f r, w)

  let rec iter ~f m = match m with
    | E -> ()
    | N (k, v, l, r, _) ->
      iter ~f l;
      f k v;
      iter ~f r

  let choose_exn = function
    | E -> raise Not_found
    | N (k, v, _, _, _) -> k, v

  let choose = function
    | E -> None
    | N (k, v, _, _, _) -> Some (k,v)

  (* pick an index within [0.. weight m-1] and get the element with
     this index *)
  let random_choose st m =
    let w = weight m in
    if w=0 then raise Not_found;
    nth_exn (Random.State.int st w) m

  (* make a node (k,v,l,r) but balances on whichever side requires it *)
  let node_shallow_ k v l r =
    if is_balanced l r
    then if is_balanced r l
      then mk_node_ k v l r
      else balance_r k v l r
    else balance_l k v l r

  (* assume keys of [l] are smaller than [k] and [k] smaller than keys of [r],
     but do not assume anything about weights.
     returns a tree with l, r, and (k,v) *)
  let rec node_ k v l r = match l, r with
    | E, E -> singleton k v
    | E, o
    | o, E -> add k v o
    | N (kl, vl, ll, lr, _), N (kr, vr, rl, rr, _) ->
      let left = is_balanced l r in
      if left && is_balanced r l
      then mk_node_ k v l r
      else if not left
      then node_shallow_ kr vr (node_ k v l rl) rr
      else node_shallow_ kl vl ll (node_ k v lr r)

  (* join two trees, assuming all keys of [l] are smaller than keys of [r] *)
  let join_ l r = match l, r with
    | E, E -> E
    | E, o
    | o, E -> o
    | N _, N _ ->
      if weight l <= weight r
      then
        let k, v, r' = extract_min r in
        node_ k v l r'
      else
        let k, v, l' = extract_max l in
        node_ k v l' r

  (* if [o_v = Some v], behave like [mk_node k v l r]
      else behave like [join_ l r] *)
  let mk_node_or_join_ k o_v l r = match o_v with
    | None -> join_ l r
    | Some v -> node_ k v l r

  let rec split k m = match m with
    | E -> E, None, E
    | N (k', v', l, r, _) ->
      match K.compare k k' with
        | 0 -> l, Some v', r
        | n when n<0 ->
          let ll, o, lr = split k l in
          ll, o, node_ k' v' lr r
        | _ ->
          let rl, o, rr = split k r in
          node_ k' v' l rl, o, rr

  (*$QR & ~count:20
     Q.(list_of_size Gen.(1 -- 100) (pair small_int small_int)) ( fun lst ->
      let lst = _list_uniq lst in
      let m = M.of_list lst in
      List.for_all (fun (k,v) ->
        let l, v', r = M.split k m in
        v' = Some v
        && (M.to_iter l |> Iter.for_all (fun (k',_) -> k' < k))
        && (M.to_iter r |> Iter.for_all (fun (k',_) -> k' > k))
        && M.balanced m
        && M.cardinal l + M.cardinal r + 1 = List.length lst
      ) lst)
  *)

  let rec merge ~f a b = match a, b with
    | E, E -> E
    | E, N (k, v, l, r, _) ->
      let v' = f k None (Some v) in
      mk_node_or_join_ k v' (merge ~f E l) (merge ~f E r)
    | N (k, v, l, r, _), E ->
      let v' = f k (Some v) None in
      mk_node_or_join_ k v' (merge ~f l E) (merge ~f r E)
    | N (k1, v1, l1, r1, w1), N (k2, v2, l2, r2, w2) ->
      if K.compare k1 k2 = 0
      then (* easy case *)
        mk_node_or_join_ k1 (f k1 (Some v1) (Some v2))
          (merge ~f l1 l2) (merge ~f r1 r2)
      else if w1 <= w2
      then (* split left tree *)
        let l1', v1', r1' = split k2 a in
        mk_node_or_join_ k2 (f k2 v1' (Some v2))
          (merge ~f l1' l2) (merge ~f r1' r2)
      else (* split right tree *)
        let l2', v2', r2' = split k1 b in
        mk_node_or_join_ k1 (f k1 (Some v1) v2')
          (merge ~f l1 l2') (merge ~f r1 r2')

  (*$R
    let m1 = M.of_list [1, 1; 2, 2; 4, 4] in
    let m2 = M.of_list [1, 1; 3, 3; 4, 4; 7, 7] in
    let m = M.merge ~f:(fun k -> CCOpt.map2 (+)) m1 m2 in
    assert_bool "balanced" (M.balanced m);
    assert_equal
      ~cmp:(CCList.equal (CCPair.equal CCInt.equal CCInt.equal))
      ~printer:CCFormat.(to_string (list (pair int int)))
      [1, 2; 4, 8]
      (M.to_list m |> List.sort Stdlib.compare)
  *)

  (*$QR
    Q.(let p = list (pair small_int small_int) in pair p p) (fun (l1, l2) ->
        let l1 = _list_uniq l1 and l2 = _list_uniq l2 in
        let m1 = M.of_list l1 and m2 = M.of_list l2  in
        let m = M.merge ~f:(fun _ v1 v2 -> match v1 with
          | None -> v2 | Some _ as r -> r) m1 m2 in
        List.for_all (fun (k,v) -> M.get_exn k m = v) l1  &&
        List.for_all (fun (k,v) -> M.mem k m1 || M.get_exn k m = v) l2)
  *)

  let cardinal m = fold ~f:(fun acc _ _ -> acc+1) ~x:0 m

  let add_list m l = List.fold_left (fun acc (k,v) -> add k v acc) m l

  let of_list l = add_list empty l

  let to_list m = fold ~f:(fun acc k v -> (k,v) :: acc) ~x:[] m

  let add_iter m seq =
    let m = ref m in
    seq (fun (k,v) -> m := add k v !m);
    !m

  let of_iter s = add_iter empty s

  let to_iter m yield = iter ~f:(fun k v -> yield (k,v)) m

  let rec add_gen m g = match g() with
    | None -> m
    | Some (k,v) -> add_gen (add k v m) g

  let of_gen g = add_gen empty g

  let to_gen m =
    let st = Stack.create () in
    Stack.push m st;
    let rec next() =
      if Stack.is_empty st then None
      else match Stack.pop st with
        | E -> next ()
        | N (k, v, l, r, _) ->
          Stack.push r st;
          Stack.push l st;
          Some (k,v)
    in next

  let pp pp_k pp_v fmt m =
    let start = "[" and stop = "]" and arrow = "->" and sep = ","in
    Format.pp_print_string fmt start;
    let first = ref true in
    iter m
      ~f:(fun k v ->
        if !first then first := false else Format.pp_print_string fmt sep;
        pp_k fmt k;
        Format.pp_print_string fmt arrow;
        pp_v fmt v;
        Format.pp_print_cut fmt ()
      );
    Format.pp_print_string fmt stop
end

module Make(X : ORD) = MakeFull(struct
    include X
    let weight _ = 1
  end)
