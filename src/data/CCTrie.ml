
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Prefix Tree} *)

type 'a sequence = ('a -> unit) -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Signatures} *)

(** {6 A Composite Word}

    Words are made of characters, who belong to a total order *)

module type WORD = sig
  type t
  type char_

  val compare : char_ -> char_ -> int
  val to_seq : t -> char_ sequence
  val of_list : char_ list -> t
end

module type S = sig
  type char_
  type key

  type 'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val add : key -> 'a -> 'a t -> 'a t
  (** Add a binding to the trie (possibly erasing the previous one) *)

  val remove : key -> 'a t -> 'a t
  (** Remove the key, if present *)

  val find : key -> 'a t -> 'a option
  (** Find the value associated with the key, if any *)

  val find_exn : key -> 'a t -> 'a
  (** Same as {!find} but can fail.
      @raise Not_found if the key is not present *)

  val longest_prefix : key -> 'a t -> key
  (** [longest_prefix k m] finds the longest prefix of [k] that leads to
      at least one path in [m] (it does not mean that the prefix is bound to
      a value.

      Example: if [m] has keys "abc0" and "abcd", then [longest_prefix "abc2" m]
      will return "abc"

      @since 0.17 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** Update the binding for the given key. The function is given
      [None] if the key is absent, or [Some v] if [key] is bound to [v];
      if it returns [None] the key is removed, otherwise it
      returns [Some y] and [key] becomes bound to [y] *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on key/value bindings. Will use {!WORD.of_list} to rebuild keys. *)

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map values, giving both key and value. Will use {!WORD.of_list} to rebuild keys.
      @since 0.17 *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map values, giving only the value.
      @since 0.17  *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Same as {!fold}, but for effectful functions *)

  val fold_values : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** More efficient version of {!fold}, that doesn't keep keys *)

  val iter_values : ('a -> unit) -> 'a t -> unit

  val merge : ('a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  (** Merge two tries together. The function is used in
      case of conflicts, when a key belongs to both tries *)

  val size : _ t -> int
  (** Number of bindings *)

  (** {6 Conversions} *)

  val to_list : 'a t -> (key * 'a) list

  val of_list : (key * 'a) list -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence

  val of_seq : (key * 'a) sequence -> 'a t

  val to_seq_values : 'a t -> 'a sequence

  val to_tree : 'a t -> [`Char of char_ | `Val of 'a | `Switch] ktree

  (** {6 Ranges} *)

  val above : key -> 'a t -> (key * 'a) sequence
  (** All bindings whose key is bigger or equal to the given key, in
      ascending order *)

  val below : key -> 'a t -> (key * 'a) sequence
  (** All bindings whose key is smaller or equal to the given key,
      in decreasing order *)

  (**/**)
  val check_invariants: _ t -> bool
  (**/**)
end

(*$inject
  module T = MakeList(CCInt)
  module S = String

  let l1 = [ [1;2], "12"; [1], "1"; [2;1], "21"; [1;2;3], "123"; [], "[]" ]
  let t1 = T.of_list l1

  let small_l l = List.fold_left (fun acc (k,v) -> List.length k+acc) 0 l

  let s1 = String.of_list ["cat", 1; "catogan", 2; "foo", 3]
*)

(*$T
  String.of_list ["a", 1; "b", 2] |> String.size = 2
  String.of_list ["a", 1; "b", 2; "a", 3] |> String.size = 2
  String.of_list ["a", 1; "b", 2] |> String.find_exn "a" = 1
  String.of_list ["a", 1; "b", 2] |> String.find_exn "b" = 2
  String.of_list ["a", 1; "b", 2] |> String.find "c" = None

  s1 |> String.find_exn "cat" = 1
  s1 |> String.find_exn "catogan" = 2
  s1 |> String.find_exn "foo" = 3
  s1 |> String.find "cato" = None
*)


module Make(W : WORD)
  : S with type char_ = W.char_ and type key = W.t
= struct
  type char_ = W.char_
  type key = W.t

  module M = Map.Make(struct
      type t = char_
      let compare = W.compare
    end)

  type 'a t =
    | Empty
    | Cons of char_ * 'a t  (* simple case *)
    | Node of 'a option * 'a t M.t

  (* invariants:
     - for Path(l,t) l is never empty
     - for Node (None,map) map always has at least 2 elements
     - for Node (Some _,map) map can be anything *)

  let empty = Empty

  let _invariant = function
    | Node (None, map) when M.is_empty map -> false
    | _ -> true

  let rec check_invariants = function
    | Empty -> true
    | Cons (_, t) -> check_invariants t
    | Node (None, map) when M.is_empty map -> false
    | Node (_, map) ->
      M.for_all (fun _ v -> check_invariants v) map

  let is_empty = function
    | Empty -> true
    | _ -> false

  let _id x = x

  (* fold [f] on [seq] with accumulator [acc], and call [finish]
     on the accumulator once [seq] is exhausted *)
  let _fold_seq_and_then f ~finish acc seq =
    let acc = ref acc in
    seq (fun x -> acc := f !acc x);
    finish !acc

  let _filter_map_seq f seq k =
    seq (fun x -> match f x with
      | None -> ()
      | Some y -> k y)

  let _seq_map f seq k = seq (fun x -> k (f x))

  let _seq_append_list_rev l seq =
    let l = ref l in
    seq (fun x -> l := x :: !l);
    !l

  let _seq_append_list l seq =
    List.rev_append (_seq_append_list_rev [] seq) l

  let seq_of_map map k =
    M.iter (fun key v -> k (key,v)) map

  (* return common prefix, and disjoint suffixes *)
  let rec _merge_lists l1 l2 = match l1, l2 with
    | [], _
    | _, [] -> [], l1, l2
    | c1::l1', c2::l2' ->
      if W.compare c1 c2 = 0
      then
        let pre, rest1, rest2 = _merge_lists l1' l2' in
        c1::pre, rest1, rest2
      else
        [], l1, l2


  (* sub-tree t prefixed with c *)
  let _cons c t = Cons (c, t)
  (* build a Node value *)
  let _mk_node value map = match value with
    | Some _ -> Node (value, map)
    | None ->
      if M.is_empty map then Empty
      else
      if M.cardinal map = 1
      then
        let c, sub = M.min_binding map in
        _cons c sub
      else Node (value,map)

  (* remove key [c] from [t] *)
  let _remove c t = match t with
    | Empty -> t
    | Cons (c', _) ->
      if W.compare c c' = 0
      then Empty
      else t
    | Node (value, map) ->
      if M.mem c map
      then
        let map' = M.remove c map in
        _mk_node value map'
      else t

  let update key f t =
    (* first arg: current subtree and rebuild function; [c]: current char *)
    let goto (t, rebuild) c =
      match t with
        | Empty -> empty, fun t -> rebuild (_cons c t)
        | Cons (c', t') ->
          if W.compare c c' = 0
          then t', (fun t -> rebuild (_cons c t))
          else
            let rebuild' new_child =
              rebuild (
                if is_empty new_child then t
                else
                  let map = M.singleton c new_child in
                  let map = M.add c' t' map in
                  _mk_node None map
              ) in
            empty, rebuild'
        | Node (value, map) ->
          try
            let t' = M.find c map in
            (* rebuild: we modify [t], so we put the new version in [map]
               if it's not empty, and make the node again *)
            let rebuild' new_child =
              rebuild (
                if is_empty new_child
                then _mk_node value (M.remove c map)
                else _mk_node value (M.add c new_child map)
              )
            in
            t', rebuild'
          with Not_found ->
            let rebuild' new_child =
              if is_empty new_child
              then rebuild t (* ignore *)
              else
                let map' = M.add c new_child map in
                rebuild (_mk_node value map')
            in
            empty, rebuild'
    in
    let finish (t,rebuild) = match t with
      | Empty -> rebuild (_mk_node (f None) M.empty)
      | Cons (c, t') ->
        rebuild
          (match f None with
            | None -> t
            | Some _ as v -> _mk_node v (M.singleton c t')
          )
      | Node (value, map) ->
        let value' = f value in
        rebuild (_mk_node value' map)
    in
    let word = W.to_seq key in
    _fold_seq_and_then goto ~finish (t, _id) word

  let add k v t = update k (fun _ -> Some v) t

  let remove k t = update k (fun _ -> None) t

  (*$T
    T.add [3] "3" t1 |> T.find_exn [3] = "3"
    T.add [3] "3" t1 |> T.find_exn [1;2] = "12"
    T.remove [1;2] t1 |> T.find [1;2] = None
    T.remove [1;2] t1 |> T.find [1] = Some "1"
    T.remove [1;2] t1 |> T.find [] = Some "[]"
  *)

  let find_exn k t =
    (* at subtree [t], and character [c] *)
    let goto t c = match t with
      | Empty -> raise Not_found
      | Cons (c', t') ->
        if W.compare c c' = 0
        then t'
        else raise Not_found
      | Node (_, map) -> M.find c map
    and finish t = match t with
      | Node (Some v, _) -> v
      | _ -> raise Not_found
    in
    let word = W.to_seq k in
    _fold_seq_and_then goto ~finish t word

  let find k t =
    try Some (find_exn k t)
    with Not_found -> None

  type 'a difflist = 'a list -> 'a list

  let _difflist_add
    : 'a difflist -> 'a -> 'a difflist
    = fun f x -> fun l' -> f (x :: l')

  let longest_prefix k t =
    (* at subtree [t], and character [c] *)
    let goto (t,prefix) c = match t with
      | Empty -> Empty, prefix
      | Cons (c', t') ->
        if W.compare c c' = 0
        then t', _difflist_add prefix c
        else Empty, prefix
      | Node (_, map) ->
        try
          let t' = M.find c map in
          t', _difflist_add prefix c
        with Not_found -> Empty, prefix
    and finish (_,prefix) =
      W.of_list (prefix [])
    in
    let word = W.to_seq k in
    _fold_seq_and_then goto ~finish (t,_id) word

  (*$= & ~printer:CCFun.id
    "ca" (String.longest_prefix "carte" s1)
    "" (String.longest_prefix "yolo" s1)
    "cat" (String.longest_prefix "cat" s1)
    "catogan" (String.longest_prefix "catogan" s1)
  *)

  (*$Q
    Q.(pair (list (pair (printable_string_of_size Gen.(0 -- 30)) int)) printable_string) (fun (l,s) -> \
      let m = String.of_list l in \
      let s' = String.longest_prefix s m in \
      CCString.prefix ~pre:s' s)
  *)

  (* fold that also keeps the path from the root, so as to provide the list
      of chars that lead to a value. The path is a difference list, ie
      a function that prepends a list to some suffix *)
  let rec _fold f path t acc = match t with
    | Empty -> acc
    | Cons (c, t') -> _fold f (_difflist_add path c) t' acc
    | Node (v, map) ->
      let acc = match v with
        | None -> acc
        | Some v -> f acc path v
      in
      M.fold
        (fun c t' acc -> _fold f (_difflist_add path c) t' acc)
        map acc

  let fold f acc t =
    _fold
      (fun acc path v ->
         let key = W.of_list (path []) in
         f acc key v)
      _id t acc

  (*$T
    T.fold (fun acc k v -> (k,v) :: acc) [] t1 \
      |> List.sort Stdlib.compare = List.sort Stdlib.compare l1
  *)

  let mapi f t =
    let rec map_ prefix t = match t with
      | Empty -> Empty
      | Cons (c, t') -> Cons (c, map_ (_difflist_add prefix c) t')
      | Node (v, map) ->
        let v' = match v with
          | None -> None
          | Some v -> Some (f (W.of_list (prefix [])) v)
        in let map' =
             M.mapi (fun c t' ->
               let prefix' = _difflist_add prefix c in
               map_ prefix' t')
               map
        in Node (v', map')
    in map_ _id t

  (*$= & ~printer:Q.Print.(list (pair (list int) string))
    (List.map (fun (k, v) -> (k, v ^ "!")) l1 |> List.sort Stdlib.compare) \
      (T.mapi (fun k v -> v ^ "!") t1 \
        |> T.to_list |> List.sort Stdlib.compare)
  *)

  let map f t =
    let rec map_ = function
      | Empty -> Empty
      | Cons (c, t') -> Cons (c, map_ t')
      | Node (v, map) ->
        let v' = match v with
          | None -> None
          | Some v -> Some (f v)
        in let map' = M.map map_ map
        in Node (v', map')
    in map_ t
  (*$= & ~printer:Q.Print.(list (pair (list int) string))
    (List.map (fun (k, v) -> (k, v ^ "!")) l1 |> List.sort Stdlib.compare) \
      (T.map (fun v -> v ^ "!") t1 \
        |> T.to_list |> List.sort Stdlib.compare)
  *)


  let iter f t =
    _fold
      (fun () path y -> f (W.of_list (path [])) y)
      _id t ()

  let _iter_prefix ~prefix f t =
    _fold
      (fun () path y ->
         let key = W.of_list (prefix (path [])) in
         f key y)
      _id t ()

  let rec fold_values f acc t = match t with
    | Empty -> acc
    | Cons (_, t') -> fold_values f acc t'
    | Node (v, map) ->
      let acc = match v with
        | None -> acc
        | Some v -> f acc v
      in
      M.fold
        (fun _c t' acc -> fold_values f acc t')
        map acc

  let iter_values f t = fold_values (fun () x -> f x) () t

  let rec merge f t1 t2 = match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Cons (c1,t1'), Cons (c2,t2') ->
      if W.compare c1 c2 = 0
      then _cons c1 (merge f t1' t2')
      else
        let map = M.add c1 t1' M.empty in
        let map = M.add c2 t2' map in
        _mk_node None map

    | Cons (c1, t1'), Node (value, map) ->
      begin try
          (* collision *)
          let t2' = M.find c1 map in
          let new_t = merge f t1' t2' in
          let map' = if is_empty new_t
            then M.remove c1 map
            else M.add c1 new_t map
          in
          _mk_node value map'
        with Not_found ->
          (* no collision *)
          assert (not(is_empty t1'));
          Node (value, M.add c1 t1' map)
      end
    | Node _, Cons _ -> merge f t2 t1  (* previous case *)
    | Node(v1, map1), Node (v2, map2) ->
      let v = match v1, v2 with
        | None, _ -> v2
        | _, None -> v1
        | Some v1, Some v2 -> f v1 v2
      in
      let map' = M.merge
          (fun _c t1 t2 -> match t1, t2 with
             | None, None -> assert false
             | Some t, None
             | None, Some t -> Some t
             | Some t1, Some t2 ->
               let new_t = merge f t1 t2 in
               if is_empty new_t then None else Some new_t
          ) map1 map2
      in
      _mk_node v map'

  (*$QR & ~count:30
    Q.(let p = list_of_size Gen.(0--100) (pair printable_string small_int) in pair p p)
      (fun (l1,l2) ->
        let t1 = S.of_list l1 and t2 = S.of_list l2 in
        let t = S.merge (fun a _ -> Some a) t1 t2 in
        S.to_seq t |> Iter.for_all
          (fun (k,v) -> S.find k t1 = Some v || S.find k t2 = Some v) &&
        S.to_seq t1 |> Iter.for_all (fun (k,v) -> S.find k t <> None) &&
        S.to_seq t2 |> Iter.for_all (fun (k,v) -> S.find k t <> None))
  *)

  let rec size t = match t with
    | Empty -> 0
    | Cons (_, t') -> size t'
    | Node (v, map) ->
      let s = match v with None -> 0 | Some _ -> 1 in
      M.fold
        (fun _ t' acc -> size t' + acc)
        map s

  (*$T
    T.size t1 = List.length l1
  *)

  let to_list t = fold (fun acc k v -> (k,v)::acc) [] t

  let of_list l =
    List.fold_left (fun acc (k,v) -> add k v acc) empty l

  let to_seq t k = iter (fun key v -> k (key,v)) t

  let to_seq_values t k = iter_values k t

  let of_seq seq =
    _fold_seq_and_then (fun acc (k,v) -> add k v acc) ~finish:_id empty seq

  let rec to_tree t () =
    let _tree_node x l () = `Node (x,l) in
    match t with
      | Empty -> `Nil
      | Cons (c, t') -> `Node (`Char c, [to_tree t'])
      | Node (v, map) ->
        let x = match v with
          | None -> `Switch
          | Some v -> `Val v
        in
        let l = M.bindings map in
        `Node(x, List.map (fun (c,t') -> _tree_node (`Char c) [to_tree t']) l)

  (** {6 Ranges} *)

  (* stack of actions for [above] and [below] *)
  type 'a alternative =
    | Yield of 'a * char_ difflist
    | Explore of 'a t * char_ difflist

  type direction =
    | Above
    | Below

  let rec explore ~dir k alt = match alt with
    | Yield (v,prefix) -> k (W.of_list (prefix[]), v)
    | Explore (Empty, _) -> ()
    | Explore (Cons (c,t), prefix) ->
      explore ~dir k (Explore (t, _difflist_add prefix c))
    | Explore (Node (o,map), prefix) ->
      (* if above, yield value now *)
      begin match o, dir with
        | Some v, Above -> k (W.of_list (prefix[]), v)
        | _ -> ()
      end;
      let seq = seq_of_map map in
      let seq = _seq_map (fun (c,t') -> Explore (t', _difflist_add prefix c)) seq in
      let l' = match o, dir with
        | _, Above -> _seq_append_list [] seq
        | None, Below -> _seq_append_list_rev [] seq
        | Some v, Below ->
          _seq_append_list_rev [Yield (v, prefix)] seq
      in
      List.iter (explore ~dir k) l'

  let _list_eq l1 l2 =
    try List.for_all2 (fun x y -> W.compare x y = 0) l1 l2
    with Invalid_argument _ -> false

  let _key_to_list key =
    List.rev (_seq_append_list_rev [] (W.to_seq key))

  (* range above (if [above = true]) or below a threshold .
     [p c c'] must return [true] if [c'], in the tree, meets some criterion
     w.r.t [c] which is a part of the key. *)
  let _half_range ~dir ~p key t k =
    (* at subtree [cur = Some (t,trail)] or [None], alternatives above
        [alternatives], and char [c] in [key]. *)
    let on_char (cur, alternatives) c =
      match cur with
        | None -> (None, alternatives)
        | Some (Empty,_) -> (None, alternatives)
        | Some (Cons (c', t'), trail) ->
          if W.compare c c' = 0
          then Some (t', _difflist_add trail c), alternatives
          else None, alternatives
        | Some (Node (o, map), trail) ->
          (* if [dir=Below], [o]'s key is below [key] and the other
             alternatives in [map] *)
          let alternatives = match o, dir with
            | Some v, Below -> Yield (v, trail) :: alternatives
            | _ -> alternatives
          in
          let alternatives =
            let seq = seq_of_map map in
            let seq = _filter_map_seq
                (fun (c', t') ->
                   if p ~cur:c ~other:c'
                   then Some (Explore (t', _difflist_add trail c'))
                   else None)
                seq
            in
            (* ordering:
               - Above: explore alternatives in increasing order
               - Below: explore alternatives in decreasing order *)
            match dir with
              | Above -> _seq_append_list alternatives seq
              | Below -> _seq_append_list_rev alternatives seq
          in
          begin
            try
              let t' = M.find c map in
              Some (t', _difflist_add trail c), alternatives
            with Not_found ->
              None, alternatives
          end

    (* run through the current path (if any) and alternatives *)
    and finish (cur,alternatives) =
      begin match cur, dir with
        | Some (t, prefix), Above ->
          (* subtree prefixed by input key, therefore above key *)
          _iter_prefix ~prefix (fun key' v -> k (key', v)) t
        | Some (Node (Some v, _), prefix), Below ->
          (* yield the value for key *)
          assert (_list_eq (prefix []) (_key_to_list key));
          k (key, v)
        | Some _, _
        | None, _ -> ()
      end;
      List.iter (explore ~dir k) alternatives
    in
    let word = W.to_seq key in
    _fold_seq_and_then on_char ~finish (Some(t,_id), []) word

  let above key t =
    _half_range ~dir:Above ~p:(fun ~cur ~other -> W.compare cur other < 0) key t

  let below key t =
    _half_range ~dir:Below ~p:(fun ~cur ~other -> W.compare cur other > 0) key t

  (*$= & ~printer:CCFormat.(to_string (list (pair (list int) string)))
    [ [1], "1"; [1;2], "12"; [1;2;3], "123"; [2;1], "21" ] \
      (T.above [1] t1 |> Iter.to_list)
    [ [1;2], "12"; [1;2;3], "123"; [2;1], "21" ] \
      (T.above [1;1] t1 |> Iter.to_list)
    [ [1;2], "12"; [1], "1"; [], "[]" ] \
      (T.below [1;2] t1 |> Iter.to_list)
    [ [1], "1"; [], "[]" ] \
      (T.below [1;1] t1 |> Iter.to_list)
  *)

  (* NOTE: Regression test. See #158 *)
  (*$T
    let module TPoly = Make (struct \
        type t = (unit -> char) list \
        type char_ = char \
        let compare = compare \
        let to_seq a k = List.iter (fun c -> k (c ())) a \
        let of_list l = List.map (fun c -> (fun () -> c)) l \
      end) \
    in \
    let trie = TPoly.of_list [[fun () -> 'a'], 1; [fun () -> 'b'], 2] in \
    ignore (TPoly.below [fun () -> 'a'] trie |> Iter.to_list); \
    true
  *)

  (*$Q & ~count:30
    Q.(list_of_size Gen.(0--100) (pair printable_string small_int)) (fun l -> \
      let t = S.of_list l in \
      S.check_invariants t)
  *)

  (*$inject
    let rec sorted ~rev = function
      | [] | [_] -> true
      | x :: ((y ::_) as tl) ->
        (if rev then x >= y else x <= y) && sorted ~rev tl

    let gen_str = Q.small_printable_string
  *)

  (*$Q & ~count:200
    Q.(list_of_size Gen.(1 -- 20) (pair gen_str small_int)) \
      (fun l -> let t = String.of_list l in \
        List.for_all (fun (k,_) -> \
          String.above k t |> Iter.for_all (fun (k',v) -> k' >= k)) \
          l)
    Q.(list_of_size Gen.(1 -- 20) (pair gen_str small_int)) \
      (fun l -> let t = String.of_list l in \
        List.for_all (fun (k,_) -> \
          String.below k t |> Iter.for_all (fun (k',v) -> k' <= k)) \
          l)
    Q.(list_of_size Gen.(1 -- 20) (pair gen_str small_int)) \
      (fun l -> let t = String.of_list l in \
        List.for_all (fun (k,_) -> \
          String.above k t |> Iter.to_list |> sorted ~rev:false) \
          l)
    Q.(list_of_size Gen.(1 -- 20) (pair gen_str small_int)) \
      (fun l -> let t = String.of_list l in \
        List.for_all (fun (k,_) -> \
          String.below k t |> Iter.to_list |> sorted ~rev:true) \
          l)
  *)
end

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MakeArray(X : ORDERED) = Make(struct
    type t = X.t array
    type char_ = X.t
    let compare = X.compare
    let to_seq a k = Array.iter k a
    let of_list = Array.of_list
  end)

module MakeList(X : ORDERED) = Make(struct
    type t = X.t list
    type char_ = X.t
    let compare = X.compare
    let to_seq a k = List.iter k a
    let of_list l = l
  end)

module String = Make(struct
    type t = string
    type char_ = char
    let compare = Char.compare
    let to_seq s k = String.iter k s
    let of_list l =
      let buf = Buffer.create (List.length l) in
      List.iter (fun c -> Buffer.add_char buf c) l;
      Buffer.contents buf
  end)
