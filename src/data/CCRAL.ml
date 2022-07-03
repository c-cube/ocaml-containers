(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Random-Access Lists} *)

(** A complete binary tree *)
type +'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

and +'a t =
  | Nil
  | Cons of int * 'a tree * 'a t  (** Functional array of complete trees *)

(** {2 Functions on trees} *)

(** {2 Functions on lists of trees} *)

let empty = Nil
let return x = Cons (1, Leaf x, Nil)

let is_empty = function
  | Nil -> true
  | Cons _ -> false

let rec get_exn l i =
  match l with
  | Nil -> invalid_arg "RAL.get"
  | Cons (size, t, _) when i < size -> tree_lookup_ size t i
  | Cons (size, _, l') -> get_exn l' (i - size)

and tree_lookup_ size t i =
  match t, i with
  | Leaf x, 0 -> x
  | Leaf _, _ -> invalid_arg "RAL.get"
  | Node (x, _, _), 0 -> x
  | Node (_, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size' then
      tree_lookup_ size' t1 (i - 1)
    else
      tree_lookup_ size' t2 (i - 1 - size')

let get l i = try Some (get_exn l i) with Invalid_argument _ -> None

let rec set l i v =
  match l with
  | Nil -> invalid_arg "RAL.set"
  | Cons (size, t, l') when i < size -> Cons (size, tree_update_ size t i v, l')
  | Cons (size, t, l') -> Cons (size, t, set l' (i - size) v)

and tree_update_ size t i v =
  match t, i with
  | Leaf _, 0 -> Leaf v
  | Leaf _, _ -> invalid_arg "RAL.set"
  | Node (_, t1, t2), 0 -> Node (v, t1, t2)
  | Node (x, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size' then
      Node (x, tree_update_ size' t1 (i - 1) v, t2)
    else
      Node (x, t1, tree_update_ size' t2 (i - 1 - size') v)

let cons x l =
  match l with
  | Cons (size1, t1, Cons (size2, t2, l')) when size1 = size2 ->
    Cons (1 + size1 + size2, Node (x, t1, t2), l')
  | _ -> Cons (1, Leaf x, l)

let cons' l x = cons x l

let hd l =
  match l with
  | Nil -> invalid_arg "RAL.hd"
  | Cons (_, Leaf x, _) -> x
  | Cons (_, Node (x, _, _), _) -> x

let tl l =
  match l with
  | Nil -> invalid_arg "RAL.tl"
  | Cons (_, Leaf _, l') -> l'
  | Cons (size, Node (_, t1, t2), l') ->
    let size' = size / 2 in
    Cons (size', t1, Cons (size', t2, l'))

let front l =
  match l with
  | Nil -> None
  | Cons (_, Leaf x, tl) -> Some (x, tl)
  | Cons (size, Node (x, t1, t2), l') ->
    let size' = size / 2 in
    Some (x, Cons (size', t1, Cons (size', t2, l')))

let front_exn l =
  match l with
  | Nil -> invalid_arg "RAL.front_exn"
  | Cons (_, Leaf x, tl) -> x, tl
  | Cons (size, Node (x, t1, t2), l') ->
    let size' = size / 2 in
    x, Cons (size', t1, Cons (size', t2, l'))

let rec _remove prefix l i =
  let x, l' = front_exn l in
  if i = 0 then
    List.fold_left (fun l x -> cons x l) l' prefix
  else
    _remove (x :: prefix) l' (i - 1)

let remove l i = _remove [] l i

let rec _get_and_remove_exn prefix l i =
  let x, l' = front_exn l in
  if i = 0 then
    x, List.fold_left (fun l x -> cons x l) l' prefix
  else
    _get_and_remove_exn (x :: prefix) l' (i - 1)

let get_and_remove_exn l i = _get_and_remove_exn [] l i

let rec _map_tree f t =
  match t with
  | Leaf x -> Leaf (f x)
  | Node (x, l, r) -> Node (f x, _map_tree f l, _map_tree f r)

let rec map ~f l =
  match l with
  | Nil -> Nil
  | Cons (i, t, tl) -> Cons (i, _map_tree f t, map ~f tl)

let mapi ~f l =
  let rec aux f i l =
    match l with
    | Nil -> Nil
    | Cons (size, t, tl) -> Cons (size, aux_t f ~size i t, aux f (i + size) tl)
  and aux_t f ~size i t =
    match t with
    | Leaf x -> Leaf (f i x)
    | Node (x, l, r) ->
      let x = f i x in
      let l = aux_t f ~size:(size / 2) (i + 1) l in
      Node (x, l, aux_t f ~size:(size / 2) (i + 1 + (size / 2)) r)
  in
  aux f 0 l

let rec length l =
  match l with
  | Nil -> 0
  | Cons (size, _, l') -> size + length l'

let rec iter ~f l =
  match l with
  | Nil -> ()
  | Cons (_, Leaf x, l') ->
    f x;
    iter ~f l'
  | Cons (_, t, l') ->
    iter_tree t f;
    iter ~f l'

and iter_tree t f =
  match t with
  | Leaf x -> f x
  | Node (x, t1, t2) ->
    f x;
    iter_tree t1 f;
    iter_tree t2 f

let iteri ~f l =
  let rec aux f i l =
    match l with
    | Nil -> ()
    | Cons (size, t, l') ->
      aux_t ~size f i t;
      aux f (i + size) l'
  and aux_t f ~size i t =
    match t with
    | Leaf x -> f i x
    | Node (x, l, r) ->
      f i x;
      let size' = size / 2 in
      aux_t ~size:size' f (i + 1) l;
      aux_t ~size:size' f (i + 1 + size') r
  in
  aux f 0 l

let rec fold ~f ~x:acc l =
  match l with
  | Nil -> acc
  | Cons (_, Leaf x, l') -> fold ~f ~x:(f acc x) l'
  | Cons (_, t, l') ->
    let acc' = fold_tree t acc f in
    fold ~f ~x:acc' l'

and fold_tree t acc f =
  match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = f acc x in
    let acc = fold_tree t1 acc f in
    fold_tree t2 acc f

let rec fold_rev ~f ~x:acc l =
  match l with
  | Nil -> acc
  | Cons (_, Leaf x, l') -> f (fold_rev ~f ~x:acc l') x
  | Cons (_, t, l') ->
    let acc = fold_rev ~f ~x:acc l' in
    fold_tree_rev t acc f

and fold_tree_rev t acc f =
  match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = fold_tree_rev t2 acc f in
    let acc = fold_tree_rev t1 acc f in
    f acc x

let rev_map ~f l = fold ~f:(fun acc x -> cons (f x) acc) ~x:empty l
let rev l = fold ~f:cons' ~x:empty l
let append l1 l2 = fold_rev ~f:(fun l2 x -> cons x l2) ~x:l2 l1
let append_tree_ t l = fold_tree_rev t l cons'

let filter ~f l =
  fold_rev
    ~f:(fun acc x ->
      if f x then
        cons x acc
      else
        acc)
    ~x:empty l

let filter_map ~f l =
  fold_rev ~x:empty l ~f:(fun acc x ->
      match f x with
      | None -> acc
      | Some y -> cons y acc)

let flat_map f l =
  fold_rev ~x:empty l ~f:(fun acc x ->
      let l = f x in
      append l acc)

let flatten l = fold_rev ~f:(fun acc l -> append l acc) ~x:empty l

let app funs l =
  fold_rev ~x:empty funs ~f:(fun acc f ->
      fold_rev ~x:acc l ~f:(fun acc x -> cons (f x) acc))

type 'a stack =
  | St_nil
  | St_list of 'a t * 'a stack
  | St_tree of 'a tree * 'a stack

let rec stack_to_list = function
  | St_nil -> Nil
  | St_list (l, st') -> append l (stack_to_list st')
  | St_tree (t, st') -> append_tree_ t (stack_to_list st')

let rec take n l =
  match l with
  | Nil -> Nil
  | Cons (size, t, tl) ->
    if size <= n then
      append_tree_ t (take (n - size) tl)
    else
      take_tree_ ~size n t

and take_tree_ ~size n t =
  match t with
  | _ when n = 0 -> Nil
  | Leaf x -> cons x Nil
  | Node (x, l, r) ->
    let size' = size / 2 in
    if size' <= n - 1 then
      cons x (append_tree_ l (take_tree_ ~size:size' (n - size' - 1) r))
    else
      cons x (take_tree_ ~size:size' (n - 1) l)

let take_while ~f l =
  (* st: stack of subtrees *)
  let rec aux p st =
    match st with
    | St_nil -> Nil
    | St_list (Nil, st') -> aux p st'
    | St_list (Cons (_, t, tl), st') -> aux p (St_tree (t, St_list (tl, st')))
    | St_tree (Leaf x, st') ->
      if p x then
        cons x (aux p st')
      else
        Nil
    | St_tree (Node (x, l, r), st') ->
      if p x then
        cons x (aux p (St_tree (l, St_tree (r, st'))))
      else
        Nil
  in
  aux f (St_list (l, St_nil))

(* drop [n < size] elements from [t] *)
let rec drop_tree_ ~size n t tail =
  match t with
  | _ when n = 0 -> tail
  | Leaf _ ->
    assert (n = 1);
    tail
  | Node (_, left, right) ->
    if n = 1 then
      append_tree_ left (append_tree_ right tail)
    else (
      assert (size mod 2 = 1);
      let size_sub = size / 2 in
      (* size of subtrees *)
      let n = n - 1 in
      if n = size_sub then
        append_tree_ right tail
      (* drop element and left tree *)
      else if n < size_sub then
        (* drop element and part of left tree *)
        drop_tree_ ~size:size_sub n left (append_tree_ right tail)
      else
        (* drop element, left tree, and part of right tree *)
        drop_tree_ ~size:size_sub (n - size_sub) right tail
    )

let rec drop n l =
  match l with
  | _ when n = 0 -> l
  | Nil -> Nil
  | Cons (size, t, tl) ->
    if n >= size then
      drop (n - size) tl
    else
      drop_tree_ ~size n t tl

let drop_while ~f l =
  let rec aux p st =
    match st with
    | St_nil -> Nil
    | St_list (Nil, st') -> aux p st'
    | St_list (Cons (_, t, tail), st') ->
      aux p (St_tree (t, St_list (tail, st')))
    | St_tree (Leaf x, st') ->
      if p x then
        aux p st'
      else
        cons x (stack_to_list st')
    | St_tree ((Node (x, l, r) as tree), st') ->
      if p x then
        aux p (St_tree (l, St_tree (r, st')))
      else
        append_tree_ tree (stack_to_list st')
  in
  aux f (St_list (l, St_nil))

let take_drop n l = take n l, drop n l

let equal ~eq l1 l2 =
  let rec aux ~eq l1 l2 =
    match l1, l2 with
    | Nil, Nil -> true
    | Cons (size1, t1, l1'), Cons (size2, t2, l2') ->
      size1 = size2 && aux_t ~eq t1 t2 && aux ~eq l1' l2'
    | Nil, Cons _ | Cons _, Nil -> false
  and aux_t ~eq t1 t2 =
    match t1, t2 with
    | Leaf x, Leaf y -> eq x y
    | Node (x1, l1, r1), Node (x2, l2, r2) ->
      eq x1 x2 && aux_t ~eq l1 l2 && aux_t ~eq r1 r2
    | Leaf _, Node _ | Node _, Leaf _ -> false
  in
  aux ~eq l1 l2

(** {2 Utils} *)

let make n x =
  let rec aux n acc x =
    if n <= 0 then
      acc
    else
      aux (n - 1) (cons x acc) x
  in
  aux n empty x

let repeat n l =
  let rec aux n l acc =
    if n <= 0 then
      acc
    else
      aux (n - 1) l (append l acc)
  in
  aux n l empty

let range i j =
  let rec aux i j acc =
    if i = j then
      cons i acc
    else if i < j then
      aux i (j - 1) (cons j acc)
    else
      aux i (j + 1) (cons j acc)
  in
  aux i j empty

let range_r_open_ i j =
  if i = j then
    empty
  else if i < j then
    range i (j - 1)
  else
    range i (j + 1)

(** {2 Conversions} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_list l l2 = List.fold_left (fun acc x -> cons x acc) l (List.rev l2)
let of_list l = add_list empty l
let to_list l = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] l
let add_array l a = Array.fold_right cons a l
let of_array a = add_array empty a

let to_array l =
  match l with
  | Nil -> [||]
  | Cons (_, Leaf x, _) | Cons (_, Node (x, _, _), _) ->
    let len = length l in
    let arr = Array.make len x in
    iteri ~f:(fun i x -> Array.set arr i x) l;
    arr

let of_iter s =
  let l = ref empty in
  s (fun x -> l := cons x !l);
  rev !l

let add_iter l s =
  let l1 = ref empty in
  s (fun x -> l1 := cons x !l1);
  fold ~f:(fun acc x -> cons x acc) ~x:l !l1

let to_iter l yield = iter ~f:yield l

let rec gen_iter_ f g =
  match g () with
  | None -> ()
  | Some x ->
    f x;
    gen_iter_ f g

let add_gen l g =
  let l1 = ref empty in
  gen_iter_ (fun x -> l1 := cons x !l1) g;
  fold ~f:(fun acc x -> cons x acc) ~x:l !l1

let of_gen g = add_gen empty g

let to_gen l =
  let st = Stack.create () in
  (* stack for tree *)
  let l = ref l in
  (* tail of list *)
  let rec next () =
    if Stack.is_empty st then (
      match !l with
      | Nil -> None
      | Cons (_, t, tl) ->
        l := tl;
        Stack.push t st;
        next ()
    ) else (
      match Stack.pop st with
      | Leaf x -> Some x
      | Node (x, l, r) ->
        Stack.push r st;
        Stack.push l st;
        Some x
    )
  in
  next

let rec of_list_map ~f l =
  match l with
  | [] -> empty
  | x :: l' ->
    let y = f x in
    cons y (of_list_map ~f l')

let compare ~cmp l1 l2 =
  let rec cmp_gen ~cmp g1 g2 =
    match g1 (), g2 () with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x, Some y ->
      let c = cmp x y in
      if c <> 0 then
        c
      else
        cmp_gen ~cmp g1 g2
  in
  cmp_gen ~cmp (to_gen l1) (to_gen l2)

(** {2 Infix} *)

module Infix = struct
  let ( @+ ) = cons
  let ( >>= ) l f = flat_map f l
  let ( >|= ) l f = map ~f l
  let ( <*> ) = app
  let ( -- ) = range
  let ( --^ ) = range_r_open_
end

include Infix

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let pp ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_item fmt l =
  let first = ref true in
  iter l ~f:(fun x ->
      if !first then
        first := false
      else
        pp_sep fmt ();
      pp_item fmt x);
  ()
