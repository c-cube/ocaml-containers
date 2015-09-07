
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Random-Access Lists} *)

(** A complete binary tree *)
type +'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

and +'a t =
  | Nil
  | Cons of int * 'a tree * 'a t
  (** Functional array of complete trees *)

(** {2 Functions on trees} *)

(* lookup [i]-th element in the tree [t], which has size [size] *)
let rec tree_lookup size t i = match t, i with
  | Leaf x, 0 -> x
  | Leaf _, _ -> raise (Invalid_argument "RAL.get: wrong index")
  | Node (x, _, _), 0 -> x
  | Node (_, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size'
      then tree_lookup size' t1 (i-1)
      else tree_lookup size' t2 (i-1-size')

(* replaces [i]-th element by [v] *)
let rec tree_update size t i v =match t, i with
  | Leaf _, 0 -> Leaf v
  | Leaf _, _ -> raise (Invalid_argument "RAL.set: wrong index")
  | Node (_, t1, t2), 0 -> Node (v, t1, t2)
  | Node (x, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size'
      then Node (x, tree_update size' t1 (i-1) v, t2)
      else Node (x, t1, tree_update size' t2 (i-1-size') v)

(** {2 Functions on lists of trees} *)

let empty = Nil

let return x = Cons (1, Leaf x, Nil)

let is_empty = function
  | Nil -> true
  | Cons _ -> false

let rec get l i = match l with
  | Nil -> raise (Invalid_argument "RAL.get: wrong index")
  | Cons (size,t, _) when i < size -> tree_lookup size t i
  | Cons (size,_, l') -> get l' (i - size)

let rec set l i v = match l with
  | Nil -> raise (Invalid_argument "RAL.set: wrong index")
  | Cons (size,t, l') when i < size -> Cons (size, tree_update size t i v, l')
  | Cons (size,t, l') -> Cons (size, t, set l' (i - size) v)

(*$Q & ~small:(CCFun.compose snd List.length)
   Q.(pair (pair small_int int) (list int)) (fun ((i,v),l) -> \
    l=[] ||  \
      (let i = (abs i) mod (List.length l) in \
      let ral = of_list l in let ral = set ral i v in \
      get ral i = v))
*)

(*$Q & ~small:List.length
   Q.(list small_int) (fun l -> \
    let l1 = of_list l in \
    CCList.Idx.mapi (fun i x -> i,x) l \
      |> List.for_all (fun (i,x) -> get l1 i = x))
*)

let cons x l = match l with
  | Cons (size1, t1, Cons (size2, t2, l')) ->
    if size1 = size2
      then Cons (1 + size1 + size2, Node (x, t1, t2), l')
      else Cons (1, Leaf x, l)
  | _ -> Cons (1, Leaf x, l)

let hd l = match l with
  | Nil -> raise (Invalid_argument "RAL.hd: empty list")
  | Cons (_, Leaf x, _) -> x
  | Cons (_, Node (x, _, _), _) -> x

let tl l = match l with
  | Nil -> raise (Invalid_argument "RAL.tl: empty list")
  | Cons (_, Leaf _, l') -> l'
  | Cons (size, Node (_, t1, t2), l') ->
    let size' = size / 2 in
    Cons (size', t1, Cons (size', t2, l'))

(*$T
  let l = of_list[1;2;3] in hd l = 1
  let l = of_list[1;2;3] in tl l |> to_list = [2;3]
*)

let front l = match l with
  | Nil -> None
  | Cons (_, Leaf x, tl) -> Some (x, tl)
  | Cons (size, Node (x, t1, t2), l') ->
    let size' = size / 2 in
    Some (x, Cons (size', t1, Cons (size', t2, l')))

let front_exn l = match l with
  | Nil -> raise (Invalid_argument "RAL.front")
  | Cons (_, Leaf x, tl) -> x, tl
  | Cons (size, Node (x, t1, t2), l') ->
    let size' = size / 2 in
    x, Cons (size', t1, Cons (size', t2, l'))

let rec _remove prefix l i =
  let x, l' = front_exn l in
  if i=0
    then List.fold_left (fun l x -> cons x l) l prefix
    else _remove (x::prefix) l' (i-1)

let remove l i = _remove [] l i

let rec _map_tree f t = match t with
  | Leaf x -> Leaf (f x)
  | Node (x, l, r) -> Node (f x, _map_tree f l, _map_tree f r)

let rec map f l = match l with
  | Nil -> Nil
  | Cons (i, t, tl) -> Cons (i, _map_tree f t, map f tl)

let rec length l = match l with
  | Nil -> 0
  | Cons (size,_, l') -> size + length l'

let rec iter f l = match l with
  | Nil -> ()
  | Cons (_, Leaf x, l') -> f x; iter f l'
  | Cons (_, t, l') -> iter_tree t f; iter f l'
and iter_tree t f = match t with
  | Leaf x -> f x
  | Node (x, t1, t2) -> f x; iter_tree t1 f; iter_tree t2 f

let rec fold f acc l = match l with
  | Nil -> acc
  | Cons (_, Leaf x, l') -> fold f (f acc x) l'
  | Cons (_, t, l') ->
    let acc' = fold_tree t acc f in
    fold f acc' l'
and fold_tree t acc f = match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = f acc x in
    let acc = fold_tree t1 acc f in
    fold_tree t2 acc f

let rec fold_rev f acc l = match l with
  | Nil -> acc
  | Cons (_, Leaf x, l') -> f (fold_rev f acc l') x
  | Cons (_, t, l') ->
    let acc = fold_rev f acc l' in
    fold_tree_rev t acc f
and fold_tree_rev t acc f = match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = fold_tree_rev t2 acc f in
    let acc = fold_tree_rev t1 acc f in
    f acc x

let rev l = fold (fun acc x -> cons x acc) empty l

(*$Q
  Q.(list small_int) (fun l -> \
    let l = of_list l in rev (rev l) = l)
  Q.(list small_int) (fun l -> \
    let l1 = of_list l in length l1 = List.length l)
*)

let append l1 l2 = fold_rev (fun l2 x -> cons x l2) l2 l1

(*$Q & ~small:(CCPair.merge (CCFun.compose_binop List.length (+)))
  Q.(pair (list int) (list int)) (fun (l1,l2) -> \
    append (of_list l1) (of_list l2) = of_list (l1 @ l2))
*)

let filter p l = fold_rev (fun acc x -> if p x then cons x acc else acc) empty l

let filter_map f l =
  fold_rev
    (fun acc x -> match f x with
      | None -> acc
      | Some y -> cons y acc
    ) empty l

(*$T
  of_list [1;2;3;4;5;6] |> filter (fun x -> x mod 2=0) |> to_list = [2;4;6]
*)

let flat_map f l =
  fold_rev
    (fun acc x ->
      let l = f x in
      append l acc
    ) empty l

let flatten l = fold_rev (fun acc l -> append l acc) empty l

(*$T
  flatten (of_list [of_list [1]; of_list []; of_list [2;3]]) = \
    of_list [1;2;3;]
*)

let app funs l =
  fold_rev
    (fun acc f ->
      fold_rev
        (fun acc x -> cons (f x) acc)
        acc l
    ) empty funs

(*$T
  app (of_list [(+) 2; ( * ) 10]) (of_list [1;10]) |> to_list = \
    [3; 12; 10; 100]
*)

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_list l l2 = List.fold_left (fun acc x -> cons x acc) l (List.rev l2)

(*$Q & ~small:(CCPair.merge (CCFun.compose_binop List.length (+)))
  Q.(pair (list small_int) (list small_int)) (fun (l1,l2) -> \
    add_list (of_list l2) l1 |> to_list = l1 @ l2)
*)

let of_list l = add_list empty l

let to_list l = fold_rev (fun acc x -> x :: acc) [] l

(*$Q
  Q.(list int) (fun l -> to_list (of_list l) = l)
  *)

let of_seq s =
  let l = ref empty in
  s (fun x -> l := cons x !l);
  rev !l

let add_seq l s =
  let l1 = ref empty in
  s (fun x -> l1 := cons x !l1);
  fold (fun acc x -> cons x acc) l !l1

let to_seq l yield = iter yield l

(*$Q & ~small:List.length
  Q.(list small_int) (fun l -> \
    of_list l |> to_seq |> Sequence.to_list = l)
  Q.(list small_int) (fun l -> \
    Sequence.of_list l |> of_seq |> to_list = l)
*)

(*$T
  add_seq (of_list [3;4]) (Sequence.of_list [1;2]) |> to_list = [1;2;3;4]
*)

let rec gen_iter_ f g = match g() with
  | None -> ()
  | Some x -> f x; gen_iter_ f g

let add_gen l g =
  let l1 = ref empty in
  gen_iter_ (fun x -> l1 := cons x !l1) g;
  fold (fun acc x -> cons x acc) l !l1

let of_gen g = add_gen empty g

let to_gen l =
  let st = Stack.create() in (* stack for tree *)
  let l = ref l in (* tail of list *)
  let rec next () =
    if Stack.is_empty st
    then match !l with
    | Nil -> None
    | Cons (_, t, tl) ->
        l := tl;
        Stack.push t st;
        next()
    else match Stack.pop st with
      | Leaf x -> Some x
      | Node (x, l, r) ->
          Stack.push r st;
          Stack.push l st;
          Some x
  in
  next

(*$Q & ~small:List.length
  Q.(list small_int) (fun l -> of_list l |> to_gen |> Gen.to_list = l)
  Q.(list small_int) (fun l -> \
    Gen.of_list l |> of_gen |> to_list = l)
*)

let rec of_list_map f l = match l with
  | [] -> empty
  | x::l' ->
      let y = f x in
      cons y (of_list_map f l')

(** {2 Infix} *)

module Infix = struct
  let (>>=) l f = flat_map f l
  let (>|=) l f = map f l
  let (<*>) = app
end

include Infix

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let print ?(sep=", ") pp_item fmt l =
  let first = ref true in
  iter
    (fun x ->
      if !first then first := false else (
        Format.pp_print_string fmt sep;
        Format.pp_print_cut fmt ();
      );
      pp_item fmt x
    ) l;
  ()

