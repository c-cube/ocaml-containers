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

(** {2 Functions on lists of trees} *)

let empty = Nil

let return x = Cons (1, Leaf x, Nil)

let is_empty = function
  | Nil -> true
  | Cons _ -> false

let rec get_exn l i = match l with
  | Nil -> invalid_arg "RAL.get"
  | Cons (size,t, _) when i < size -> tree_lookup_ size t i
  | Cons (size,_, l') -> get_exn l' (i - size)
and tree_lookup_ size t i = match t, i with
  | Leaf x, 0 -> x
  | Leaf _, _ -> invalid_arg "RAL.get"
  | Node (x, _, _), 0 -> x
  | Node (_, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size'
    then tree_lookup_ size' t1 (i-1)
    else tree_lookup_ size' t2 (i-1-size')

let get l i = try Some (get_exn l i) with Invalid_argument _ -> None

let rec set l i v = match l with
  | Nil -> invalid_arg "RAL.set"
  | Cons (size,t, l') when i < size -> Cons (size, tree_update_ size t i v, l')
  | Cons (size,t, l') -> Cons (size, t, set l' (i - size) v)
and tree_update_ size t i v =match t, i with
  | Leaf _, 0 -> Leaf v
  | Leaf _, _ -> invalid_arg "RAL.set"
  | Node (_, t1, t2), 0 -> Node (v, t1, t2)
  | Node (x, t1, t2), _ ->
    let size' = size / 2 in
    if i <= size'
    then Node (x, tree_update_ size' t1 (i-1) v, t2)
    else Node (x, t1, tree_update_ size' t2 (i-1-size') v)

(*$Q & ~small:(CCFun.compose snd List.length)
   Q.(pair (pair small_int int) (list int)) (fun ((i,v),l) -> \
    l=[] ||  \
      (let i = (abs i) mod (List.length l) in \
      let ral = of_list l in let ral = set ral i v in \
      get_exn ral i = v))
*)

(*$Q & ~small:List.length
   Q.(list small_int) (fun l -> \
    let l1 = of_list l in \
    CCList.mapi (fun i x -> i,x) l \
      |> List.for_all (fun (i,x) -> get_exn l1 i = x))
*)

let cons x l = match l with
  | Cons (size1, t1, Cons (size2, t2, l')) when size1=size2 ->
    Cons (1 + size1 + size2, Node (x, t1, t2), l')
  | _ -> Cons (1, Leaf x, l)

let cons' l x = cons x l

let hd l = match l with
  | Nil -> invalid_arg "RAL.hd"
  | Cons (_, Leaf x, _) -> x
  | Cons (_, Node (x, _, _), _) -> x

let tl l = match l with
  | Nil -> invalid_arg "RAL.tl"
  | Cons (_, Leaf _, l') -> l'
  | Cons (size, Node (_, t1, t2), l') ->
    let size' = size / 2 in
    Cons (size', t1, Cons (size', t2, l'))

(*$T
  let l = of_list[1;2;3] in hd l = 1
  let l = of_list[1;2;3] in tl l |> to_list = [2;3]
*)

(*$Q
  Q.(list_of_size Gen.(1--100) int) (fun l -> \
    let l' =  of_list l in \
    (not (is_empty l')) ==> (equal ~eq:CCInt.equal l' (cons (hd l') (tl l'))) )
*)

let front l = match l with
  | Nil -> None
  | Cons (_, Leaf x, tl) -> Some (x, tl)
  | Cons (size, Node (x, t1, t2), l') ->
    let size' = size / 2 in
    Some (x, Cons (size', t1, Cons (size', t2, l')))

let front_exn l = match l with
  | Nil -> invalid_arg "RAL.front_exn"
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

let rec map ~f l = match l with
  | Nil -> Nil
  | Cons (i, t, tl) -> Cons (i, _map_tree f t, map ~f tl)

let mapi ~f l =
  let rec aux f i l = match l with
    | Nil -> Nil
    | Cons (size, t, tl) -> Cons (size, aux_t f ~size i t, aux f (i+size) tl)
  and aux_t f ~size i t = match t with
    | Leaf x -> Leaf (f i x)
    | Node (x, l, r) ->
      let x = f i x in
      let l = aux_t f ~size:(size/2) (i+1) l in
      Node (x, l, aux_t f ~size:(size/2) (i+1+size/2) r)
  in
  aux f 0 l

(*$QR
  Q.small_int (fun n ->
    let l = CCList.(0 -- n) in
    let l' = of_list l |> mapi ~f:(fun i x ->i,x) in
    List.mapi (fun i x->i,x) l = to_list l'
  )
*)

(*$Q
  Q.(pair (list small_int)(fun2 Observable.int Observable.int bool)) (fun (l,f) -> \
    let f = Q.Fn.apply f in \
    mapi ~f (of_list l) |> to_list = List.mapi f l )
*)

let rec length l = match l with
  | Nil -> 0
  | Cons (size,_, l') -> size + length l'

let rec iter ~f l = match l with
  | Nil -> ()
  | Cons (_, Leaf x, l') -> f x; iter ~f l'
  | Cons (_, t, l') -> iter_tree t f; iter ~f l'
and iter_tree t f = match t with
  | Leaf x -> f x
  | Node (x, t1, t2) -> f x; iter_tree t1 f; iter_tree t2 f

let iteri ~f l =
  let rec aux f i l = match l with
    | Nil -> ()
    | Cons (size, t, l') ->
      aux_t ~size f i t;
      aux f (i+size) l'
  and aux_t f ~size i t = match t with
    | Leaf x -> f i x
    | Node (x, l, r) ->
      f i x;
      let size' = size/2 in
      aux_t ~size:size' f (i+1) l;
      aux_t ~size:size' f (i+1+size') r
  in
  aux f 0 l

let rec fold ~f ~x:acc l = match l with
  | Nil -> acc
  | Cons (_, Leaf x, l') -> fold ~f ~x:(f acc x) l'
  | Cons (_, t, l') ->
    let acc' = fold_tree t acc f in
    fold ~f ~x:acc' l'
and fold_tree t acc f = match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = f acc x in
    let acc = fold_tree t1 acc f in
    fold_tree t2 acc f

let rec fold_rev ~f ~x:acc l = match l with
  | Nil -> acc
  | Cons (_, Leaf x, l') -> f (fold_rev ~f ~x:acc l') x
  | Cons (_, t, l') ->
    let acc = fold_rev ~f ~x:acc l' in
    fold_tree_rev t acc f
and fold_tree_rev t acc f = match t with
  | Leaf x -> f acc x
  | Node (x, t1, t2) ->
    let acc = fold_tree_rev t2 acc f in
    let acc = fold_tree_rev t1 acc f in
    f acc x

let rev_map ~f l = fold ~f:(fun acc x -> cons (f x) acc) ~x:empty l

(*$Q
   Q.(list int) (fun l -> \
    let f x = x+1 in \
    of_list l |> rev_map ~f |> to_list = List.rev_map f l)
*)

let rev l = fold ~f:cons' ~x:empty l

(*$Q
  Q.(list small_int) (fun l -> \
    let l = of_list l in rev (rev l) = l)
  Q.(list small_int) (fun l -> \
    let l1 = of_list l in length l1 = List.length l)
*)

let append l1 l2 = fold_rev ~f:(fun l2 x -> cons x l2) ~x:l2 l1

(*$Q & ~small:(CCPair.merge (CCFun.compose_binop List.length (+)))
  Q.(pair (list int) (list int)) (fun (l1,l2) -> \
    append (of_list l1) (of_list l2) = of_list (l1 @ l2))
*)

let append_tree_ t l = fold_tree_rev t l cons'

let filter ~f l =
  fold_rev ~f:(fun acc x -> if f x then cons x acc else acc) ~x:empty l

let filter_map ~f l =
  fold_rev ~x:empty l
    ~f:(fun acc x -> match f x with
      | None -> acc
      | Some y -> cons y acc
    )

(*$T
  of_list [1;2;3;4;5;6] |> filter ~f:(fun x -> x mod 2=0) |> to_list = [2;4;6]
*)

let flat_map f l =
  fold_rev ~x:empty l
    ~f:(fun acc x ->
      let l = f x in
      append l acc
    )

(*$Q
  Q.(pair (fun1 Observable.int (small_list int)) (small_list int)) (fun (f,l) -> \
    let f x = Q.Fn.apply f x in \
    let f' x = f x |> of_list in \
    of_list l |> flat_map f' |> to_list = CCList.(flat_map f l))
*)

let flatten l = fold_rev ~f:(fun acc l -> append l acc) ~x:empty l

(*$T
  flatten (of_list [of_list [1]; of_list []; of_list [2;3]]) = \
    of_list [1;2;3;]
*)

(*$Q
  Q.(small_list (small_list int)) (fun l -> \
    of_list l |> map ~f:of_list |> flatten |> to_list = CCList.flatten l)
*)

let app funs l =
  fold_rev ~x:empty funs
    ~f:(fun acc f ->
      fold_rev ~x:acc l
        ~f:(fun acc x -> cons (f x) acc)
    )

(*$T
  app (of_list [(+) 2; ( * ) 10]) (of_list [1;10]) |> to_list = \
    [3; 12; 10; 100]
*)

type 'a stack =
  | St_nil
  | St_list of 'a t * 'a stack
  | St_tree of 'a tree * 'a stack

let rec stack_to_list = function
  | St_nil -> Nil
  | St_list (l, st') -> append l (stack_to_list st')
  | St_tree (t, st') -> append_tree_ t (stack_to_list st')

let rec take n l = match l with
  | Nil -> Nil
  | Cons (size, t, tl) ->
    if size <= n
    then append_tree_ t (take (n-size) tl)
    else take_tree_ ~size n t
and take_tree_ ~size n t = match t with
  | _ when n=0 -> Nil
  | Leaf x -> cons x Nil
  | Node (x, l, r) ->
    let size' = size/2 in
    if size' <= n-1
    then cons x (append_tree_ l (take_tree_ ~size:size' (n-size'-1) r))
    else cons x (take_tree_ ~size:size' (n-1) l)

(*$T
  take 3 (of_list CCList.(1--10)) |> to_list = [1;2;3]
  take 5 (of_list CCList.(1--10)) |> to_list = [1;2;3;4;5]
  take 0 (of_list CCList.(1--10)) |> to_list = []
*)

(*$Q
  Q.(pair small_int (list int)) (fun (n,l) -> \
    of_list l |> take n |> to_list = CCList.take n l)
*)

let take_while ~f l =
  (* st: stack of subtrees *)
  let rec aux p st = match st with
    | St_nil -> Nil
    | St_list (Nil, st') -> aux p st'
    | St_list (Cons (_, t, tl), st') -> aux p (St_tree (t, St_list (tl, st')))
    | St_tree (Leaf x, st') ->
      if p x then cons x (aux p st') else Nil
    | St_tree (Node (x,l,r), st') ->
      if p x then cons x (aux p (St_tree (l, St_tree (r, st')))) else Nil
  in aux f (St_list (l, St_nil))

(*$Q
  Q.(list int) (fun l -> \
    let f x = x mod 7 <> 0 in \
    of_list l |> take_while ~f |> to_list = CCList.take_while f l)
  Q.(pair (fun1 Observable.int bool) (list int)) (fun (f,l) -> \
    let f x = Q.Fn.apply f x in \
    of_list l |> take_while ~f |> to_list = CCList.take_while f l)
*)

(* drop [n < size] elements from [t] *)
let rec drop_tree_ ~size n t tail = match t with
  | _ when n=0 -> tail
  | Leaf _ ->
    assert (n=1);
    tail
  | Node (_,left,right) ->
    if n=1 then append_tree_ left (append_tree_ right tail)
    else (
      assert (size mod 2 = 1);
      let size_sub = size/2 in (* size of subtrees *)
      let n = n-1 in
      if n = size_sub then (
        append_tree_ right tail (* drop element and left tree *)
      ) else if n < size_sub then (
        (* drop element and part of left tree *)
        drop_tree_ ~size:size_sub n left (append_tree_ right tail)
      ) else (
        (* drop element, left tree, and part of right tree *)
        drop_tree_ ~size:size_sub (n-size_sub) right tail
      )
    )

let rec drop n l = match l with
  | _ when n=0 -> l
  | Nil -> Nil
  | Cons (size, t, tl) ->
    if n >= size then drop (n-size) tl
    else drop_tree_ ~size n t tl

(*$T
  of_list [1;2;3] |> drop 2 |> length = 1
*)

(*$Q
  Q.(pair small_int (list int)) (fun (n,l) -> \
    of_list l |> drop n |> to_list = CCList.drop n l)
*)

let drop_while ~f l =
  let rec aux p st = match st with
    | St_nil -> Nil
    | St_list (Nil, st') -> aux p st'
    | St_list (Cons (_, t, tail), st') ->
      aux p (St_tree (t, St_list (tail, st')))
    | St_tree (Leaf x, st') ->
      if p x then aux p st' else cons x (stack_to_list st')
    | St_tree (Node (x,l,r) as tree, st') ->
      if p x
      then aux p (St_tree (l, St_tree (r, st')))
      else append_tree_ tree (stack_to_list st')
  in aux f (St_list (l, St_nil))

(*$T
  drop 3 (of_list CCList.(1--10)) |> to_list = CCList.(4--10)
  drop 5 (of_list CCList.(1--10)) |> to_list = [6;7;8;9;10]
  drop 0 (of_list CCList.(1--10)) |> to_list = CCList.(1--10)
  drop 15 (of_list CCList.(1--10)) |> to_list = []
*)

(*$Q
  Q.(list_of_size Gen.(0 -- 200) int) (fun l -> \
    let f x = x mod 10 <> 0 in \
    of_list l |> drop_while ~f |> to_list = CCList.drop_while f l)
*)

let take_drop n l = take n l, drop n l

let equal ~eq l1 l2 =
  let rec aux ~eq l1 l2 = match l1, l2 with
    | Nil, Nil -> true
    | Cons (size1, t1, l1'), Cons (size2, t2, l2') ->
      size1 = size2 && aux_t ~eq t1 t2 && aux ~eq l1' l2'
    | Nil, Cons _
    | Cons _, Nil -> false
  and aux_t ~eq t1 t2 = match t1, t2 with
    | Leaf x, Leaf y -> eq x y
    | Node (x1, l1, r1), Node (x2, l2, r2) ->
      eq x1 x2 && aux_t ~eq l1 l2 && aux_t ~eq r1 r2
    | Leaf _, Node _
    | Node _, Leaf _ -> false
  in
  aux ~eq l1 l2

(*$Q
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    equal ~eq:CCInt.equal (of_list l1) (of_list l2) = (l1=l2))
*)

(** {2 Utils} *)

let make n x =
  let rec aux n acc x =
    if n<=0 then acc else aux (n-1) (cons x acc) x
  in
  aux n empty x

let repeat n l =
  let rec aux n l acc =
    if n<=0 then acc else aux (n-1) l (append l acc)
  in
  aux n l empty


(*$Q
  Q.(pair small_int (small_list int)) (fun (n,l) -> \
    of_list l |> repeat n |> to_list = CCList.(repeat n l))
*)

let range i j =
  let rec aux i j acc =
    if i=j then cons i acc
    else if i<j
    then aux i (j-1) (cons j acc)
    else
      aux i (j+1) (cons j acc)
  in
  aux i j empty

(*$T
  range 0 3 |> to_list = [0;1;2;3]
  range 3 0 |> to_list = [3;2;1;0]
  range 17 17 |> to_list = [17]
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    range i j |> to_list = CCList.(i -- j) )
*)

let range_r_open_ i j =
  if i=j then empty
  else if i<j then range i (j-1)
  else range i (j+1)

(*$= & ~printer:CCFormat.(to_string (hbox (list int)))
  [1;2;3;4] (1 --^ 5 |> to_list)
  [5;4;3;2] (5 --^ 1 |> to_list)
  [1]       (1 --^ 2 |> to_list)
  []        (0 --^ 0 |> to_list)
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

let to_list l = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] l

(*$Q
  Q.(list int) (fun l -> to_list (of_list l) = l)
*)

let add_array l a = Array.fold_right cons a l

let of_array a = add_array empty a

let to_array l = match l with
  | Nil -> [||]
  | Cons (_, Leaf x, _)
  | Cons (_, Node (x, _,_), _) ->
    let len = length l in
    let arr = Array.make len x in
    iteri ~f:(fun i x -> Array.set arr i x) l;
    arr

(*$Q
  Q.(array int) (fun a -> \
    of_array a |> to_array = a)
*)

let of_seq s =
  let l = ref empty in
  s (fun x -> l := cons x !l);
  rev !l

let add_seq l s =
  let l1 = ref empty in
  s (fun x -> l1 := cons x !l1);
  fold ~f:(fun acc x -> cons x acc) ~x:l !l1

let to_seq l yield = iter ~f:yield l

(*$Q & ~small:List.length
  Q.(list small_int) (fun l -> \
    of_list l |> to_seq |> Iter.to_list = l)
  Q.(list small_int) (fun l -> \
    Iter.of_list l |> of_seq |> to_list = l)
*)

(*$T
  add_seq (of_list [3;4]) (Iter.of_list [1;2]) |> to_list = [1;2;3;4]
*)

let rec gen_iter_ f g = match g() with
  | None -> ()
  | Some x -> f x; gen_iter_ f g

let add_gen l g =
  let l1 = ref empty in
  gen_iter_ (fun x -> l1 := cons x !l1) g;
  fold ~f:(fun acc x -> cons x acc) ~x:l !l1

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

let rec of_list_map ~f l = match l with
  | [] -> empty
  | x::l' ->
    let y = f x in
    cons y (of_list_map ~f l')

let compare ~cmp l1 l2 =
  let rec cmp_gen ~cmp g1 g2 = match g1(), g2() with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x, Some y ->
      let c = cmp x y in
      if c<> 0 then c else cmp_gen ~cmp g1 g2
  in
  cmp_gen ~cmp (to_gen l1)(to_gen l2)

(*$Q
  Q.(pair (list int)(list int)) (fun (l1,l2) -> \
    compare ~cmp:CCInt.compare (of_list l1) (of_list l2) = (Stdlib.compare l1 l2))
*)

(** {2 Infix} *)

module Infix = struct
  let (@+) = cons
  let (>>=) l f = flat_map f l
  let (>|=) l f = map ~f l
  let (<*>) = app
  let (--) = range
  let (--^) = range_r_open_
end

include Infix

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let pp ?(sep=", ") pp_item fmt l =
  let first = ref true in
  iter l
    ~f:(fun x ->
      if !first then first := false else (
        Format.pp_print_string fmt sep;
        Format.pp_print_cut fmt ();
      );
      pp_item fmt x
    );
  ()
