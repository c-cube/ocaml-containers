
(* This file is free software, part of containers. See file "license" for more details. *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

type + 'a t = unit -> 'a node
and +'a node = 'a Seq.node =
  | Nil
  | Cons of 'a * 'a t

let nil () = Nil
let cons a b () = Cons (a,b)
let empty = nil

let singleton x () = Cons (x, nil)

let rec _forever x () = Cons (x, _forever x)

let rec _repeat n x () =
  if n<=0 then Nil else Cons (x, _repeat (n-1) x)

let repeat ?n x = match n with
  | None -> _forever x
  | Some n -> _repeat n x

(*$T
  repeat ~n:4 0 |> to_list = [0;0;0;0]
  repeat ~n:0 1 |> to_list = []
  repeat 1 |> take 20 |> to_list = (repeat ~n:20 1 |> to_list)
*)

let is_empty l = match l () with
  | Nil -> true
  | Cons _ -> false

let head_exn l = match l() with | Nil -> raise Not_found | Cons (x, _) -> x
let head l = match l() with Nil -> None | Cons (x, _) -> Some x
let tail_exn l = match l() with | Nil -> raise Not_found | Cons (_, l) -> l
let tail l = match l() with | Nil -> None | Cons (_, l) -> Some l

let rec equal eq l1 l2 = match l1(), l2() with
  | Nil, Nil -> true
  | Nil, _
  | _, Nil -> false
  | Cons (x1,l1'), Cons (x2,l2') ->
    eq x1 x2 && equal eq l1' l2'

let rec compare cmp l1 l2 = match l1(), l2() with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Cons (x1,l1'), Cons (x2,l2') ->
    let c = cmp x1 x2 in
    if c = 0 then compare cmp l1' l2' else c

let rec fold f acc res = match res () with
  | Nil -> acc
  | Cons (s, cont) -> fold f (f acc s) cont

let fold_left = fold

let rec iter f l = match l () with
  | Nil -> ()
  | Cons (x, l') -> f x; iter f l'

let iteri f l =
  let rec aux f l i = match l() with
    | Nil -> ()
    | Cons (x, l') ->
      f i x;
      aux f l' (i+1)
  in
  aux f l 0

let length l = fold (fun acc _ -> acc+1) 0 l

let rec take n (l:'a t) () =
  if n=0 then Nil
  else match l () with
    | Nil -> Nil
    | Cons (x,l') -> Cons (x, take (n-1) l')

let rec take_while p l () = match l () with
  | Nil -> Nil
  | Cons (x,l') ->
    if p x then Cons (x, take_while p l') else Nil

(*$T
  of_list [1;2;3;4] |> take_while (fun x->x < 4) |> to_list = [1;2;3]
*)

let rec drop n (l:'a t) () = match l () with
  | l' when n=0 -> l'
  | Nil -> Nil
  | Cons (_,l') -> drop (n-1) l' ()

let rec drop_while p l () = match l() with
  | Nil -> Nil
  | Cons (x,l') when p x -> drop_while p l' ()
  | Cons _ as res -> res

(*$Q
  (Q.pair (Q.list Q.small_int) Q.small_int) (fun (l,n) -> \
    let s = of_list l in let s1, s2 = take n s, drop n s in \
    append s1 s2 |> to_list = l  )
*)

let rec map f l () = match l () with
  | Nil -> Nil
  | Cons (x, l') -> Cons (f x, map f l')

(*$T
  (map ((+) 1) (1 -- 5) |> to_list) = (2 -- 6 |> to_list)
*)

let mapi f l =
  let rec aux f l i () = match l() with
    | Nil -> Nil
    | Cons (x, tl) ->
      Cons (f i x, aux f tl (i+1))
  in
  aux f l 0

(*$T
  mapi (fun i x -> i,x) (1 -- 3) |> to_list = [0, 1; 1, 2; 2, 3]
*)

let rec fmap f (l:'a t) () = match l() with
  | Nil -> Nil
  | Cons (x, l') ->
    begin match f x with
      | None -> fmap f l' ()
      | Some y -> Cons (y, fmap f l')
    end

(*$T
  fmap (fun x -> if x mod 2=0 then Some (x*3) else None) (1--10) |> to_list \
    = [6;12;18;24;30]
*)

let rec filter p l () = match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    if p x
    then Cons (x, filter p l')
    else filter p l' ()

let rec append l1 l2 () = match l1 () with
  | Nil -> l2 ()
  | Cons (x, l1') -> Cons (x, append l1' l2)

let rec cycle l () = append l (cycle l) ()

(*$T
  cycle (of_list [1;2]) |> take 5 |> to_list = [1;2;1;2;1]
  cycle (of_list [1; ~-1]) |> take 100_000 |> fold (+) 0 = 0
*)

let rec unfold f acc () = match f acc with
  | None -> Nil
  | Some (x, acc') -> Cons (x, unfold f acc')

(*$T
  let f = function  10 -> None | x -> Some (x, x+1) in \
  unfold f 0 |> to_list = [0;1;2;3;4;5;6;7;8;9]
*)

let rec flat_map f l () = match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    _flat_map_app f (f x) l' ()
and _flat_map_app f l l' () = match l () with
  | Nil -> flat_map f l' ()
  | Cons (x, tl) ->
    Cons (x, _flat_map_app f tl l')

let product_with f l1 l2 =
  let rec _next_left h1 tl1 h2 tl2 () =
    match tl1() with
      | Nil -> _next_right ~die:true h1 tl1 h2 tl2 ()
      | Cons (x, tl1') ->
        _map_list_left x h2
          (_next_right ~die:false (x::h1) tl1' h2 tl2)
          ()
  and _next_right ~die h1 tl1 h2 tl2 () =
    match tl2() with
      | Nil when die -> Nil
      | Nil -> _next_left h1 tl1 h2 tl2 ()
      | Cons (y, tl2') ->
        _map_list_right h1 y
          (_next_left h1 tl1 (y::h2) tl2')
          ()
  and _map_list_left x l kont () = match l with
    | [] -> kont()
    | y::l' -> Cons (f x y, _map_list_left x l' kont)
  and _map_list_right l y kont () = match l with
    | [] -> kont()
    | x::l' -> Cons (f x y, _map_list_right l' y kont)
  in
  _next_left [] l1 [] l2

let product l1 l2 =
  product_with (fun x y -> x,y) l1 l2

let rec group eq l () = match l() with
  | Nil -> Nil
  | Cons (x, l') ->
    Cons (cons x (take_while (eq x) l'), group eq (drop_while (eq x) l'))

(*$T
  of_list [1;1;1;2;2;3;3;1] |> group (=) |> map to_list |> to_list = \
    [[1;1;1]; [2;2]; [3;3]; [1]]
*)

let rec _uniq eq prev l () = match prev, l() with
  | _, Nil -> Nil
  | None, Cons (x, l') ->
    Cons (x, _uniq eq (Some x) l')
  | Some y, Cons (x, l') ->
    if eq x y
    then _uniq eq prev l' ()
    else Cons (x, _uniq eq (Some x) l')

let uniq eq l = _uniq eq None l

let rec filter_map f l () = match l() with
  | Nil -> Nil
  | Cons (x, l') ->
    begin match f x with
      | None -> filter_map f l' ()
      | Some y -> Cons (y, filter_map f l')
    end

let flatten l = flat_map (fun x->x) l

let range i j =
  let rec aux i j () =
    if i=j then Cons(i, nil)
    else if i<j then Cons (i, aux (i+1) j)
    else Cons (i, aux (i-1) j)
  in aux i j

(*$T
  range 0 5 |> to_list = [0;1;2;3;4;5]
  range 0 0 |> to_list = [0]
  range 5 2 |> to_list = [5;4;3;2]
*)

let (--) = range

let (--^) i j =
  if i=j then empty
  else if i<j then range i (j-1)
  else range i (j+1)

(*$T
  1 --^ 5 |> to_list = [1;2;3;4]
  5 --^ 1 |> to_list = [5;4;3;2]
  1 --^ 2 |> to_list = [1]
  0 --^ 0 |> to_list = []
*)

let rec fold2 f acc l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> acc
  | Cons(x1,l1'), Cons(x2,l2') ->
    fold2 f (f acc x1 x2) l1' l2'

let rec map2 f l1 l2 () = match l1(), l2() with
  | Nil, _
  | _, Nil -> Nil
  | Cons(x1,l1'), Cons(x2,l2') ->
    Cons (f x1 x2, map2 f l1' l2')

let rec iter2 f l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> ()
  | Cons(x1,l1'), Cons(x2,l2') ->
    f x1 x2; iter2 f l1' l2'

let rec for_all2 f l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> true
  | Cons(x1,l1'), Cons(x2,l2') ->
    f x1 x2 && for_all2 f l1' l2'

let rec exists2 f l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> false
  | Cons(x1,l1'), Cons(x2,l2') ->
    f x1 x2 || exists2 f l1' l2'

let rec merge cmp l1 l2 () = match l1(), l2() with
  | Nil, tl2 -> tl2
  | tl1, Nil -> tl1
  | Cons(x1,l1'), Cons(x2,l2') ->
    if cmp x1 x2 < 0
    then Cons (x1, merge cmp l1' l2)
    else Cons (x2, merge cmp l1 l2')

let rec zip a b () = match a(), b() with
  | Nil, _
  | _, Nil -> Nil
  | Cons (x, a'), Cons (y, b') -> Cons ((x,y), zip a' b')

let unzip l =
  let rec first l () = match l() with
    | Nil -> Nil
    | Cons ((x,_), tl) -> Cons (x, first tl)
  and second l () = match l() with
    | Nil -> Nil
    | Cons ((_, y), tl) -> Cons (y, second tl)
  in
  first l, second l

(*$Q
  Q.(list (pair int int)) (fun l -> \
    let l = of_list l in let a, b = unzip l in equal (=) l (zip a b))
*)

(** {2 Implementations} *)

let return x () = Cons (x, nil)
let pure = return
let (>>=) xs f = flat_map f xs
let (>|=) xs f = map f xs

let (<*>) fs xs = product_with (fun f x -> f x) fs xs

(** {2 Conversions} *)

let rec _to_rev_list acc l = match l() with
  | Nil -> acc
  | Cons (x,l') -> _to_rev_list (x::acc) l'

let to_rev_list l = _to_rev_list [] l

let to_list l =
  let rec direct i (l:'a t) = match l () with
    | Nil -> []
    | _ when i=0 -> List.rev (_to_rev_list [] l)
    | Cons (x, f) -> x :: direct (i-1) f
  in
  direct 200 l

let of_list l =
  let rec aux l () = match l with
    | [] -> Nil
    | x::l' -> Cons (x, aux l')
  in aux l

let of_array a =
  let rec aux a i () =
    if i=Array.length a then Nil
    else Cons (a.(i), aux a (i+1))
  in
  aux a 0

let to_array l =
  match l() with
    | Nil -> [| |]
    | Cons (x, _) ->
      let n = length l in
      let a = Array.make n x in (* need first elem to create [a] *)
      iteri
        (fun i x -> a.(i) <- x)
        l;
      a

(*$Q
   Q.(array int) (fun a -> of_array a |> to_array = a)
*)

(*$T
  of_array [| 1; 2; 3 |] |> to_list = [1;2;3]
  of_list [1;2;3] |> to_array = [| 1; 2; 3; |]
*)

let rec to_iter res k = match res () with
  | Nil -> ()
  | Cons (s, f) -> k s; to_iter f k

let to_gen l =
  let l = ref l in
  fun () ->
    match !l () with
      | Nil -> None
      | Cons (x,l') ->
        l := l';
        Some x

type 'a of_gen_state =
  | Of_gen_thunk of 'a gen
  | Of_gen_saved of 'a node

let of_gen g =
  let rec consume r () = match !r with
    | Of_gen_saved cons -> cons
    | Of_gen_thunk g ->
      begin match g() with
        | None ->
          r := Of_gen_saved Nil;
          Nil
        | Some x ->
          let tl = consume (ref (Of_gen_thunk g)) in
          let l = Cons (x, tl) in
          r := Of_gen_saved l;
          l
      end
  in
  consume (ref (Of_gen_thunk g))

(*$R
  let g = let n = ref 0 in fun () -> Some (incr n; !n) in
  let l = of_gen g in
  assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
  assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
  assert_equal [11;12] (drop 10 l |> take 2 |> to_list);
*)

let sort ~cmp l =
  let l = to_list l in
  of_list (List.sort cmp l)

let sort_uniq ~cmp l =
  let l = to_list l in
  uniq (fun x y -> cmp x y = 0) (of_list (List.sort cmp l))

type 'a memoize =
  | MemoThunk
  | MemoSave of 'a node

let rec memoize f =
  let r = ref MemoThunk in
  fun () -> match !r with
    | MemoSave l -> l
    | MemoThunk ->
      let l = match f() with
        | Nil -> Nil
        | Cons (x, tail) -> Cons (x, memoize tail)
      in
      r := MemoSave l;
      l

(*$R
  let printer = Q.Print.(list int) in
  let gen () =
    let rec l = let r = ref 0 in fun () -> incr r; Cons (!r, l) in l
  in
  let l1 = gen () in
  assert_equal ~printer [1;2;3;4] (take 4 l1 |> to_list);
  assert_equal ~printer [5;6;7;8] (take 4 l1 |> to_list);
  let l2 = gen () |> memoize in
  assert_equal ~printer [1;2;3;4] (take 4 l2 |> to_list);
  assert_equal ~printer [1;2;3;4] (take 4 l2 |> to_list);
*)


(** {2 Fair Combinations} *)

let rec interleave a b () = match a() with
  | Nil -> b ()
  | Cons (x, tail) -> Cons (x, interleave b tail)

let rec fair_flat_map f a () = match a() with
  | Nil -> Nil
  | Cons (x, tail) ->
    let y = f x in
    interleave y (fair_flat_map f tail) ()

let rec fair_app f a () = match f() with
  | Nil -> Nil
  | Cons (f1, fs) ->
    interleave (map f1 a) (fair_app fs a) ()

let (>>-) a f = fair_flat_map f a
let (<.>) f a = fair_app f a

(*$T
  interleave (of_list [1;3;5]) (of_list [2;4;6]) |> to_list = [1;2;3;4;5;6]
  fair_app (of_list [(+)1; ( * ) 3]) (of_list [1; 10]) \
    |> to_list |> List.sort Stdlib.compare = [2; 3; 11; 30]
*)

(** {2 Infix} *)

module Infix = struct
  let (>>=) = (>>=)
  let (>|=) = (>|=)
  let (<*>) = (<*>)
  let (>>-) = (>>-)
  let (<.>) = (<.>)
  let (--) = (--)
  let (--^) = (--^)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) = struct
  open M

  let map_m f l =
    let rec aux acc l = match l () with
      | Nil -> return (of_list (List.rev acc))
      | Cons (x,l') ->
        f x >>= fun x' ->
        aux (x' :: acc) l'
    in
    aux [] l

  let sequence_m l = map_m (fun x->x) l

  let rec fold_m f acc l = match l() with
    | Nil -> return acc
    | Cons (x,l') ->
      f acc x >>= fun acc' -> fold_m f acc' l'
end

(** {2 IO} *)

let pp ?(sep=",") pp_item fmt l =
  let rec pp fmt l = match l() with
    | Nil -> ()
    | Cons (x,l') ->
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      pp_item fmt x;
      pp fmt l'
  in
  match l() with
    | Nil -> ()
    | Cons (x,l') -> pp_item fmt x; pp fmt l'
