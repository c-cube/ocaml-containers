
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional queues (fifo)} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a equal = 'a -> 'a -> bool
type 'a printer = Format.formatter -> 'a -> unit

(*$inject
  let pp_ilist = CCFormat.(to_string (list int))
*)

(** {2 Basics} *)

[@@@warning "-37"]

type zero = Zero
type 'x succ = Succ
type one = zero succ
type two = zero succ succ
type three = zero succ succ succ

type (+'a, 'l) digit =
  | Zero : ('a, zero) digit
  | One : 'a -> ('a, one) digit
  | Two : 'a * 'a -> ('a, two) digit
  | Three : 'a * 'a * 'a -> ('a, three) digit

(* store the size in deep version *)
type +'a t =
  | Shallow : ('a, _) digit -> 'a t
  | Deep : int * ('a, _ succ) digit * ('a * 'a) t lazy_t * ('a, _ succ) digit -> 'a t

let empty : type a. a t = Shallow Zero

(*$R
  let q = empty in
  OUnit.assert_bool "is_empty" (is_empty q)
*)

exception Empty

let _empty = Shallow Zero
let _single x = Shallow (One x)
let _double x y = Shallow (Two (x,y))
let _deep
  : type l0 l1. int -> ('a, l0 succ) digit -> ('a * 'a) t lazy_t -> ('a, l1 succ) digit -> 'a t
  = fun n hd middle tl ->
  Deep (n, hd, middle, tl)

let is_empty = function
  | Shallow Zero -> true
  | _ -> false

let singleton x = _single x
let doubleton x y = _double x y

let rec cons : type a. a -> a t -> a t
  = fun x q -> match q with
    | Shallow Zero -> _single x
    | Shallow (One y) -> Shallow (Two (x,y))
    | Shallow (Two (y,z)) -> Shallow (Three (x,y,z))
    | Shallow (Three (y,z,z')) ->
      _deep 4 (Two (x,y)) (lazy _empty) (Two (z,z'))
    | Deep (n,One y, middle, tl) -> _deep (n+1) (Two (x,y)) middle tl
    | Deep (n,Two (y,z), middle, tl) -> _deep (n+1)(Three (x,y,z)) middle tl
    | Deep (n,Three (y,z,z'), lazy q', tail) ->
      _deep (n+1) (Two (x,y)) (lazy (cons (z,z') q')) tail

(*$Q
  (Q.pair Q.int (Q.list Q.int)) (fun (x,l) -> \
    cons x (of_list l) |> to_list = x::l)
*)

let rec snoc : type a. a t -> a -> a t
  = fun q x -> match q with
    | Shallow Zero -> _single x
    | Shallow (One y) -> Shallow (Two (y,x))
    | Shallow (Two (y,z)) -> Shallow (Three (y,z,x))
    | Shallow (Three (y,z,z')) ->
      _deep 4 (Two (y,z)) (lazy _empty) (Two (z',x))
    | Deep (n,hd, middle, One y) -> _deep (n+1) hd middle (Two(y,x))
    | Deep (n,hd, middle, Two (y,z)) -> _deep (n+1) hd middle (Three(y,z,x))
    | Deep (n,hd, lazy q', Three (y,z,z')) ->
      _deep (n+1) hd (lazy (snoc q' (y,z))) (Two(z',x))

(*$Q
  (Q.pair Q.int (Q.list Q.int)) (fun (x,l) -> \
    snoc (of_list l) x |> to_list = l @ [x])
*)

(*$R
  let q = List.fold_left snoc empty [1;2;3;4;5] in
  let q = tail q in
  let q = List.fold_left snoc q [6;7;8] in
  let l = Iter.to_list (to_seq q) in
  OUnit.assert_equal ~printer:pp_ilist [2;3;4;5;6;7;8] l
*)

let rec take_front_exn : 'a. 'a t -> ('a *'a t)
  = fun q -> match q with
    | Shallow Zero -> raise Empty
    | Shallow (One x) -> x, empty
    | Shallow (Two (x,y)) -> x, Shallow (One y)
    | Shallow (Three (x,y,z)) -> x, Shallow (Two (y,z))
    | Deep (n,One x, lazy q', tail) ->
      if is_empty q'
      then x, Shallow tail
      else
        let (y,z), q' = take_front_exn q' in
        x, _deep (n-1)(Two (y,z)) (Lazy.from_val q') tail
    | Deep (n,Two (x,y), middle, tail) ->
      x, _deep (n-1) (One y) middle tail
    | Deep (n,Three (x,y,z), middle, tail) ->
      x, _deep (n-1) (Two(y,z)) middle tail

(*$Q
  (Q.pair Q.int (Q.list Q.int)) (fun (x,l) -> \
    let x', q = cons x (of_list l) |> take_front_exn in \
    x'=x && to_list q = l)
*)

(*$R
  let q = of_list [1;2;3;4] in
  let x, q = take_front_exn q in
  OUnit.assert_equal 1 x;
  let q = List.fold_left snoc q [5;6;7] in
  OUnit.assert_equal 2 (first_exn q);
  let x, q = take_front_exn q in
  OUnit.assert_equal 2 x;
*)

let take_front q =
  try Some (take_front_exn q)
  with Empty -> None

(*$T
  take_front empty = None
*)

let take_front_l n q =
  if n<0 then (
    invalid_arg "take_back_l: cannot take negative number of arguments"
  );
  let rec aux acc q n =
    if n=0 || is_empty q then List.rev acc, q
    else
      let x,q' = take_front_exn q in
      aux (x::acc) q' (n-1)
  in aux [] q n

(*$T
  let l, q = take_front_l 5 (1 -- 10) in \
  l = [1;2;3;4;5] && to_list q = [6;7;8;9;10]
*)

let take_front_while p q =
  let rec aux acc q =
    if is_empty q then List.rev acc, q
    else
      let x,q' = take_front_exn q in
      if p x then aux (x::acc) q' else List.rev acc, q
  in aux [] q

(*$T
  take_front_while (fun x-> x<5) (1 -- 10) |> fst = [1;2;3;4]
*)

let rec take_back_exn : 'a. 'a t -> 'a t * 'a
  = fun q -> match q with
    | Shallow Zero -> raise Empty
    | Shallow (One x) -> empty, x
    | Shallow (Two (x,y)) -> _single x, y
    | Shallow (Three (x,y,z)) -> Shallow (Two(x,y)), z
    | Deep (n, hd, lazy q', One x) ->
      if is_empty q'
      then Shallow hd, x
      else
        let q'', (y,z) = take_back_exn q' in
        _deep (n-1) hd (Lazy.from_val q'') (Two (y,z)), x
    | Deep (n, hd, middle, Two(x,y)) -> _deep (n-1) hd middle (One x), y
    | Deep (n, hd, middle, Three(x,y,z)) -> _deep (n-1) hd middle (Two (x,y)), z

(*$Q
  (Q.pair Q.int (Q.list Q.int)) (fun (x,l) -> \
    let q,x' = snoc (of_list l) x |> take_back_exn in \
    x'=x && to_list q = l)
*)

let take_back q =
  try Some (take_back_exn q)
  with Empty -> None

(*$T
  take_back empty = None
*)

let take_back_l n q =
  if n<0 then (
    invalid_arg "take_back_l: cannot take negative number of arguments"
  );
  let rec aux acc q n =
    if n=0 || is_empty q then q, acc
    else
      let q',x = take_back_exn q in
      aux (x::acc) q' (n-1)
  in aux [] q n

let take_back_while p q =
  let rec aux acc q =
    if is_empty q then q, acc
    else
      let q',x = take_back_exn q in
      if p x then aux (x::acc) q' else q, acc
  in aux [] q

(** {2 Individual extraction} *)

let first q =
  try Some (fst (take_front_exn q))
  with Empty -> None

let first_exn q = fst (take_front_exn q)

let last q =
  try Some (snd (take_back_exn q))
  with Empty -> None

let last_exn q = snd (take_back_exn q)

let _size_digit : type l. ('a, l) digit -> int = function
  | Zero -> 0
  | One _ -> 1
  | Two _ -> 2
  | Three _ -> 3

let size : 'a. 'a t -> int
  = function
    | Shallow d -> _size_digit d
    | Deep (n, _, _, _) -> n

(*$Q
  (Q.list Q.int) (fun l -> \
    size (of_list l) = List.length l)
*)

let _nth_digit : type l. int -> ('a, l) digit -> 'a = fun i d -> match i, d with
  | _, Zero -> raise Not_found
  | 0, One x -> x
  | 0, Two (x,_) -> x
  | 1, Two (_,x) -> x
  | 0, Three (x,_,_) -> x
  | 1, Three (_,x,_) -> x
  | 2, Three (_,_,x) -> x
  | _, _ -> raise Not_found

let rec nth_exn : 'a. int -> 'a t -> 'a
  = fun i q -> match i, q with
    | _, Shallow Zero -> raise Not_found
    | 0, Shallow (One x) -> x
    | 0, Shallow (Two (x,_)) -> x
    | 1, Shallow (Two (_,x)) -> x
    | 0, Shallow (Three (x,_,_)) -> x
    | 1, Shallow (Three (_,x,_)) -> x
    | 2, Shallow (Three (_,_,x)) -> x
    | _, Shallow _ -> raise Not_found
    | _, Deep (_, l, q, r) ->
      if i<_size_digit l
      then _nth_digit i l
      else
        let i' = i - _size_digit l in
        let q' = Lazy.force q in
        if i'<2*size q'
        then
          let (x,y) = nth_exn (i'/2) q' in
          if i' mod 2 = 0 then x else y
        else
          _nth_digit (i'-2*size q') r

(*$T
  let l = CCList.(0--100) in let q = of_list l in \
    List.map (fun i->nth_exn i q) l = l
*)

let nth i q =
  try Some (nth_exn i q)
  with Failure _ -> None

(*$Q & ~count:30
  (Q.list Q.int) (fun l -> \
   let len = List.length l in let idx = CCList.(0 -- (len - 1)) in \
   let q = of_list l in \
   l = [] || List.for_all (fun i -> nth i q = Some (List.nth l i)) idx)
*)

let init q =
  try fst (take_back_exn q)
  with Empty -> q

(*$Q
  (Q.list Q.int) (fun l -> \
    l = [] || (of_list l |> init |> to_list = List.rev (List.tl (List.rev l))))
*)

let tail q =
  try snd (take_front_exn q)
  with Empty -> q

(*$Q
  (Q.list Q.int) (fun l -> \
    l = [] || (of_list l |> tail |> to_list = List.tl l))
*)

let add_seq_front seq q =
  let l = ref [] in
  (* reversed seq *)
  seq (fun x -> l := x :: !l);
  List.fold_left (fun q x -> cons x q) q !l

(*$Q
  Q.(pair (list int) (list int)) (fun (l1, l2) -> \
    add_seq_front (Iter.of_list l1) (of_list l2) |> to_list = l1 @ l2)
*)

let add_seq_back q seq =
  let q = ref q in
  seq (fun x -> q := snoc !q x);
  !q

let _digit_to_seq : type l. ('a, l) digit -> 'a sequence = fun d k -> match d with
  | Zero -> ()
  | One x -> k x
  | Two (x,y) -> k x; k y
  | Three (x,y,z) -> k x; k y; k z

let rec to_seq : 'a. 'a t -> 'a sequence
  = fun q k -> match q with
    | Shallow d -> _digit_to_seq d k
    | Deep (_, hd, lazy q', tail) ->
      _digit_to_seq hd k;
      to_seq q' (fun (x,y) -> k x; k y);
      _digit_to_seq tail k

(*$Q
  (Q.list Q.int) (fun l -> \
    of_list l |> to_seq |> Iter.to_list = l)
*)

let append q1 q2 =
  match q1, q2 with
    | Shallow Zero, _ -> q2
    | _, Shallow Zero -> q1
    | _ -> add_seq_back q1 (to_seq q2)

(*$Q
  (Q.pair (Q.list Q.int)(Q.list Q.int)) (fun (l1,l2) -> \
    append (of_list l1) (of_list l2) |> to_list = l1 @ l2)
*)

(*$R
  let q1 = of_seq (Iter.of_list [1;2;3;4]) in
  let q2 = of_seq (Iter.of_list [5;6;7;8]) in
  let q = append q1 q2 in
  let l = Iter.to_list (to_seq q) in
  OUnit.assert_equal ~printer:pp_ilist [1;2;3;4;5;6;7;8] l
*)

let _map_digit : type l. ('a -> 'b) -> ('a, l) digit -> ('b, l) digit = fun f d -> match d with
  | Zero -> Zero
  | One x -> One (f x)
  | Two (x,y) -> Two (f x, f y)
  | Three (x,y,z) -> Three (f x, f y, f z)

let rec map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  = fun f q -> match q with
    | Shallow d -> Shallow (_map_digit f d)
    | Deep (size, hd, lazy q', tl) ->
      let q'' = map (fun (x,y) -> f x, f y) q' in
      _deep size (_map_digit f hd) (Lazy.from_val q'') (_map_digit f tl)

(*$Q
  (Q.list Q.int) (fun l -> \
    of_list l |> map string_of_int |> to_list = List.map string_of_int l)
*)

let (>|=) q f = map f q

let _fold_digit : type l. ('acc -> 'a -> 'acc) -> 'acc -> ('a, l) digit -> 'acc = fun f acc d -> match d with
  | Zero -> acc
  | One x -> f acc x
  | Two (x,y) -> f (f acc x) y
  | Three (x,y,z) -> f (f (f acc x) y) z

let rec fold : 'a 'b. ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  = fun f acc q -> match q with
    | Shallow d -> _fold_digit f acc d
    | Deep (_, hd, lazy q', tl) ->
      let acc = _fold_digit f acc hd in
      let acc = fold (fun acc (x,y) -> f (f acc x) y) acc q' in
      _fold_digit f acc tl

(*$Q
  (Q.list Q.int) (fun l -> \
    of_list l |> fold (fun acc x->x::acc) [] = List.rev l)
*)

(*$R
  let q = of_seq (Iter.of_list [1;2;3;4]) in
  let n = fold (+) 0 q in
  OUnit.assert_equal 10 n;
*)

let iter f q = to_seq q f

let of_list l = List.fold_left snoc empty l

let to_list q =
  let l = ref [] in
  to_seq q (fun x -> l := x :: !l);
  List.rev !l

let of_seq seq = add_seq_front seq empty

(*$Q
  (Q.list Q.int) (fun l -> \
    Iter.of_list l |> of_seq |> to_list = l)
*)

let rev q =
  let q' = ref empty in
  iter (fun x -> q' := cons x !q') q;
  !q'

(*$Q
  (Q.list Q.int) (fun l -> \
    of_list l |> rev |> to_list = List.rev l)
*)

let _nil () = `Nil
let _single x cont () = `Cons (x, cont)
let _double x y cont () = `Cons (x, _single y cont)
let _triple x y z cont () = `Cons (x, _double y z cont)

let _digit_to_klist : type l. ('a, l) digit -> 'a klist -> 'a klist = fun d cont -> match d with
  | Zero -> _nil
  | One x -> _single x cont
  | Two (x,y) -> _double x y cont
  | Three (x,y,z) -> _triple x y z cont

let rec _flat_klist : 'a. ('a * 'a) klist -> 'a klist -> 'a klist
  = fun l cont () -> match l () with
    | `Nil -> cont ()
    | `Cons ((x,y),l') -> _double x y (_flat_klist l' cont) ()

let to_klist q =
  let rec aux : 'a. 'a t -> 'a klist -> 'a klist
    = fun q cont () -> match q with
      | Shallow d -> _digit_to_klist d cont ()
      | Deep (_, hd, lazy q', tl) ->
        _digit_to_klist hd
          (_flat_klist
             (aux q' _nil)
             (_digit_to_klist tl cont))
          ()
  in
  aux q _nil

let of_klist l =
  let rec seq l k = match l() with
    | `Nil -> ()
    | `Cons(x,l') -> k x; seq l' k
  in
  add_seq_front (seq l) empty

let rec _equal_klist eq l1 l2 = match l1(), l2() with
  | `Nil, `Nil -> true
  | `Nil, _
  | _, `Nil -> false
  | `Cons(x1,l1'), `Cons(x2,l2') ->
    eq x1 x2 && _equal_klist eq l1' l2'

let equal eq q1 q2 = _equal_klist eq (to_klist q1) (to_klist q2)

(*$T
  let q1 = 1 -- 10 and q2 = append (1 -- 5) (6 -- 10) in \
  equal (=) q1 q2
*)

let (--) a b =
  let rec up_to q a b = if a = b
    then snoc q a
    else up_to (snoc q a) (a+1) b
  and down_to q a b = if a = b then snoc q a
    else down_to (snoc q a) (a-1) b
  in
  if a <= b then up_to empty a b else down_to empty a b

(*$T
  1 -- 5 |> to_list = [1;2;3;4;5]
  5 -- 1 |> to_list = [5;4;3;2;1]
  0 -- 0 |> to_list = [0]
*)

let (--^) a b =
  if a=b then empty
  else if a<b then a -- (b-1)
  else a -- (b+1)

(*$T
  1 --^ 5 |> to_list = [1;2;3;4]
  5 --^ 1 |> to_list = [5;4;3;2]
  1 --^ 2 |> to_list = [1]
  0 --^ 0 |> to_list = []
*)

let pp pp_x out d =
  let first = ref true in
  Format.fprintf out "@[<hov2>queue {";
  iter
    (fun x ->
       if !first then first:= false else Format.fprintf out ";@ ";
       pp_x out x
    ) d;
  Format.fprintf out "}@]"
