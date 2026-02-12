(* This file is free software, part of containers. See file "license" for more details. *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

include Seq

let nil () = Nil

[@@@iflt 4.11]

let cons a b () = Cons (a, b)

[@@@endif]
[@@@iflt 5.4]

let singleton x () = Cons (x, nil)

[@@@endif]
[@@@iflt 4.14]

let init n f =
  let rec aux i () =
    if i >= n then
      Nil
    else
      Cons (f i, aux (i + 1))
  in
  aux 0

[@@@endif]

let rec _forever x () = Cons (x, _forever x)

let rec _repeat n x () =
  if n <= 0 then
    Nil
  else
    Cons (x, _repeat (n - 1) x)

let repeat ?n x =
  match n with
  | None -> _forever x
  | Some n -> _repeat n x

let rec forever f () = Cons (f (), forever f)

[@@@iflt 4.14]

let is_empty l =
  match l () with
  | Nil -> true
  | Cons _ -> false

[@@@endif]

let head_exn l =
  match l () with
  | Nil -> raise Not_found
  | Cons (x, _) -> x

let head l =
  match l () with
  | Nil -> None
  | Cons (x, _) -> Some x

let tail_exn l =
  match l () with
  | Nil -> raise Not_found
  | Cons (_, l) -> l

let tail l =
  match l () with
  | Nil -> None
  | Cons (_, l) -> Some l

[@@@iflt 4.14]

let uncons l =
  match l () with
  | Nil -> None
  | Cons (h, t) -> Some (h, t)

[@@@endif]

let rec equal eq l1 l2 =
  match l1 (), l2 () with
  | Nil, Nil -> true
  | Nil, _ | _, Nil -> false
  | Cons (x1, l1'), Cons (x2, l2') -> eq x1 x2 && equal eq l1' l2'

let rec compare cmp l1 l2 =
  match l1 (), l2 () with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Cons (x1, l1'), Cons (x2, l2') ->
    let c = cmp x1 x2 in
    if c = 0 then
      compare cmp l1' l2'
    else
      c

let rec fold f acc res =
  match res () with
  | Nil -> acc
  | Cons (s, cont) -> fold f (f acc s) cont

let fold_left = fold

let foldi f acc res =
  let rec aux acc i res =
    match res () with
    | Nil -> acc
    | Cons (s, cont) -> aux (f acc i s) (i + 1) cont
  in
  aux acc 0 res

[@@@iflt 4.14]

let fold_lefti = foldi

let iteri f l =
  let rec aux f l i =
    match l () with
    | Nil -> ()
    | Cons (x, l') ->
      f i x;
      aux f l' (i + 1)
  in
  aux f l 0

let length l = fold (fun acc _ -> acc + 1) 0 l

let rec take n (l : 'a t) () =
  if n = 0 then
    Nil
  else (
    match l () with
    | Nil -> Nil
    | Cons (x, l') -> Cons (x, take (n - 1) l')
  )

let rec take_while p l () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    if p x then
      Cons (x, take_while p l')
    else
      Nil

let rec drop n (l : 'a t) () =
  match l () with
  | l' when n = 0 -> l'
  | Nil -> Nil
  | Cons (_, l') -> drop (n - 1) l' ()

let rec drop_while p l () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') when p x -> drop_while p l' ()
  | Cons _ as res -> res

let mapi f l =
  let rec aux f l i () =
    match l () with
    | Nil -> Nil
    | Cons (x, tl) -> Cons (f i x, aux f tl (i + 1))
  in
  aux f l 0

[@@@endif]
[@@@iflt 5.4]

let filteri f l =
  let rec aux f l i () =
    match l () with
    | Nil -> Nil
    | Cons (x, tl) ->
      if f i x then
        Cons (x, aux f tl (i + 1))
      else
        aux f tl (i + 1) ()
  in
  aux f l 0

[@@@endif]

let fmap = filter_map

[@@@iflt 4.11]

let rec append l1 l2 () =
  match l1 () with
  | Nil -> l2 ()
  | Cons (x, l1') -> Cons (x, append l1' l2)

[@@@endif]
[@@@iflt 4.14]

let rec cycle l =
  if is_empty l then
    l
  else
    fun () ->
  append l (cycle l) ()

let rec iterate f a () = Cons (a, iterate f (f a))

[@@@endif]
[@@@iflt 4.11]

let rec unfold f acc () =
  match f acc with
  | None -> Nil
  | Some (x, acc') -> Cons (x, unfold f acc')

[@@@endif]
[@@@iflt 4.14]

let rec for_all p l =
  match l () with
  | Nil -> true
  | Cons (x, tl) -> p x && for_all p tl

let rec exists p l =
  match l () with
  | Nil -> false
  | Cons (x, tl) -> p x || exists p tl

let rec find p l =
  match l () with
  | Nil -> None
  | Cons (x, tl) ->
    if p x then
      Some x
    else
      find p tl

let rec find_map f l =
  match l () with
  | Nil -> None
  | Cons (x, tl) ->
    (match f x with
    | None -> find_map f tl
    | e -> e)

[@@@endif]
[@@@iflt 5.1]

let find_index p l =
  let rec aux i l =
    match l () with
    | Nil -> None
    | Cons (x, tl) ->
      if p x then
        Some i
      else
        aux (i + 1) tl
  in
  aux 0 l

let find_mapi f l =
  let rec aux i l =
    match l () with
    | Nil -> None
    | Cons (x, tl) ->
      (match f i x with
      | Some _ as res -> res
      | None -> aux (i + 1) tl)
  in
  aux 0 l

[@@@endif]
[@@@iflt 5.1]

let rec scan f acc res () =
  Cons
    ( acc,
      fun () ->
        match res () with
        | Nil -> Nil
        | Cons (s, cont) -> scan f (f acc s) cont () )

[@@@endif]
[@@@iflt 4.13]

let concat_map = flat_map

[@@@endif]

let product_with f l1 l2 =
  let rec _next_left h1 tl1 h2 tl2 () =
    match tl1 () with
    | Nil -> _next_right ~die:true h1 tl1 h2 tl2 ()
    | Cons (x, tl1') ->
      _map_list_left x h2 (_next_right ~die:false (x :: h1) tl1' h2 tl2) ()
  and _next_right ~die h1 tl1 h2 tl2 () =
    match tl2 () with
    | Nil when die -> Nil
    | Nil -> _next_left h1 tl1 h2 tl2 ()
    | Cons (y, tl2') ->
      _map_list_right h1 y (_next_left h1 tl1 (y :: h2) tl2') ()
  and _map_list_left x l kont () =
    match l with
    | [] -> kont ()
    | y :: l' -> Cons (f x y, _map_list_left x l' kont)
  and _map_list_right l y kont () =
    match l with
    | [] -> kont ()
    | x :: l' -> Cons (f x y, _map_list_right l' y kont)
  in
  _next_left [] l1 [] l2

[@@@iflt 4.14]

let map_product = product_with
let product l1 l2 = product_with (fun x y -> x, y) l1 l2

let rec group eq l () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    Cons (cons x (take_while (eq x) l'), group eq (drop_while (eq x) l'))

[@@@endif]

let rec _uniq eq prev l () =
  match prev, l () with
  | _, Nil -> Nil
  | None, Cons (x, l') -> Cons (x, _uniq eq (Some x) l')
  | Some y, Cons (x, l') ->
    if eq x y then
      _uniq eq prev l' ()
    else
      Cons (x, _uniq eq (Some x) l')

let uniq eq l = _uniq eq None l

[@@@iflt 4.13]

let concat l = flat_map (fun x -> x) l

[@@@endif]

let flatten = concat

let range i j =
  let rec aux i j () =
    if i = j then
      Cons (i, nil)
    else if i < j then
      Cons (i, aux (i + 1) j)
    else
      Cons (i, aux (i - 1) j)
  in
  aux i j

let ( -- ) = range

let ( --^ ) i j =
  if i = j then
    empty
  else if i < j then
    range i (j - 1)
  else
    range i (j + 1)

[@@@iflt 4.14]

let rec fold_left2 f acc l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> acc
  | Cons (x1, l1'), Cons (x2, l2') -> fold_left2 f (f acc x1 x2) l1' l2'

[@@@endif]

let fold2 = fold_left2

[@@@iflt 4.14]

let rec map2 f l1 l2 () =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> Nil
  | Cons (x1, l1'), Cons (x2, l2') -> Cons (f x1 x2, map2 f l1' l2')

let rec iter2 f l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> ()
  | Cons (x1, l1'), Cons (x2, l2') ->
    f x1 x2;
    iter2 f l1' l2'

let rec for_all2 f l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> true
  | Cons (x1, l1'), Cons (x2, l2') -> f x1 x2 && for_all2 f l1' l2'

let rec exists2 f l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> false
  | Cons (x1, l1'), Cons (x2, l2') -> f x1 x2 || exists2 f l1' l2'

let rec sorted_merge cmp l1 l2 () =
  match l1 (), l2 () with
  | Nil, tl2 -> tl2
  | tl1, Nil -> tl1
  | Cons (x1, l1'), Cons (x2, l2') ->
    if cmp x1 x2 < 0 then
      Cons (x1, sorted_merge cmp l1' l2)
    else
      Cons (x2, sorted_merge cmp l1 l2')

[@@@endif]

let merge = sorted_merge

[@@@iflt 4.14]

let rec zip a b () =
  match a (), b () with
  | Nil, _ | _, Nil -> Nil
  | Cons (x, a'), Cons (y, b') -> Cons ((x, y), zip a' b')

let unzip l =
  let rec first l () =
    match l () with
    | Nil -> Nil
    | Cons ((x, _), tl) -> Cons (x, first tl)
  and second l () =
    match l () with
    | Nil -> Nil
    | Cons ((_, y), tl) -> Cons (y, second tl)
  in
  first l, second l

let split = unzip

[@@@endif]

let zip_i seq =
  let rec loop i seq () =
    match seq () with
    | Nil -> Nil
    | Cons (x, tl) -> Cons ((i, x), loop (i + 1) tl)
  in
  loop 0 seq

(** {2 Implementations} *)

let pure = return
let ( >>= ) xs f = flat_map f xs
let ( >|= ) xs f = map f xs
let ( <*> ) fs xs = product_with (fun f x -> f x) fs xs

(** {2 Conversions} *)

let rec _to_rev_list acc l =
  match l () with
  | Nil -> acc
  | Cons (x, l') -> _to_rev_list (x :: acc) l'

let to_rev_list l = _to_rev_list [] l

let to_list l =
  let rec direct i (l : 'a t) =
    match l () with
    | Nil -> []
    | _ when i = 0 -> List.rev (_to_rev_list [] l)
    | Cons (x, f) -> x :: direct (i - 1) f
  in
  direct 200 l

let of_list l =
  let rec aux l () =
    match l with
    | [] -> Nil
    | x :: l' -> Cons (x, aux l')
  in
  aux l

let of_array a =
  let rec aux a i () =
    if i = Array.length a then
      Nil
    else
      Cons (a.(i), aux a (i + 1))
  in
  aux a 0

let of_string s =
  let rec aux s i () =
    if i = String.length s then
      Nil
    else
      Cons (String.get s i, aux s (i + 1))
  in
  aux s 0

let to_array l =
  (* We contruct the length and list of seq elements (in reverse) in one pass *)
  let len = ref 0 in
  let ls =
    fold_left
      (fun acc x ->
        incr len;
        x :: acc)
      [] l
  in
  (* The length is used to initialize the array, and then to derive the index for
     each item, working back from the last. This lets us only traverse the list
     twice, instead of having to reverse it. *)
  match ls with
  | [] -> [||]
  | init :: rest ->
    let a = Array.make !len init in
    (* Subtract 1 for len->index conversion and 1 for the removed [init] *)
    let idx = !len - 2 in
    ignore
      (List.fold_left
         (fun i x ->
           a.(i) <- x;
           i - 1)
         idx rest
        : int);
    a

let rec to_iter res k =
  match res () with
  | Nil -> ()
  | Cons (s, f) ->
    k s;
    to_iter f k

let to_gen l =
  let l = ref l in
  fun () ->
    match !l () with
    | Nil -> None
    | Cons (x, l') ->
      l := l';
      Some x

type 'a of_gen_state =
  | Of_gen_thunk of 'a gen
  | Of_gen_saved of 'a node

let of_gen g =
  let rec consume r () =
    match !r with
    | Of_gen_saved cons -> cons
    | Of_gen_thunk g ->
      (match g () with
      | None ->
        r := Of_gen_saved Nil;
        Nil
      | Some x ->
        let tl = consume (ref (Of_gen_thunk g)) in
        let l = Cons (x, tl) in
        r := Of_gen_saved l;
        l)
  in
  consume (ref (Of_gen_thunk g))

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
  fun () ->
    match !r with
    | MemoSave l -> l
    | MemoThunk ->
      let l =
        match f () with
        | Nil -> Nil
        | Cons (x, tail) -> Cons (x, memoize tail)
      in
      r := MemoSave l;
      l

(** {2 Fair Combinations} *)

[@@@iflt 4.14]

let rec interleave a b () =
  match a () with
  | Nil -> b ()
  | Cons (x, tail) -> Cons (x, interleave b tail)

[@@@endif]

let rec fair_flat_map f a () =
  match a () with
  | Nil -> Nil
  | Cons (x, tail) ->
    let y = f x in
    interleave y (fair_flat_map f tail) ()

let rec fair_app f a () =
  match f () with
  | Nil -> Nil
  | Cons (f1, fs) -> interleave (map f1 a) (fair_app fs a) ()

let ( >>- ) a f = fair_flat_map f a
let ( <.> ) f a = fair_app f a

(** {2 Infix} *)

module Infix = struct
  let ( >>= ) = ( >>= )
  let ( >|= ) = ( >|= )
  let ( <*> ) = ( <*> )
  let ( >>- ) = ( >>- )
  let ( <.> ) = ( <.> )
  let ( -- ) = ( -- )
  let ( --^ ) = ( --^ )
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse (M : MONAD) = struct
  open M

  let map_m f l =
    let rec aux acc l =
      match l () with
      | Nil -> return (of_list (List.rev acc))
      | Cons (x, l') -> f x >>= fun x' -> aux (x' :: acc) l'
    in
    aux [] l

  let sequence_m l = map_m (fun x -> x) l

  let rec fold_m f acc l =
    match l () with
    | Nil -> return acc
    | Cons (x, l') -> f acc x >>= fun acc' -> fold_m f acc' l'
end

(** {2 IO} *)

let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
    ?(pp_sep = fun out () -> Format.fprintf out ",@ ") pp_item fmt l =
  pp_start fmt ();
  let rec pp fmt l =
    match l () with
    | Nil -> ()
    | Cons (x, l') ->
      pp_sep fmt ();
      Format.pp_print_cut fmt ();
      pp_item fmt x;
      pp fmt l'
  in
  (match l () with
  | Nil -> ()
  | Cons (x, l') ->
    pp_item fmt x;
    pp fmt l');
  pp_stop fmt ()
