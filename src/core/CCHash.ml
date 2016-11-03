
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

type t = int

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

type state = int

type 'a hash_fun = 'a -> state -> state

let combine f x s = Hashtbl.seeded_hash s (f x)

let combine2 a b = Hashtbl.seeded_hash a b

let init : state = 0
let finish i = i

let apply f x = f x init

(** {2 Combinators} *)

let int i s = combine2 s (Int64.of_int i)
let bool x s = combine2 s (if x then 1L else 2L)
let char x s = combine2 s (Int64.of_int (Char.code x))
let int32 x s = combine2 s (Int64.of_int32 x)
let int64 x s = combine2 s x
let nativeint x s = combine2 s (Int64.of_nativeint x)

let slice x i len s =
  let j=i+len in
  let rec aux i s =
    if i=j then s else aux (i+1) (char x.[i] s)
  in
  aux i s

let rec list f l s = match l with
  | [] -> s
  | x::l' -> list f l' (f x s)

let array f a s = Array.fold_right f a s

let opt f o h = match o with
  | None -> h
  | Some x -> f x h
let pair h1 h2 (x,y) s = h2 y (h1 x s)
let triple h1 h2 h3 (x,y,z) s = h3 z (h2 y (h1 x s))

let string x s = slice x 0 (String.length x) s

let if_ b then_ else_ h =
  if b then then_ h else else_ h

let seq f seq s =
  let s = ref s in
  seq (fun x -> s := f x !s);
  !s

let rec gen f g s = match g () with
  | None -> s
  | Some x -> gen f g (f x s)

let rec klist f l s = match l () with
  | `Nil -> s
  | `Cons (x,l') -> klist f l' (f x s)
