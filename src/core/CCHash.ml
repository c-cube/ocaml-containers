
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

type hash = int

type 'a t = 'a -> hash

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

let combine f s x = Hashtbl.seeded_hash s (f x)

let combine2 a b = Hashtbl.seeded_hash a b

let combine3 a b c =
  combine2 (combine2 a b) c

let combine4 a b c d =
  combine2 (combine2 a b) (combine2 c d)

let combine5 a b c d e =
  combine2 (combine2 a b) (combine2 (combine2 c d) e)

let combine6 a b c d e f =
  combine2 (combine2 a b) (combine2 (combine2 c d) (combine2 e f))

(** {2 Combinators} *)

let const h _ = h
let const0 _ = 0

let int i = i land max_int
let bool b = if b then 1 else 2
let char x = Char.code x
let int32 (x:int32) = Hashtbl.hash x
let int64 (x:int64) = Hashtbl.hash x
let nativeint (x:nativeint) = Hashtbl.hash x
let string (x:string) = Hashtbl.hash x

let slice x i len =
  let j=i+len in
  let rec aux i s =
    if i=j then s else aux (i+1) (combine2 (char x.[i]) s)
  in
  aux i 0

let opt f = function
  | None -> 42
  | Some x -> combine2 43 (f x)

let list f l = List.fold_left (combine f) 0x42 l
let array f l = Array.fold_left (combine f) 0x42 l

let pair f g (x,y) = combine2 (f x) (g y)
let triple f g h (x,y,z) = combine2 (combine2 (f x) (g y)) (h z)
let quad f g h i (x,y,z,w) = combine2 (combine2 (f x) (g y)) (combine2 (h z) (i w))

let if_ b then_ else_ h =
  if b then then_ h else else_ h

let poly x = Hashtbl.hash x

let array_comm f a =
  let arr = Array.init (Array.length a) (fun i -> f a.(i)) in
  Array.sort CCInt.compare arr; (* sort the hashes, so their order does not matter *)
  array (fun h->h) arr

let list_comm f l =
  let a = Array.of_list l in
  array_comm f a

let iter f seq =
  let h = ref 0x43 in
  seq (fun x -> h := combine f !h x);
  !h

let seq f seq =
  let h = ref 0x43 in
  Seq.iter (fun x -> h := combine f !h x) seq;
  !h

let gen f g =
  let rec aux s = match g () with
    | None -> s
    | Some x -> aux (combine2 s (f x))
  in aux 0x42

let klist f l =
  let rec aux l s = match l () with
    | `Nil -> s
    | `Cons (x,tail) -> aux tail (combine2 s (f x))
  in aux l 0x42
