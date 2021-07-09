
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

type hash = int

type 'a t = 'a -> hash

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(* FNV hashing
   https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
*)
let fnv_offset_basis = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L

(* hash an integer *)
let hash_int_ n =
  let h = ref fnv_offset_basis in
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((n lsr (k * 8)) land 0xff)));
  done;
  (Int64.to_int !h) land max_int (* truncate back to int and remove sign *)

let combine2 a b =
  let h = ref fnv_offset_basis in
  (* we only do one loop, where we mix bytes of [a] and [b], so as
     to simplify control flow *)
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((a lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((b lsr (k * 8)) land 0xff)));
  done;
  Int64.to_int !h land max_int

let[@inline] combine f s x =
  combine2 s (f x)

let combine3 a b c =
  let h = ref fnv_offset_basis in
  (* we only do one loop, where we mix bytes of [a] [b] and [c], so as
     to simplify control flow *)
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((a lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((b lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((c lsr (k * 8)) land 0xff)));
  done;
  Int64.to_int !h land max_int

let combine4 a b c d =
  let h = ref fnv_offset_basis in
  for k = 0 to 7 do
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((a lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((b lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((c lsr (k * 8)) land 0xff)));
    h := Int64.(mul !h fnv_prime);
    h := Int64.(logxor !h (of_int ((d lsr (k * 8)) land 0xff)));
  done;
  Int64.to_int !h land max_int

let combine5 a b c d e =
  combine3 a b (combine3 c d e)

let combine6 a b c d e f =
  combine4 a b c (combine3 d e f)

(** {2 Combinators} *)

let const h _ = h
let const0 _ = 0

let int = hash_int_
let bool b = hash_int_ (if b then 1 else 2)
let char x = hash_int_ (Char.code x)
let int32 (x:int32) = Hashtbl.hash x (* TODO: FNV *)
let int64 (x:int64) = Hashtbl.hash x (* TODO: FNV *)
let nativeint (x:nativeint) = Hashtbl.hash x

let bytes (x:bytes) =
  let h = ref fnv_offset_basis in
  Bytes.iter (fun c ->
      h := Int64.(mul !h fnv_prime);
      let byte = Char.code c in
      h := Int64.(logxor !h (of_int byte));
    ) x;
  Int64.to_int !h land max_int

let string (x:string) = bytes (Bytes.unsafe_of_string x)

(*$T
  int 42 >= 0
  int max_int >= 0
  int max_int = int max_int
  int min_int >= 0
  int 0 >= 0
  char 'c' >= 0
  int 152352 = int 152352
*)

let slice x i len =
  let j=i+len in
  let rec aux i s =
    if i=j then s else aux (i+1) (combine2 (Char.code x.[i]) s)
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
let map f h x = h (f x)

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
