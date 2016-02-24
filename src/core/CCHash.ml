
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

type t = int

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

let _r = 47
let _m = 0xc6a4a7935bd1e995L

let init = _m

(* combine key [k] with the current state [s] *)
let combine_murmur_ s k =
  let k = Int64.mul _m k in
  let k = Int64.logxor k (Int64.shift_right k _r) in
  let k = Int64.mul _m k in
  let s = Int64.logxor s k in
  let s = Int64.mul _m s in
  s

let finish s =
  let s = Int64.logxor s (Int64.shift_right s _r) in
  let s = Int64.mul s _m in
  let s = Int64.logxor s (Int64.shift_right s _r) in
  (Int64.to_int s) land max_int

let apply f x = finish (f x init)

(** {2 Combinators} *)

(** {2 Generic Hashing} *)

module type HASH = sig
  type state

  val int : int -> state -> state
  val bool : bool -> state -> state
  val char : char -> state -> state
  val int32 : int32 -> state -> state
  val int64 : int64 -> state -> state
  val nativeint : nativeint -> state -> state
  val slice : string -> int -> int -> state -> state
  (** [slice s i len state] hashes the slice [[i, ... i+len)] of [s]
      into [state] *)
end

module type S = sig
  include HASH

  type 'a hash_fun = 'a -> state -> state

  val string : string hash_fun

  val list : 'a hash_fun -> 'a list hash_fun

  val array : 'a hash_fun -> 'a array hash_fun

  val opt : 'a hash_fun -> 'a option hash_fun
  val pair : 'a hash_fun -> 'b hash_fun -> ('a * 'b) hash_fun
  val triple : 'a hash_fun -> 'b hash_fun -> 'c hash_fun -> ('a * 'b * 'c) hash_fun

  val if_ : bool -> 'a hash_fun -> 'a hash_fun -> 'a hash_fun
  (** Decide which hash function to use depending on the boolean *)

  (** {2 Iterators} *)

  val seq : 'a hash_fun -> 'a sequence hash_fun
  val gen : 'a hash_fun -> 'a gen hash_fun
  val klist : 'a hash_fun -> 'a klist hash_fun
end

module Base = struct
  type state = int64
  let int i s = combine_murmur_ s (Int64.of_int i)
  let bool x s = combine_murmur_ s (if x then 1L else 2L)
  let char x s = combine_murmur_ s (Int64.of_int (Char.code x))
  let int32 x s = combine_murmur_ s (Int64.of_int32 x)
  let int64 x s = combine_murmur_ s x
  let nativeint x s = combine_murmur_ s (Int64.of_nativeint x)

  let slice x i len s =
    let j=i+len in
    let rec aux i s =
      if i=j then s else aux (i+1) (char x.[i] s)
    in
    aux i s
end

module Make(H : HASH) : S with type state = H.state = struct
  include H

  type 'a hash_fun = 'a -> state -> state

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
end

include Make(Base)

(* deprecated aliases *)

let int_ = int
let bool_ = bool
let char_ = char
let int32_ = int32
let int64_ = int64
let nativeint_ = nativeint
let string_ = string

let list_ = list
let array_ = array
