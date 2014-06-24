(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Hash combinators} *)

type t = int
type state = int64
type 'a hash_fun = 'a -> state -> state

let _r = 47
let _m = 0xc6a4a7935bd1e995L

let init = _m  (* TODO? *)

(* combine key [k] with the current state [s] *)
let _combine s k =
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

let int_ i s = _combine s (Int64.of_int i)
let bool_ x s = _combine s (if x then 1L else 2L)
let char_ x s = _combine s (Int64.of_int (Char.code x))
let int32_ x s = _combine s (Int64.of_int32 x)
let int64_ x s = _combine s x
let nativeint_ x s = _combine s (Int64.of_nativeint x)
let string_ x s =
  let s = ref s in
  String.iter (fun c -> s := char_ c !s) x;
  !s

let rec list_ f l s = match l with
  | [] -> s
  | x::l' -> list_ f l' (f x s)

let array_ f a s = Array.fold_right f a s

let opt f o h = match o with
  | None -> h
  | Some x -> f x h
let pair h1 h2 (x,y) s = h2 y (h1 x s)
let triple h1 h2 h3 (x,y,z) s = h3 z (h2 y (h1 x s))

let if_ b then_ else_ h =
  if b then then_ h else else_ h

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

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
