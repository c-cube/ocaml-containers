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
type 'a hash_fun = 'a -> t

let combine hash i =
  (hash * 65599 + i) land max_int

let (<<>>) = combine

let hash_int i = combine 0 i

let hash_int2 i j = combine i j

let hash_int3 i j k = combine (combine i j) k

let hash_int4 i j k l =
  combine (combine (combine i j) k) l

let rec hash_list f h l = match l with
  | [] -> h
  | x::l' -> hash_list f (combine h (f x)) l'

let hash_array f h a =
  let h = ref h in
  Array.iter (fun x -> h := combine !h (f x)) a;
  !h

let hash_string s = Hashtbl.hash s

let hash_pair h1 h2 (x,y) = combine (h1 x) (h2 y)
let hash_triple h1 h2 h3 (x,y,z) = (h1 x) <<>> (h2 y) <<>> (h3 z)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = [`Nil | `Cons of 'a * (unit -> 'a klist)]

let hash_seq f h seq =
  let h = ref h in
  seq (fun x -> h := !h <<>> f x);
  !h

let rec hash_gen f h g = match g () with
  | None -> h
  | Some x ->
      hash_gen f (h <<>> f x) g

let rec hash_klist f h l = match l with
  | `Nil -> h
  | `Cons (x,l') -> hash_klist f (h <<>> f x) (l' ())

