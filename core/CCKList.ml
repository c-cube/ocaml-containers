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

(** {1 Continuation List} *)


type + 'a t =
  [ `Nil
  | `Cons of 'a * (unit -> 'a t)
  ]

let nil = `Nil
let cons a b = `Cons (a,b)

let singleton x = `Cons (x, fun () -> `Nil)

let to_list l =
  let rec direct i (l:'a t) = match l with
    | `Nil -> []
    | _ when i=0 -> safe [] l
    | `Cons (x, f) -> x :: direct (i-1) (f ())
  and safe acc l = match l with
    | `Nil -> List.rev acc
    | `Cons (x,l') -> safe (x::acc) (l' ())
  in
  direct 200 l

let of_list l =
  let rec aux l () = match l with
    | [] -> `Nil
    | x::l' -> `Cons (x, aux l')
  in aux l ()

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let rec to_seq res k = match res with
  | `Nil -> ()
  | `Cons (s, f) -> k s; to_seq (f ()) k

let to_gen l =
  let l = ref l in
  fun () ->
    match !l with
    | `Nil -> None
    | `Cons (x,l') ->
        l := l' ();
        Some x

let rec fold f acc res = match res with
  | `Nil -> acc
  | `Cons (s, cont) -> fold f (f acc s) (cont ())

let rec iter f l = match l with
  | `Nil -> ()
  | `Cons (x, l') -> f x; iter f (l' ())

let length l = fold (fun acc _ -> acc+1) 0 l

let rec take n (l:'a t):'a t = match l with
  | _ when n=0 -> `Nil
  | `Nil -> `Nil
  | `Cons (x,l') -> `Cons (x, fun () -> take (n-1) (l' ()))

let rec drop n (l:'a t) = match l with
  | _ when n=0 -> l
  | `Nil -> `Nil
  | `Cons (_,l') -> drop (n-1) (l'())

let rec map f l = match l with
  | `Nil -> `Nil
  | `Cons (x, l') -> `Cons (f x, fun () -> map f (l' ()))

let rec fmap f (l:'a t):'b t = match l with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      begin match f x with
      | None -> fmap f (l' ())
      | Some y -> `Cons (y, fun () -> fmap f (l' ()))
      end

let rec filter p l = match l with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      if p x
      then `Cons (x, fun () -> filter p (l' ()))
      else filter p (l' ())

let rec append l1 l2 = match l1 with
  | `Nil -> l2
  | `Cons (x, l1') -> `Cons (x, fun () -> append (l1' ()) l2)

let rec flat_map f l = match l with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      append (f x) (flat_map f (l' ()))

