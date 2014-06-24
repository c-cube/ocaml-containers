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

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

type + 'a t = unit -> 
  [ `Nil
  | `Cons of 'a * 'a t
  ]

let nil () = `Nil
let cons a b () = `Cons (a,b)
let empty = nil

let singleton x () = `Cons (x, nil)

let is_empty l = match l () with
  | `Nil -> true
  | `Cons _ -> false

let rec equal eq l1 l2 = match l1(), l2() with
  | `Nil, `Nil -> true
  | `Nil, _
  | _, `Nil -> false
  | `Cons (x1,l1'), `Cons (x2,l2') ->
      eq x1 x2 && equal eq l1' l2'

let rec compare cmp l1 l2 = match l1(), l2() with
  | `Nil, `Nil -> 0
  | `Nil, _ -> -1
  | _, `Nil -> 1
  | `Cons (x1,l1'), `Cons (x2,l2') ->
      let c = cmp x1 x2 in
      if c = 0 then compare cmp l1' l2' else c

let rec fold f acc res = match res () with
  | `Nil -> acc
  | `Cons (s, cont) -> fold f (f acc s) cont

let rec iter f l = match l () with
  | `Nil -> ()
  | `Cons (x, l') -> f x; iter f l'

let length l = fold (fun acc _ -> acc+1) 0 l

let rec take n (l:'a t) () = match l () with
  | _ when n=0 -> `Nil
  | `Nil -> `Nil
  | `Cons (x,l') -> `Cons (x, take (n-1) l')

let rec take_while p l () = match l () with
  | `Nil -> `Nil
  | `Cons (x,l') when p x -> `Cons (x, take_while p l')
  | `Cons (_,l') -> take_while p l' ()

let rec drop n (l:'a t) () = match l () with
  | l' when n=0 -> l'
  | `Nil -> `Nil
  | `Cons (_,l') -> drop (n-1) l' ()

let rec drop_while p l () = match l() with
  | `Nil -> `Nil
  | `Cons (x,l') when p x -> drop_while p l' ()
  | `Cons _ as res -> res

(*$Q
  (Q.pair (Q.list Q.small_int) Q.small_int) (fun (l,n) -> \
    let s = of_list l in let s1, s2 = take n s, drop n s in \
    append s1 s2 |> to_list = l  )
*)

let rec map f l () = match l () with
  | `Nil -> `Nil
  | `Cons (x, l') -> `Cons (f x, map f l')

(*$T
  (map ((+) 1) (1 -- 5) |> to_list) = (2 -- 6 |> to_list)
*)

let rec fmap f (l:'a t) () = match l() with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      begin match f x with
      | None -> fmap f l' ()
      | Some y -> `Cons (y, fmap f l')
      end

(*$T
  fmap (fun x -> if x mod 2=0 then Some (x*3) else None) (1--10) |> to_list \
    = [6;12;18;24;30]
*)

let rec filter p l () = match l () with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      if p x
      then `Cons (x, filter p l')
      else filter p l' ()

let rec append l1 l2 () = match l1 () with
  | `Nil -> l2 ()
  | `Cons (x, l1') -> `Cons (x, append l1' l2)

let rec flat_map f l () = match l () with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      _flat_map_app f (f x) l' ()
and _flat_map_app f l l' () = match l () with
  | `Nil -> flat_map f l' ()
  | `Cons (x, tl) ->
      `Cons (x, _flat_map_app f tl l')

let rec filter_map f l () = match l() with
  | `Nil -> `Nil
  | `Cons (x, l') ->
      begin match f x with
      | None -> filter_map f l' ()
      | Some y -> `Cons (y, filter_map f l')
      end

let flatten l = flat_map (fun x->x) l

let range i j =
  let rec aux i j () =
    if i=j then `Cons(i, nil)
    else if i<j then `Cons (i, aux (i+1) j)
    else `Cons (i, aux (i-1) j)
  in aux i j

(*$T
  range 0 5 |> to_list = [0;1;2;3;4;5]
  range 0 0 |> to_list = [0]
  range 5 2 |> to_list = [5;4;3;2]
*)

let (--) = range

let rec fold2 f acc l1 l2 = match l1(), l2() with
  | `Nil, _
  | _, `Nil -> acc
  | `Cons(x1,l1'), `Cons(x2,l2') ->
      fold2 f (f acc x1 x2) l1' l2'

let rec map2 f l1 l2 () = match l1(), l2() with
  | `Nil, _
  | _, `Nil -> `Nil
  | `Cons(x1,l1'), `Cons(x2,l2') ->
      `Cons (f x1 x2, map2 f l1' l2')

let rec iter2 f l1 l2 = match l1(), l2() with
  | `Nil, _
  | _, `Nil -> ()
  | `Cons(x1,l1'), `Cons(x2,l2') ->
      f x1 x2; iter2 f l1' l2'

let rec for_all2 f l1 l2 = match l1(), l2() with
  | `Nil, _
  | _, `Nil -> true
  | `Cons(x1,l1'), `Cons(x2,l2') ->
      f x1 x2 && for_all2 f l1' l2'

let rec exists2 f l1 l2 = match l1(), l2() with
  | `Nil, _
  | _, `Nil -> false
  | `Cons(x1,l1'), `Cons(x2,l2') ->
      f x1 x2 || exists2 f l1' l2'

let rec merge cmp l1 l2 () = match l1(), l2() with
  | `Nil, tl2 -> tl2
  | tl1, `Nil -> tl1
  | `Cons(x1,l1'), `Cons(x2,l2') ->
      if cmp x1 x2 < 0
      then `Cons (x1, merge cmp l1' l2)
      else `Cons (x2, merge cmp l1 l2')

(** {2 Conversions} *)

let rec _to_rev_list acc l = match l() with
  | `Nil -> acc
  | `Cons (x,l') -> _to_rev_list (x::acc) l'

let to_rev_list l = _to_rev_list [] l

let to_list l =
  let rec direct i (l:'a t) = match l () with
    | `Nil -> []
    | _ when i=0 -> List.rev (_to_rev_list [] l)
    | `Cons (x, f) -> x :: direct (i-1) f
  in
  direct 200 l

let of_list l =
  let rec aux l () = match l with
    | [] -> `Nil
    | x::l' -> `Cons (x, aux l')
  in aux l

let rec to_seq res k = match res () with
  | `Nil -> ()
  | `Cons (s, f) -> k s; to_seq f k

let to_gen l =
  let l = ref l in
  fun () ->
    match !l () with
    | `Nil -> None
    | `Cons (x,l') ->
        l := l';
        Some x

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
      | `Nil -> return (of_list (List.rev acc))
      | `Cons (x,l') ->
          f x >>= fun x' ->
          aux (x' :: acc) l'
    in
    aux [] l

  let sequence_m l = map_m (fun x->x) l

  let rec fold_m f acc l = match l() with
    | `Nil -> return acc
    | `Cons (x,l') ->
        f acc x >>= fun acc' -> fold_m f acc' l'
end

(** {2 IO} *)

let pp ?(sep=",") pp_item buf l =
  let rec pp buf l = match l() with
    | `Nil -> ()
    | `Cons (x,l') -> Buffer.add_string buf sep; pp_item buf x; pp buf l'
  in
  match l() with
    | `Nil -> ()
    | `Cons (x,l') -> pp_item buf x; pp buf l'

let print ?(sep=",") pp_item fmt l =
  let rec pp fmt l = match l() with
    | `Nil -> ()
    | `Cons (x,l') -> Format.pp_print_string fmt sep; pp_item fmt x; pp fmt l'
  in
  match l() with
    | `Nil -> ()
    | `Cons (x,l') -> pp_item fmt x; pp fmt l'
