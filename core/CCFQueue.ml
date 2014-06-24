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

(** {1 Functional queues (fifo)} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a equal = 'a -> 'a -> bool

(** {2 Basics} *)

type 'a digit =
  | Zero
  | One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a

type 'a t =
  | Shallow of 'a digit
  | Deep of 'a digit * ('a * 'a) t lazy_t * 'a digit

let empty = Shallow Zero

exception Empty

let _single x = Shallow (One x)
let _double x y = Shallow (Two (x,y))
let _deep hd middle tl =
  assert (hd<>Zero && tl<>Zero);
  Deep (hd, middle, tl)

let is_empty = function
  | Shallow Zero -> true
  | _ -> false

let _empty = Lazy.from_val empty

let rec cons : 'a. 'a -> 'a t -> 'a t
  = fun x q -> match q with
  | Shallow Zero -> _single x
  | Shallow (One y) -> Shallow (Two (x,y))
  | Shallow (Two (y,z)) -> Shallow (Three (x,y,z))
  | Shallow (Three (y,z,z')) ->
      _deep (Two (x,y)) _empty (Two (z,z'))
  | Deep (Zero, middle, tl) -> assert false
  | Deep (One y, middle, tl) -> _deep (Two (x,y)) middle tl
  | Deep (Two (y,z), middle, tl) -> _deep (Three (x,y,z)) middle tl
  | Deep (Three (y,z,z'), lazy q', tail) ->
      _deep (Two (x,y)) (lazy (cons (z,z') q')) tail

let rec snoc : 'a. 'a t -> 'a -> 'a t
  = fun q x -> match q with
  | Shallow Zero -> _single x
  | Shallow (One y) -> Shallow (Two (y,x))
  | Shallow (Two (y,z)) -> Shallow (Three (y,z,x))
  | Shallow (Three (y,z,z')) ->
      _deep (Two (y,z)) _empty (Two (z',x))
  | Deep (hd, middle, Zero) -> assert false
  | Deep (hd, middle, One y) -> _deep hd middle (Two(y,x))
  | Deep (hd, middle, Two (y,z)) -> _deep hd middle (Three(y,z,x))
  | Deep (hd, lazy q', Three (y,z,z')) ->
      _deep hd (lazy (snoc q' (y,z))) (Two(z',x))

let rec take_front_exn : 'a. 'a t -> ('a *'a t)
  = fun q -> match q with
  | Shallow Zero -> raise Empty
  | Shallow (One x) -> x, empty
  | Shallow (Two (x,y)) -> x, Shallow (One y)
  | Shallow (Three (x,y,z)) -> x, Shallow (Two (y,z))
  | Deep (Zero, _, _) -> assert false
  | Deep (One x, lazy q', tail) ->
      if is_empty q'
        then x, Shallow tail
        else
          let (y,z), q' = take_front_exn q' in
          x, _deep (Two (y,z)) (Lazy.from_val q') tail
  | Deep (Two (x,y), middle, tail) ->
      x, _deep (One y) middle tail
  | Deep (Three (x,y,z), middle, tail) ->
      x, _deep (Two(y,z)) middle tail

let take_front q =
  try Some (take_front_exn q)
  with Empty -> None

let take_front_l n q =
  let rec aux acc q n =
    if n=0 || is_empty q then List.rev acc, q
    else
      let x,q' = take_front_exn q in
      aux (x::acc) q' (n-1)
  in aux [] q n

let take_front_while p q =
  let rec aux acc q =
    if is_empty q then List.rev acc, q
    else
      let x,q' = take_front_exn q in
      if p x then aux (x::acc) q' else List.rev acc, q
  in aux [] q

let rec take_back_exn : 'a. 'a t -> 'a t * 'a
  = fun q -> match q with
  | Shallow Zero -> invalid_arg "FQueue.take_back_exn"
  | Shallow (One x) -> empty, x
  | Shallow (Two (x,y)) -> _single x, y
  | Shallow (Three (x,y,z)) -> Shallow (Two(x,y)), z
  | Deep (hd, middle, Zero) -> assert false
  | Deep (hd, lazy q', One x) ->
      if is_empty q'
        then Shallow hd, x
        else
          let q'', (y,z) = take_back_exn q' in
          _deep hd (Lazy.from_val q'') (Two (y,z)), x
  | Deep (hd, middle, Two(x,y)) -> _deep hd middle (One x), y
  | Deep (hd, middle, Three(x,y,z)) -> _deep hd middle (Two (x,y)), z

let take_back q =
  try Some (take_back_exn q)
  with Empty -> None

let take_back_l n q =
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

let init q =
  try fst (take_back_exn q)
  with Empty -> q

let tail q =
  try snd (take_front_exn q)
  with Empty -> q

let add_seq_front seq q =
  let q = ref q in
  seq (fun x -> q := cons x !q);
  !q

let add_seq_back q seq =
  let q = ref q in
  seq (fun x -> q := snoc !q x);
  !q

let _digit_to_seq d k = match d with
  | Zero -> ()
  | One x -> k x
  | Two (x,y) -> k x; k y
  | Three (x,y,z) -> k x; k y; k z

let rec to_seq : 'a. 'a t -> 'a sequence
  = fun q k -> match q with
  | Shallow d -> _digit_to_seq d k
  | Deep (hd, lazy q', tail) ->
      _digit_to_seq hd k;
      to_seq q' (fun (x,y) -> k x; k y);
      _digit_to_seq tail k

let append q1 q2 =
  match q1, q2 with
  | Shallow Zero, _ -> q2
  | _, Shallow Zero -> q1
  | _ -> add_seq_front (to_seq q1) q2

let _map_digit f d = match d with
  | Zero -> Zero
  | One x -> One (f x)
  | Two (x,y) -> Two (f x, f y)
  | Three (x,y,z) -> Three (f x, f y, f z)

let rec map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  = fun f q -> match q with
  | Shallow d -> Shallow (_map_digit f d)
  | Deep (hd, lazy q', tl) ->
      let q'' = map (fun (x,y) -> f x, f y) q' in
      _deep (_map_digit f hd) (Lazy.from_val q'') (_map_digit f tl)

let _size_digit = function
  | Zero -> 0
  | One _ -> 1
  | Two _ -> 2
  | Three _ -> 3

let rec size : 'a. 'a t -> int
  = function
  | Shallow d -> _size_digit d
  | Deep (hd, lazy q', tl) ->
      _size_digit hd + 2 * size q' + _size_digit tl

let (>|=) q f = map f q

let _fold_digit f acc d = match d with
  | Zero -> acc
  | One x -> f acc x
  | Two (x,y) -> f (f acc x) y
  | Three (x,y,z) -> f (f (f acc x) y) z

let rec fold : 'a 'b. ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  = fun f acc q -> match q with
  | Shallow d -> _fold_digit f acc d
  | Deep (hd, lazy q', tl) ->
      let acc = _fold_digit f acc hd in
      let acc = fold (fun acc (x,y) -> f (f acc x) y) acc q' in
      _fold_digit f acc tl

let iter f q = to_seq q f

let of_list l = List.fold_left snoc empty l

let to_list q =
  let l = ref [] in
  to_seq q (fun x -> l := x :: !l);
  List.rev !l

let of_seq seq = add_seq_front seq empty

let _nil () = `Nil
let _single x cont () = `Cons (x, cont)
let _double x y cont () = `Cons (x, _single y cont)
let _triple x y z cont () = `Cons (x, _double y z cont)

let _digit_to_klist d cont = match d with
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
    | Deep (hd, lazy q', tl) ->
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
