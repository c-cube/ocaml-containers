(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Comparisons} *)

type 'a t = 'a -> 'a -> int
(** Comparison (total ordering) between two elements, that returns an int *)

let poly = Stdlib.compare
let compare = Stdlib.compare
let opp f x y = -f x y

let equiv i j =
  if i < 0 then
    j < 0
  else if i > 0 then
    j > 0
  else
    j = 0

let int (x : int) y = Stdlib.compare x y
let string (x : string) y = Stdlib.compare x y
let bool (x : bool) y = Stdlib.compare x y
let float (x : float) y = Stdlib.compare x y

(** {2 Lexicographic Combination} *)

let ( <?> ) c (ord, x, y) =
  if c = 0 then
    ord x y
  else
    c

let option c o1 o2 =
  match o1, o2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x1, Some x2 -> c x1 x2

let pair o_x o_y (x1, y1) (x2, y2) =
  let c = o_x x1 x2 in
  if c = 0 then
    o_y y1 y2
  else
    c

let triple o_x o_y o_z (x1, y1, z1) (x2, y2, z2) =
  let c = o_x x1 x2 in
  if c = 0 then (
    let c' = o_y y1 y2 in
    if c' = 0 then
      o_z z1 z2
    else
      c'
  ) else
    c

let rec list ord l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1 :: l1', x2 :: l2' ->
    let c = ord x1 x2 in
    if c = 0 then
      list ord l1' l2'
    else
      c

let array ord a1 a2 =
  let rec aux i =
    if i = Array.length a1 then
      if Array.length a1 = Array.length a2 then
        0
      else
        -1
    else if i = Array.length a2 then
      1
    else (
      let c = ord a1.(i) a2.(i) in
      if c = 0 then
        aux (i + 1)
      else
        c
    )
  in
  aux 0

let map f ord a b = ord (f a) (f b)
let ( >|= ) x f = map f x

module Infix = struct
  let ( >|= ) = ( >|= )
  let ( <?> ) = ( <?> )
end
