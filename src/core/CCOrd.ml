
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Comparisons} *)

open CCShims_

type 'a t = 'a -> 'a -> int
(** Comparison (total ordering) between two elements, that returns an int *)

let compare = Stdlib.compare

let opp f x y = - (f x y)

let equiv i j =
  if i<0 then j<0
  else if i>0 then j>0
  else j=0

(*$T
  equiv 1 2
  equiv ~-1 ~-10
  equiv 0 0
  equiv ~-1 ~-1
  not (equiv 0 1)
  not (equiv 1 ~-1)
  not (equiv 1 0)
*)

(*$Q
  Q.(pair int int) (fun (x,y) -> \
    (equiv x y) = (equiv y x))
  Q.(triple int int int) (fun (x,y,z) -> \
    if (equiv x y && equiv y z) then (equiv x z) else true)
*)

let int (x:int) y = Stdlib.compare x y
let string (x:string) y = Stdlib.compare x y
let bool (x:bool) y = Stdlib.compare x y
let float (x:float) y = Stdlib.compare x y

(*$T
  bool true false > 0
  bool false true < 0
  bool true true = 0
  bool false false = 0
*)

(** {2 Lexicographic Combination} *)

let (<?>) c (ord,x,y) =
  if c = 0
  then ord x y
  else c

let option c o1 o2 = match o1, o2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x1, Some x2 -> c x1 x2

(*$Q
  Q.(option int) (fun o -> option int None o <= 0)
*)

let pair o_x o_y (x1,y1) (x2,y2) =
  let c = o_x x1 x2 in
  if c = 0
  then o_y y1 y2
  else c

(*$T
  pair int string (1, "b") (2, "a") < 0
  pair int string (1, "b") (0, "a") > 0
  pair int string (1, "b") (1, "b") = 0
*)

let triple o_x o_y o_z (x1,y1,z1) (x2,y2,z2) =
  let c = o_x x1 x2 in
  if c = 0
  then
    let c' = o_y y1 y2 in
    if c' = 0
    then o_z z1 z2
    else c'
  else c

let rec list ord l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1', x2::l2' ->
    let c = ord x1 x2 in
    if c = 0
    then list ord l1' l2'
    else c

(*$T
  list int [1;2;3] [1;2;3;4] < 0
  list int [1;2;3;4] [1;2;3] > 0
  list int [1;2;3;4] [1;3;4] < 0
*)

(*$Q
  Q.(pair (list int)(list int)) CCOrd.(fun (l1,l2) -> \
    equiv (list int l1 l2) (Stdlib.compare l1 l2))
*)

let array ord a1 a2 =
  let rec aux i =
    if i = Array.length a1
    then if Array.length a1 = Array.length a2 then 0
      else -1
    else if i = Array.length a2
    then 1
    else
      let c = ord a1.(i) a2.(i) in
      if c = 0
      then aux (i+1) else c
  in
  aux 0

(*$T
  array int [|1;2;3|] [|1;2;3;4|] < 0
  array int [|1;2;3;4|] [|1;2;3|] > 0
  array int [|1;2;3;4|] [|1;3;4|] < 0
*)

(*$Q & ~small:(fun (a1, a2) -> Array.length a1+Array.length a2)
  Q.(pair (array int)(array int)) CCOrd.(fun (a1,a2) -> \
    equiv (array int a1 a2) (list int (Array.to_list a1) (Array.to_list a2)))
*)

let map f ord a b = ord (f a) (f b)

let (>|=) x f = map f x

module Infix = struct
  let (>|=) = (>|=)
  let (<?>) = (<?>)
end
