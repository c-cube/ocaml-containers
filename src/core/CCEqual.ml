
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Equality Combinators} *)

open CCShims_

type 'a t = 'a -> 'a -> bool

let poly = Stdlib.(=)
let physical = Stdlib.(==)

let int : int t = (=)
let string : string t = Stdlib.(=)
let bool : bool t = Stdlib.(=)
let float : float t = Stdlib.(=)
let unit () () = true

let rec list f l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1::l1', x2::l2' -> f x1 x2 && list f l1' l2'

let array eq a b =
  let rec aux i =
    if i = Array.length a then true
    else eq a.(i) b.(i) && aux (i+1)
  in
  Array.length a = Array.length b
  &&
  aux 0

let option f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some _, None
  | None, Some _ -> false
  | Some x, Some y -> f x y

let pair f g (x1,y1)(x2,y2) = f x1 x2 && g y1 y2
let triple f g h (x1,y1,z1)(x2,y2,z2) = f x1 x2 && g y1 y2 && h z1 z2

let map f eq x y = eq (f x) (f y)

(*$Q
  Q.(let p = small_list (pair small_int bool) in pair p p) (fun (l1,l2) -> \
    CCEqual.(list (pair int bool)) l1 l2 = (l1=l2))
*)

let always_eq _ _ = true
let never_eq _ _ = false

module Infix = struct
  let (>|=) x f = map f x
end

include Infix
