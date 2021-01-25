(* This file is free software, part of containers. See file "license" for more details. *)

type 'a iter = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

include CCShimsEither_

let left l = Left l

let right r = Right r

let is_left = function Left _ -> true | Right _ -> false

(*$=
  (is_left (Left 1)) (true)
  (is_left (Right 1)) (false)
*)

let is_right = function Left _ -> false | Right _ -> true

(*$=
  (is_right (Left 1)) (false)
  (is_right (Right 1)) (true)
*)

let find_left = function Left l -> Some l | Right _ -> None

(*$=
  (find_left (Left 1)) (Some 1)
  (find_left (Right 1)) (None)
*)

let find_right = function Left _ -> None | Right r -> Some r

(*$=
  (find_right (Left 1)) (None)
  (find_right (Right 1)) (Some 1)
*)

let map_left f = function Left l -> Left (f l) | Right r -> Right r

let map_right f = function Left l -> Left l | Right r -> Right (f r)

let map ~left ~right = function Left l -> Left (left l) | Right r -> Right (right r)

let fold ~left ~right = function Left l -> left l | Right r -> right r

let iter = fold

let for_all = fold

let equal ~left ~right e1 e2 =
  match e1, e2 with
    | (Left l1, Left l2) -> left l1 l2
    | (Right r1, Right r2) -> right r1 r2
    | _ -> false

let compare ~left ~right e1 e2 =
  match e1, e2 with
    | (Left _, Right _) -> -1
    | (Right _, Left _) -> 1
    | (Left l1, Left l2) -> left l1 l2
    | (Right r1, Right r2) -> right r1 r2

(** {2 IO} *)

let pp ~left ~right fmt = function
  | Left l -> Format.fprintf fmt "Left@ (@[%a@])" left l
  | Right r -> Format.fprintf fmt "Right@ (@[%a@])" right r
