(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_

type t = bool

let equal (a : bool) b = Stdlib.( = ) a b
let compare (a : bool) b = Stdlib.compare a b

let to_int (x : bool) : int =
  if x then
    1
  else
    0

let of_int x : t = x <> 0

type 'a printer = Format.formatter -> 'a -> unit

let pp = Format.pp_print_bool
