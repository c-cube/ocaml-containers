
(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_

type t = bool

let equal (a:bool) b = Stdlib.(=) a b

let compare (a:bool) b = Stdlib.compare a b

let negate = not

type 'a printer = Format.formatter -> 'a -> unit

let pp = Format.pp_print_bool
