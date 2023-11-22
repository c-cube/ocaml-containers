(* This file is free software, part of containers. See file "license" for more details. *)


include Bool

let of_int x : t = x <> 0

type 'a printer = Format.formatter -> 'a -> unit

let pp = Format.pp_print_bool
