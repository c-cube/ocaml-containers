(* This file is free software, part of containers. See file "license" for more details. *)

include Bool

let if_then f x =
  if x then
    Some (f ())
  else
    None

let if_then_else f g x =
  if x then
    f ()
  else
    g ()

let of_int x : t = x <> 0

type 'a printer = Format.formatter -> 'a -> unit

let pp = Format.pp_print_bool
