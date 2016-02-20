
(* This file is free software, part of containers. See file "license" for more details. *)

type t = bool

let equal (a:bool) b = a=b

let compare (a:bool) b = Pervasives.compare a b

let negate x = not x

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

let pp buf = Printf.bprintf buf "%B"
let print fmt = Format.pp_print_bool fmt
