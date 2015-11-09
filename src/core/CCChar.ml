(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

  @since 0.14 *)

type t = char

let equal (a:char) b = a=b
let compare = Char.compare

let pp = Buffer.add_char
let print = Format.pp_print_char
