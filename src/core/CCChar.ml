(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

  @since 0.14 *)

type t = char

let equal (a:char) b = a=b
let compare = Char.compare

let pp = Buffer.add_char
let print = Format.pp_print_char

let lowercase_ascii c =
  if c >= 'A' && c <= 'Z'
  then Char.unsafe_chr (Char. code c + 32)
  else c

let uppercase_ascii c =
  if c >= 'a' && c <= 'z'
  then Char.unsafe_chr (Char.code c - 32)
  else c
