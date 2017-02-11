(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

    @since 0.14 *)

type t = char

let equal (a:char) b = a=b
let compare = Char.compare

let pp = Buffer.add_char
let print = Format.pp_print_char

let of_int_exn = Char.chr
let of_int c = try Some (of_int_exn c) with _ -> None
let to_int = Char.code

let lowercase_ascii c =
  if c >= 'A' && c <= 'Z'
  then Char.unsafe_chr (Char. code c + 32)
  else c

let uppercase_ascii c =
  if c >= 'a' && c <= 'z'
  then Char.unsafe_chr (Char.code c - 32)
  else c
