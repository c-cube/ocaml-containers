(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

    @since 0.14 *)

include Char

let equal (a:char) b = Pervasives.(=) a b

let pp_buf = Buffer.add_char
let pp = Format.pp_print_char

let of_int_exn = Char.chr
let of_int c = try Some (of_int_exn c) with _ -> None
let to_int = Char.code

let lowercase_ascii = function
  | 'A'..'Z' as c -> Char.unsafe_chr (Char.code c + 32)
  | c -> c

let uppercase_ascii = function
  | 'a'..'z' as c -> Char.unsafe_chr (Char.code c - 32)
  | c -> c
