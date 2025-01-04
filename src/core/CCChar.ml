(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

    @since 0.14 *)

include Char

let pp_buf = Buffer.add_char
let pp = Format.pp_print_char
let of_int_exn = Char.chr
let of_int c = try Some (of_int_exn c) with Invalid_argument _ -> None
let to_int = Char.code
let to_string c = String.make 1 c

module Infix = struct
  let ( = ) : t -> t -> bool = Stdlib.( = )
  let ( <> ) : t -> t -> bool = Stdlib.( <> )
  let ( < ) : t -> t -> bool = Stdlib.( < )
  let ( > ) : t -> t -> bool = Stdlib.( > )
  let ( <= ) : t -> t -> bool = Stdlib.( <= )
  let ( >= ) : t -> t -> bool = Stdlib.( >= )
end

include Infix

let is_uppercase_ascii c = c > '\064' && c < '\091'
let is_lowercase_ascii c = c > '\096' && c < '\123'

let is_letter_ascii c =
  (is_lowercase_ascii [@inlined]) c || (is_uppercase_ascii [@inlined]) c

let is_digit_ascii c = c > '\047' && c < '\058'
let is_whitespace_ascii c = c = '\032' || (c > '\008' && c < '\014')
