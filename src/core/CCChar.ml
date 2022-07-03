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
  let ( = ) : t -> t -> bool = CCShims_.Stdlib.( = )
  let ( <> ) : t -> t -> bool = CCShims_.Stdlib.( <> )
  let ( < ) : t -> t -> bool = CCShims_.Stdlib.( < )
  let ( > ) : t -> t -> bool = CCShims_.Stdlib.( > )
  let ( <= ) : t -> t -> bool = CCShims_.Stdlib.( <= )
  let ( >= ) : t -> t -> bool = CCShims_.Stdlib.( >= )
end

include Infix
