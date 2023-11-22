(* This file is free software, part of containers. See file "license" for more details. *)

include Float

module Infix = struct
  let ( = ) : t -> t -> bool = Stdlib.( = )
  let ( <> ) : t -> t -> bool = Stdlib.( <> )
  let ( < ) : t -> t -> bool = Stdlib.( < )
  let ( > ) : t -> t -> bool = Stdlib.( > )
  let ( <= ) : t -> t -> bool = Stdlib.( <= )
  let ( >= ) : t -> t -> bool = Stdlib.( >= )
  let ( ~- ) : t -> t = Stdlib.( ~-. )
  let ( + ) : t -> t -> t = Stdlib.( +. )
  let ( - ) : t -> t -> t = Stdlib.( -. )
  let ( * ) : t -> t -> t = Stdlib.( *. )
  let ( / ) : t -> t -> t = Stdlib.( /. )
end

include Infix

[@@@ocaml.warning "-32"]

let max_value = infinity
let min_value = neg_infinity
let max_finite_value = Stdlib.max_float
let scale = ( *. )

[@@@ocaml.warning "+32"]

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let pp = Format.pp_print_float

let fsign a =
  if is_nan a then
    nan
  else if a = 0. then
    a
  else
    Stdlib.copysign 1. a

exception TrapNaN of string

let sign_exn (a : float) =
  if is_nan a then
    raise (TrapNaN "sign_exn")
  else
    compare a 0.

let of_string_exn (a : string) = Stdlib.float_of_string a

let random n st = Random.State.float st n
let random_small = random 100.0
let random_range i j st = i +. random (j -. i) st
let equal_precision ~epsilon a b = abs_float (a -. b) < epsilon
let classify = Stdlib.classify_float
