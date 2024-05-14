(* This file is free software, part of containers. See file "license" for more details. *)

type t = float

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

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

let nan = Stdlib.nan
let infinity = Stdlib.infinity
let neg_infinity = Stdlib.neg_infinity
let max_value = infinity
let min_value = neg_infinity
let max_finite_value = Stdlib.max_float
let epsilon = Stdlib.epsilon_float
let pi = 0x1.921fb54442d18p+1
let is_nan x = Stdlib.(classify_float x = Stdlib.FP_nan)
let add = ( +. )
let sub = ( -. )
let mul = ( *. )
let div = ( /. )
let neg = ( ~-. )
let abs = Stdlib.abs_float
let scale = ( *. )

let min (x : t) y =
  match Stdlib.classify_float x, Stdlib.classify_float y with
  | FP_nan, _ -> y
  | _, FP_nan -> x
  | _ ->
    if x < y then
      x
    else
      y

let max (x : t) y =
  match Stdlib.classify_float x, Stdlib.classify_float y with
  | FP_nan, _ -> y
  | _, FP_nan -> x
  | _ ->
    if x > y then
      x
    else
      y

let equal (a : float) b = a = b
let hash : t -> int = Hashtbl.hash
let compare (a : float) b = Stdlib.compare a b

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

let round x =
  let low = floor x in
  let high = ceil x in
  if x -. low > high -. x then
    high
  else
    low

let to_int (a : float) = Stdlib.int_of_float a
let of_int (a : int) = Stdlib.float_of_int a
let to_string (a : float) = Stdlib.string_of_float a
let of_string_exn (a : string) = Stdlib.float_of_string a

let of_string_opt (a : string) =
  try Some (Stdlib.float_of_string a) with Failure _ -> None

let random n st = Random.State.float st n
let random_small = random 100.0
let random_range i j st = i +. random (j -. i) st
let equal_precision ~epsilon a b = abs_float (a -. b) < epsilon
let classify = Stdlib.classify_float
