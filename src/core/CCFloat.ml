
(* This file is free software, part of containers. See file "license" for more details. *)

type t = float
type fpclass = Pervasives.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float

let epsilon = Pervasives.epsilon_float

let is_nan x = (x : t) <> x

let add = (+.)
let sub = (-.)
let neg = (~-.)
let abs = Pervasives.abs_float
let scale = ( *. )

let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

let equal (a:float) b = a=b

let hash = Hashtbl.hash
let compare (a:float) b = Pervasives.compare a b

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let pp buf = Printf.bprintf buf "%f"
let print fmt = Format.pp_print_float fmt

let fsign a =
  if is_nan a then nan
  else if a = 0. then a
  else Pervasives.copysign 1. a

exception TrapNaN of string

let sign_exn (a:float) =
  if is_nan a then raise (TrapNaN "sign_exn")
  else compare a 0.

let round x =
  let low = floor x in
  let high = ceil x in
  if x-.low > high-.x then high else low

(*$=
  2. (round 1.6)
  1. (round 1.4)
  0. (round 0.)
*)

let to_int (a:float) = Pervasives.int_of_float a
let of_int (a:int) = Pervasives.float_of_int a

let to_string (a:float) = Pervasives.string_of_float a
let of_string (a:string) = Pervasives.float_of_string a


let random n st = Random.State.float st n
let random_small = random 100.0
let random_range i j st = i +. random (j-.i) st

let equal_precision ~epsilon a b = abs_float (a-.b) < epsilon

let classify = Pervasives.classify_float

module Infix = struct
  let (=) = Pervasives.(=)
  let (<>) = Pervasives.(<>)
  let (<) = Pervasives.(<)
  let (>) = Pervasives.(>)
  let (<=) = Pervasives.(<=)
  let (>=) = Pervasives.(>=)
end
include Infix
