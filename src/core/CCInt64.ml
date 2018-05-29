(* This file is free software, part of containers. See file "license" for more details. *)

include Int64

let equal (x:int64) y = Pervasives.(=) x y
let leq (a:int64) b = Pervasives.(<=) a b

module Infix = struct
  let (+) = add

  let (-) = sub

  let (~-) = neg

  let ( * ) = mul

  let (/) = div

  let (mod) = rem

  let (land) = logand

  let (lor) = logor

  let (lxor) = logxor

  let lnot = lognot

  let (lsl) = shift_left

  let (lsr) = shift_right_logical

  let (asr) = shift_right

  let (=) = equal

  let (<>) = Pervasives.(<>)
  let (<) = Pervasives.(<)
  let (<=) = Pervasives.(<=)
  let (>) = Pervasives.(>)
  let (>=) = Pervasives.(>=)
end

include Infix

let hash x = Pervasives.abs (to_int x)

(** {2 Conversion} *)

let of_int_exn = of_int
let of_nativeint_exn = of_nativeint
let of_int32_exn = of_int32
let of_float_exn = of_float

let of_string_exn = of_string

let of_string x = try Some (of_string_exn x) with Failure _ -> None
let of_string_opt = of_string
