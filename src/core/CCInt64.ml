(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_
include Int64

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

  let (<>) = Stdlib.(<>)
  let (<) = Stdlib.(<)
  let (<=) = Stdlib.(<=)
  let (>) = Stdlib.(>)
  let (>=) = Stdlib.(>=)
end

include Infix

let hash x = Stdlib.abs (to_int x)

(** {2 Conversion} *)

let of_string_exn = of_string

let of_string x = try Some (of_string_exn x) with Failure _ -> None
let of_string_opt = of_string
