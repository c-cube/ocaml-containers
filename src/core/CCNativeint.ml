(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_
include Nativeint


let min : t -> t -> t = Stdlib.min
let max : t -> t -> t = Stdlib.max

let hash x = Stdlib.abs (to_int x)

let sign i = compare i zero

let pow a b =
  let rec aux acc = function
    | 1n -> acc
    | n ->
      if equal (rem n 2n) zero
      then aux (mul acc acc) (div n 2n)
      else mul acc (aux (mul acc acc) (div n 2n))
  in
  match b with
    | 0n -> if equal a 0n then raise (Invalid_argument "pow: undefined value 0^0") else 1n
    | b when compare b 0n < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
    | b -> aux a b

(*$T
  pow 2n 10n = 1024n
  pow 2n 15n = 32768n
  pow 10n 5n = 100000n
  pow 42n 0n = 1n
  pow 0n 1n = 0n
*)

let floor_div a n =
  if compare a 0n < 0 && compare n 0n >= 0 then
    sub (div (add a 1n) n) 1n
  else if compare a 0n > 0 && compare n 0n < 0 then
    sub (div (sub a 1n) n) 1n
  else
    div a n

(*$T
  (floor_div 3n 5n = 0n)
  (floor_div 5n 5n = 1n)
  (floor_div 20n 5n = 4n)
  (floor_div 12n 5n = 2n)
  (floor_div 0n 5n = 0n)
  (floor_div (-1n) 5n = -1n)
  (floor_div (-5n) 5n = -1n)
  (floor_div (-12n) 5n = -3n)

  (floor_div 0n (-5n) = 0n)
  (floor_div 3n (-5n) = -1n)
  (floor_div 5n (-5n) = -1n)
  (floor_div 9n (-5n) = -2n)
  (floor_div 20n (-5n) = -4n)
  (floor_div (-2n) (-5n) = 0n)
  (floor_div (-8n) (-5n) = 1n)
  (floor_div (-35n) (-5n) = 7n)

  try ignore (floor_div 12n 0n); false with Division_by_zero -> true
  try ignore (floor_div (-12n) 0n); false with Division_by_zero -> true
*)

(*$Q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat)) \
      (fun (n, m) -> let m = m + 1n in \
                     floor_div n m = of_float @@ floor (to_float n /. to_float m))
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat)) \
      (fun (n, m) -> let m = m + 1n in \
                     floor_div n (-m) = of_float @@ floor (to_float n /. to_float (-m)))
*)

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a
type 'a iter = ('a -> unit) -> unit

let range i j yield =
  let rec up i j yield =
    if equal i j then yield i
    else (
      yield i;
      up (add i 1n) j yield
    )
  and down i j yield =
    if equal i j then yield i
    else (
      yield i;
      down (sub i 1n) j yield
    )
  in
  if compare i j <= 0 then up i j yield else down i j yield

(*$= & ~printer:Q.Print.(list to_string)
  [0n;1n;2n;3n;4n;5n] (range 0n 5n |> Iter.to_list)
  [0n]                (range 0n 0n |> Iter.to_list)
  [5n;4n;3n;2n]       (range 5n 2n |> Iter.to_list)
*)

let range' i j yield =
  if compare i j < 0 then range i (sub j 1n) yield
  else if equal i j then ()
  else range i (add j 1n) yield

let range_by ~step i j yield =
  let rec range i j yield =
    if equal i j then yield i
    else (
      yield i;
      range (add i step) j yield
    )
  in
  if equal step 0n then
    raise (Invalid_argument "CCNativeint.range_by")
  else if (if compare step 0n > 0 then compare i j > 0 else compare i j < 0) then ()
  else range i (add (mul (div (sub j i) step) step) i) yield

(* note: the last test checks that no error occurs due to overflows. *)
(*$= & ~printer:Q.Print.(list to_string)
  [0n]       (range_by ~step:1n   0n 0n      |> Iter.to_list)
  []         (range_by ~step:1n   5n 0n      |> Iter.to_list)
  []         (range_by ~step:2n   1n 0n      |> Iter.to_list)
  [0n;2n;4n] (range_by ~step:2n   0n 4n      |> Iter.to_list)
  [0n;2n;4n] (range_by ~step:2n   0n 5n      |> Iter.to_list)
  [0n]       (range_by ~step:(neg 1n) 0n 0n  |> Iter.to_list)
  []         (range_by ~step:(neg 1n) 0n 5n  |> Iter.to_list)
  []         (range_by ~step:(neg 2n) 0n 1n  |> Iter.to_list)
  [5n;3n;1n] (range_by ~step:(neg 2n) 5n 1n  |> Iter.to_list)
  [5n;3n;1n] (range_by ~step:(neg 2n) 5n 0n  |> Iter.to_list)
  [0n]       (range_by ~step:max_int 0n 2n   |> Iter.to_list)
*)

(*$Q
  Q.(pair (map of_int small_int) (map of_int small_int)) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    CCList.equal CCNativeint.equal \
      (CCNativeint.range_by ~step:1n i j |> Iter.to_list) \
      (CCNativeint.range i j |> Iter.to_list) )
*)

let random n st = Random.State.nativeint st n
let random_small = random 100n
let random_range i j st = add i (random (sub j i) st)


(** {2 Conversion} *)

let of_string_exn = of_string

let of_string x = try Some (of_string_exn x) with Failure _ -> None
let of_string_opt = of_string

let most_significant_bit =
  logxor (neg 1n) (shift_right_logical (neg 1n) 1)

type output = char -> unit

(* abstract printer *)
let to_binary_gen (out:output) n =
  let n = if compare n 0n <0 then (out '-'; neg n) else n in
  out '0'; out 'b';
  let rec loop started bit n =
    if equal bit 0n then (
      if not started then out '0'
    ) else (
      let b = logand n bit in
      if equal b 0n then (
        if started then out '0';
        loop started (shift_right_logical bit 1) n
      ) else (
        out '1';
        loop true (shift_right_logical bit 1) n
      )
    )
  in
  loop false most_significant_bit n

let to_string_binary n =
  let buf = Buffer.create 16 in
  to_binary_gen (Buffer.add_char buf) n;
  Buffer.contents buf

(*$= & ~printer:CCFun.id
  "0b111" (to_string_binary 7n)
  "-0b111" (to_string_binary (-7n))
  "0b0" (to_string_binary 0n)
*)

(** {2 Printing} *)

let pp out n = Format.pp_print_string out (to_string n)

let pp_binary out n =
  to_binary_gen (Format.pp_print_char out) n


(** {2 Infix Operators} *)

module Infix = struct
  let (+) = add

  let (-) = sub

  let (~-) = neg

  let ( * ) = mul

  let (/) = div

  let ( ** ) = pow

  let (--) = range

  let (--^) = range'

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
