(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_
include Int64


let min : t -> t -> t = Stdlib.min
let max : t -> t -> t = Stdlib.max

let hash x = Stdlib.abs (to_int x)

let sign i = compare i zero

let pow a b =
  let rec aux acc = function
    | 1L -> acc
    | n ->
      if equal (rem n 2L) zero
      then aux (mul acc acc) (div n 2L)
      else mul acc (aux (mul acc acc) (div n 2L))
  in
  match b with
    | 0L -> if equal a 0L then raise (Invalid_argument "pow: undefined value 0^0") else 1L
    | b when compare b 0L < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
    | b -> aux a b

(*$T
  pow 2L 10L = 1024L
  pow 2L 15L = 32768L
  pow 10L 5L = 100000L
  pow 42L 0L = 1L
  pow 0L 1L = 0L
*)

let floor_div a n =
  if compare a 0L < 0 && compare n 0L >= 0 then
    sub (div (add a 1L) n) 1L
  else if compare a 0L > 0 && compare n 0L < 0 then
    sub (div (sub a 1L) n) 1L
  else
    div a n

(*$T
  (floor_div 3L 5L = 0L)
  (floor_div 5L 5L = 1L)
  (floor_div 20L 5L = 4L)
  (floor_div 12L 5L = 2L)
  (floor_div 0L 5L = 0L)
  (floor_div (-1L) 5L = -1L)
  (floor_div (-5L) 5L = -1L)
  (floor_div (-12L) 5L = -3L)

  (floor_div 0L (-5L) = 0L)
  (floor_div 3L (-5L) = -1L)
  (floor_div 5L (-5L) = -1L)
  (floor_div 9L (-5L) = -2L)
  (floor_div 20L (-5L) = -4L)
  (floor_div (-2L) (-5L) = 0L)
  (floor_div (-8L) (-5L) = 1L)
  (floor_div (-35L) (-5L) = 7L)

  try ignore (floor_div 12L 0L); false with Division_by_zero -> true
  try ignore (floor_div (-12L) 0L); false with Division_by_zero -> true
*)

(*$Q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat)) \
      (fun (n, m) -> let m = m + 1L in \
                     floor_div n m = of_float @@ floor (to_float n /. to_float m))
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat)) \
      (fun (n, m) -> let m = m + 1L in \
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
      up (add i 1L) j yield
    )
  and down i j yield =
    if equal i j then yield i
    else (
      yield i;
      down (sub i 1L) j yield
    )
  in
  if compare i j <= 0 then up i j yield else down i j yield

(*$= & ~printer:Q.Print.(list to_string)
  [0L;1L;2L;3L;4L;5L] (range 0L 5L |> Iter.to_list)
  [0L]                (range 0L 0L |> Iter.to_list)
  [5L;4L;3L;2L]       (range 5L 2L |> Iter.to_list)
*)

let range' i j yield =
  if compare i j < 0 then range i (sub j 1L) yield
  else if equal i j then ()
  else range i (add j 1L) yield

let range_by ~step i j yield =
  let rec range i j yield =
    if equal i j then yield i
    else (
      yield i;
      range (add i step) j yield
    )
  in
  if equal step 0L then
    raise (Invalid_argument "CCInt64.range_by")
  else if (if compare step 0L > 0 then compare i j > 0 else compare i j < 0) then ()
  else range i (add (mul (div (sub j i) step) step) i) yield

(* note: the last test checks that no error occurs due to overflows. *)
(*$= & ~printer:Q.Print.(list to_string)
  [0L]       (range_by ~step:1L   0L 0L      |> Iter.to_list)
  []         (range_by ~step:1L   5L 0L      |> Iter.to_list)
  []         (range_by ~step:2L   1L 0L      |> Iter.to_list)
  [0L;2L;4L] (range_by ~step:2L   0L 4L      |> Iter.to_list)
  [0L;2L;4L] (range_by ~step:2L   0L 5L      |> Iter.to_list)
  [0L]       (range_by ~step:(neg 1L) 0L 0L  |> Iter.to_list)
  []         (range_by ~step:(neg 1L) 0L 5L  |> Iter.to_list)
  []         (range_by ~step:(neg 2L) 0L 1L  |> Iter.to_list)
  [5L;3L;1L] (range_by ~step:(neg 2L) 5L 1L  |> Iter.to_list)
  [5L;3L;1L] (range_by ~step:(neg 2L) 5L 0L  |> Iter.to_list)
  [0L]       (range_by ~step:max_int 0L 2L   |> Iter.to_list)
*)

(*$Q
  Q.(pair (map of_int small_int) (map of_int small_int)) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    CCList.equal CCInt64.equal \
      (CCInt64.range_by ~step:1L i j |> Iter.to_list) \
      (CCInt64.range i j |> Iter.to_list) )
*)

let random n st = Random.State.int64 st n
let random_small = random 100L
let random_range i j st = add i (random (sub j i) st)


(** {2 Conversion} *)

let of_string_exn = of_string

let of_string x = try Some (of_string_exn x) with Failure _ -> None
let of_string_opt = of_string

let most_significant_bit =
  logxor (neg 1L) (shift_right_logical (neg 1L) 1)

type output = char -> unit

(* abstract printer *)
let to_binary_gen (out:output) n =
  let n = if compare n 0L <0 then (out '-'; neg n) else n in
  out '0'; out 'b';
  let rec loop started bit n =
    if equal bit 0L then (
      if not started then out '0'
    ) else (
      let b = logand n bit in
      if equal b 0L then (
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
  "0b111" (to_string_binary 7L)
  "-0b111" (to_string_binary (-7L))
  "0b0" (to_string_binary 0L)
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
