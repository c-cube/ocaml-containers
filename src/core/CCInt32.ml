(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_
include Int32


let min : t -> t -> t = Stdlib.min
let max : t -> t -> t = Stdlib.max

let hash x = Stdlib.abs (to_int x)

let sign i = compare i zero

let pow a b =
  let rec aux acc = function
    | 1l -> acc
    | n ->
      if equal (rem n 2l) zero
      then aux (mul acc acc) (div n 2l)
      else mul acc (aux (mul acc acc) (div n 2l))
  in
  match b with
    | 0l -> if equal a 0l then raise (Invalid_argument "pow: undefined value 0^0") else 1l
    | b when compare b 0l < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
    | b -> aux a b

(*$T
  pow 2l 10l = 1024l
  pow 2l 15l = 32768l
  pow 10l 5l = 100000l
  pow 42l 0l = 1l
  pow 0l 1l = 0l
*)

let floor_div a n =
  if compare a 0l < 0 && compare n 0l >= 0 then
    sub (div (add a 1l) n) 1l
  else if compare a 0l > 0 && compare n 0l < 0 then
    sub (div (sub a 1l) n) 1l
  else
    div a n

(*$T
  (floor_div 3l 5l = 0l)
  (floor_div 5l 5l = 1l)
  (floor_div 20l 5l = 4l)
  (floor_div 12l 5l = 2l)
  (floor_div 0l 5l = 0l)
  (floor_div (-1l) 5l = -1l)
  (floor_div (-5l) 5l = -1l)
  (floor_div (-12l) 5l = -3l)

  (floor_div 0l (-5l) = 0l)
  (floor_div 3l (-5l) = -1l)
  (floor_div 5l (-5l) = -1l)
  (floor_div 9l (-5l) = -2l)
  (floor_div 20l (-5l) = -4l)
  (floor_div (-2l) (-5l) = 0l)
  (floor_div (-8l) (-5l) = 1l)
  (floor_div (-35l) (-5l) = 7l)

  try ignore (floor_div 12l 0l); false with Division_by_zero -> true
  try ignore (floor_div (-12l) 0l); false with Division_by_zero -> true
*)

(*$Q
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat)) \
      (fun (n, m) -> let m = m + 1l in \
                     floor_div n m = of_float @@ floor (to_float n /. to_float m))
  (Q.pair (Q.map of_int Q.small_signed_int) (Q.map of_int Q.small_nat)) \
      (fun (n, m) -> let m = m + 1l in \
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
      up (add i 1l) j yield
    )
  and down i j yield =
    if equal i j then yield i
    else (
      yield i;
      down (sub i 1l) j yield
    )
  in
  if compare i j <= 0 then up i j yield else down i j yield

(*$= & ~printer:Q.Print.(list to_string)
  [0l;1l;2l;3l;4l;5l] (range 0l 5l |> Iter.to_list)
  [0l]                (range 0l 0l |> Iter.to_list)
  [5l;4l;3l;2l]       (range 5l 2l |> Iter.to_list)
*)

let range' i j yield =
  if compare i j < 0 then range i (sub j 1l) yield
  else if equal i j then ()
  else range i (add j 1l) yield

let range_by ~step i j yield =
  let rec range i j yield =
    if equal i j then yield i
    else (
      yield i;
      range (add i step) j yield
    )
  in
  if equal step 0l then
    raise (Invalid_argument "CCInt32.range_by")
  else if (if compare step 0l > 0 then compare i j > 0 else compare i j < 0) then ()
  else range i (add (mul (div (sub j i) step) step) i) yield

(* note: the last test checks that no error occurs due to overflows. *)
(*$= & ~printer:Q.Print.(list to_string)
  [0l]       (range_by ~step:1l   0l 0l      |> Iter.to_list)
  []         (range_by ~step:1l   5l 0l      |> Iter.to_list)
  []         (range_by ~step:2l   1l 0l      |> Iter.to_list)
  [0l;2l;4l] (range_by ~step:2l   0l 4l      |> Iter.to_list)
  [0l;2l;4l] (range_by ~step:2l   0l 5l      |> Iter.to_list)
  [0l]       (range_by ~step:(neg 1l) 0l 0l  |> Iter.to_list)
  []         (range_by ~step:(neg 1l) 0l 5l  |> Iter.to_list)
  []         (range_by ~step:(neg 2l) 0l 1l  |> Iter.to_list)
  [5l;3l;1l] (range_by ~step:(neg 2l) 5l 1l  |> Iter.to_list)
  [5l;3l;1l] (range_by ~step:(neg 2l) 5l 0l  |> Iter.to_list)
  [0l]       (range_by ~step:max_int 0l 2l   |> Iter.to_list)
*)

(*$Q
  Q.(pair (map of_int small_int) (map of_int small_int)) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    CCList.equal CCInt32.equal \
      (CCInt32.range_by ~step:1l i j |> Iter.to_list) \
      (CCInt32.range i j |> Iter.to_list) )
*)

let random n st = Random.State.int32 st n
let random_small = random 100l
let random_range i j st = add i (random (sub j i) st)


(** {2 Conversion} *)

let of_string_exn = of_string

let of_string x = try Some (of_string_exn x) with Failure _ -> None
let of_string_opt = of_string

let most_significant_bit =
  logxor (neg 1l) (shift_right_logical (neg 1l) 1)

type output = char -> unit

(* abstract printer *)
let to_binary_gen (out:output) n =
  let n = if compare n 0l <0 then (out '-'; neg n) else n in
  out '0'; out 'b';
  let rec loop started bit n =
    if equal bit 0l then (
      if not started then out '0'
    ) else (
      let b = logand n bit in
      if equal b 0l then (
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
  "0b111" (to_string_binary 7l)
  "-0b111" (to_string_binary (-7l))
  "0b0" (to_string_binary 0l)
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
