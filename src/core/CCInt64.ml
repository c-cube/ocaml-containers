(* This file is free software, part of containers. See file "license" for more details. *)


include Int64

let min : t -> t -> t = Stdlib.min
let max : t -> t -> t = Stdlib.max
let sign i = compare i zero

(* use FNV:
   https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function *)
let hash_to_int64 (n : t) =
  let offset_basis = 0xcbf29ce484222325L in
  let prime = 0x100000001b3L in

  let h = ref offset_basis in
  for k = 0 to 7 do
    h := mul !h prime;
    (* h := h xor (k-th byte of n) *)
    h := logxor !h (logand (shift_right n (k * 8)) 0xffL)
  done;
  logand !h max_int

let[@inline] hash (n : t) : int =
  to_int (hash_to_int64 n) land Stdlib.max_int

(* see {!CCInt.popcount} for more details *)
let[@inline] popcount (b : t) : int =
  let m1 = 0x5555555555555555L in
  let m2 = 0x3333333333333333L in
  let m4 = 0x0f0f0f0f0f0f0f0fL in

  let b = sub b (logand (shift_right_logical b 1) m1) in
  let b = add (logand b m2) (logand (shift_right_logical b 2) m2) in
  let b = logand (add b (shift_right_logical b 4)) m4 in
  let b = add b (shift_right_logical b 8) in
  let b = add b (shift_right_logical b 16) in
  let b = add b (shift_right_logical b 32) in
  let b = logand b 0x7fL in
  to_int b

let pow a b =
  let rec aux acc = function
    | 1L -> acc
    | n ->
      if equal (rem n 2L) zero then
        aux (mul acc acc) (div n 2L)
      else
        mul acc (aux (mul acc acc) (div n 2L))
  in
  match b with
  | 0L ->
    if equal a 0L then
      raise (Invalid_argument "pow: undefined value 0^0")
    else
      1L
  | b when compare b 0L < 0 ->
    raise (Invalid_argument "pow: can't raise int to negative power")
  | b -> aux a b

let floor_div a n =
  if compare a 0L < 0 && compare n 0L >= 0 then
    sub (div (add a 1L) n) 1L
  else if compare a 0L > 0 && compare n 0L < 0 then
    sub (div (sub a 1L) n) 1L
  else
    div a n

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a
type 'a iter = ('a -> unit) -> unit

let range i j yield =
  let rec up i j yield =
    if equal i j then
      yield i
    else (
      yield i;
      up (add i 1L) j yield
    )
  and down i j yield =
    if equal i j then
      yield i
    else (
      yield i;
      down (sub i 1L) j yield
    )
  in
  if compare i j <= 0 then
    up i j yield
  else
    down i j yield

let range' i j yield =
  if compare i j < 0 then
    range i (sub j 1L) yield
  else if equal i j then
    ()
  else
    range i (add j 1L) yield

let range_by ~step i j yield =
  let rec range i j yield =
    if equal i j then
      yield i
    else (
      yield i;
      range (add i step) j yield
    )
  in
  if equal step 0L then
    raise (Invalid_argument "CCInt64.range_by")
  else if
    if compare step 0L > 0 then
      compare i j > 0
    else
      compare i j < 0
  then
    ()
  else
    range i (add (mul (div (sub j i) step) step) i) yield

let random n st = Random.State.int64 st n
let random_small = random 100L
let random_range i j st = add i (random (sub j i) st)

(** {2 Conversion} *)

let of_string_exn = of_string
let of_string x = try Some (of_string_exn x) with Failure _ -> None
let most_significant_bit = logxor (neg 1L) (shift_right_logical (neg 1L) 1)

type output = char -> unit

(* abstract printer *)
let to_binary_gen (out : output) n =
  let n =
    if compare n 0L < 0 then (
      out '-';
      neg n
    ) else
      n
  in
  out '0';
  out 'b';
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

(** {2 Printing} *)

let pp out n = Format.pp_print_string out (to_string n)
let pp_binary out n = to_binary_gen (Format.pp_print_char out) n

(** {2 Infix Operators} *)

module Infix = struct
  let ( + ) = add
  let ( - ) = sub
  let ( ~- ) = neg
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( -- ) = range
  let ( --^ ) = range'
  let ( mod ) = rem
  let ( land ) = logand
  let ( lor ) = logor
  let ( lxor ) = logxor
  let lnot = lognot
  let ( lsl ) = shift_left
  let ( lsr ) = shift_right_logical
  let ( asr ) = shift_right
  let ( = ) = equal
  let ( <> ) = Stdlib.( <> )
  let ( < ) = Stdlib.( < )
  let ( <= ) = Stdlib.( <= )
  let ( > ) = Stdlib.( > )
  let ( >= ) = Stdlib.( >= )
end

include Infix
