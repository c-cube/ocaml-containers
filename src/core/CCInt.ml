(* This file is free software, part of containers. See file "license" for more details. *)

include Int

type 'a iter = ('a -> unit) -> unit

(* use FNV:
   https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function *)
let hash (n : int) : int =
  let offset_basis = 0xcbf29ce484222325L in
  let prime = 0x100000001b3L in

  let h = ref offset_basis in
  for k = 0 to 7 do
    (h := Int64.(mul !h prime));
    (* h := h xor (k-th byte of n) *)
    h := Int64.(logxor !h (of_int ((n lsr (k * 8)) land 0xff)))
  done;
  (* truncate back to int and remove sign *)
  Int64.to_int !h land max_int

let range i j yield =
  let rec up i j yield =
    if i = j then
      yield i
    else (
      yield i;
      up (i + 1) j yield
    )
  and down i j yield =
    if i = j then
      yield i
    else (
      yield i;
      down (i - 1) j yield
    )
  in
  if i <= j then
    up i j yield
  else
    down i j yield

let range' i j yield =
  if i < j then
    range i (j - 1) yield
  else if i = j then
    ()
  else
    range i (j + 1) yield

let sign i = compare i 0

let pow a b =
  let rec aux acc = function
    | 1 -> acc
    | n ->
      if n mod 2 = 0 then
        aux (acc * acc) (n / 2)
      else
        acc * aux (acc * acc) (n / 2)
  in
  match b with
  | 0 ->
    if a = 0 then
      raise (Invalid_argument "pow: undefined value 0^0")
    else
      1
  | b when b < 0 ->
    raise (Invalid_argument "pow: can't raise int to negative power")
  | b -> aux a b

module Infix : sig
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( -- ) : t -> t -> t iter
  val ( --^ ) : t -> t -> t iter
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
  val ( lxor ) : t -> t -> t
  val lnot : t -> t
  val ( lsl ) : t -> int -> t
  val ( lsr ) : t -> int -> t
  val ( asr ) : t -> int -> t
end = struct
  include Stdlib

  let ( -- ) = range
  let ( --^ ) = range'
  let ( ** ) = pow
end

include Infix

let min : t -> t -> t = Stdlib.min
let max : t -> t -> t = Stdlib.max

let floor_div a n =
  if a < 0 && n >= 0 then
    ((a + 1) / n) - 1
  else if a > 0 && n < 0 then
    ((a - 1) / n) - 1
  else
    a / n

let bool_neq (a : bool) b = Stdlib.( <> ) a b

let rem a n =
  let y = a mod n in
  if bool_neq (y < 0) (n < 0) && y <> 0 then
    y + n
  else
    y

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random n st = Random.State.int st n
let random_small = random 100
let random_range i j st = i + random (j - i) st
let pp fmt = Format.pp_print_int fmt
let most_significant_bit = -1 lxor (-1 lsr 1)
let of_string s = try Some (int_of_string s) with Failure _ -> None
let of_string_exn = Stdlib.int_of_string

type output = char -> unit

(* abstract printer *)
let to_binary_gen (out : output) n =
  let n =
    if n < 0 then (
      out '-';
      -n
    ) else
      n
  in
  out '0';
  out 'b';
  let rec loop started bit n =
    if bit = 0 then (
      if not started then out '0'
    ) else (
      let b = n land bit in
      if b = 0 then (
        if started then out '0';
        loop started (bit lsr 1) n
      ) else (
        out '1';
        loop true (bit lsr 1) n
      )
    )
  in
  loop false most_significant_bit n

let pp_binary out n = to_binary_gen (Format.pp_print_char out) n

let to_string_binary n =
  let buf = Buffer.create 16 in
  to_binary_gen (Buffer.add_char buf) n;
  Buffer.contents buf

let range_by ~step i j yield =
  let rec range i j yield =
    if i = j then
      yield i
    else (
      yield i;
      range (i + step) j yield
    )
  in
  if step = 0 then
    raise (Invalid_argument "CCInt.range_by")
  else if
    if step > 0 then
      i > j
    else
      i < j
  then
    ()
  else
    range i (((j - i) / step * step) + i) yield

(*
  from https://en.wikipedia.org/wiki/Hamming_weight

  //This uses fewer arithmetic operations than any other known
  //implementation on machines with slow multiplication.
  //It uses 17 arithmetic operations.
  int popcount_2(uint64_t x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits
    x += x >>  8;  //put count of each 16 bits into their lowest 8 bits
    x += x >> 16;  //put count of each 32 bits into their lowest 8 bits
    x += x >> 32;  //put count of each 64 bits into their lowest 8 bits
    return x & 0x7f;
  }

   m1 = 0x5555555555555555
   m2 = 0x3333333333333333
   m4 = 0x0f0f0f0f0f0f0f0f
*)
let popcount (b : int) : int =
  let m1 = 0x5555555555555555L in
  let m2 = 0x3333333333333333L in
  let m4 = 0x0f0f0f0f0f0f0f0fL in
  let open Int64 in
  let b = of_int b in
  (* int->int64 *)
  let b = logand b 0x7fffffffffffffffL in

  (* remove sign bit, we deal with uint64 here *)
  let b = sub b (logand (shift_right_logical b 1) m1) in
  let b = add (logand b m2) (logand (shift_right_logical b 2) m2) in
  let b = logand (add b (shift_right_logical b 4)) m4 in
  let b = add b (shift_right_logical b 8) in
  let b = add b (shift_right_logical b 16) in
  let b = add b (shift_right_logical b 32) in
  let b = logand b 0x7fL in
  to_int b
