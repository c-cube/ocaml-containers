
(* This file is free software, part of containers. See file "license" for more details. *)

open CCShims_

include CCShimsInt_

type t = int
type 'a iter = ('a -> unit) -> unit

let zero = 0

let one = 1

let minus_one = -1

let add = (+)

let sub = (-)

let mul = ( * )

let div = (/)

let succ = succ

let pred = pred

let abs = abs

let max_int = max_int

let min_int = min_int

let equal (a:int) b = Stdlib.(=) a b

let compare (a:int) b = compare a b

(* use FNV:
   https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function *)
let hash (n:int) : int =
  let offset_basis = 0xcbf29ce484222325L in
  let prime = 0x100000001b3L in

  let h = ref offset_basis in
  for k = 0 to 7 do
    h := Int64.(mul !h prime);
    (* h := h xor (k-th bit of n) *)
    h := Int64.(logxor !h (of_int ((n lsr (k * 8)) land 0xff)));
  done;
  (Int64.to_int !h) land max_int (* truncate back to int and remove sign *)

let range i j yield =
  let rec up i j yield =
    if i=j then yield i
    else (
      yield i;
      up (i+1) j yield
    )
  and down i j yield =
    if i=j then yield i
    else (
      yield i;
      down (i-1) j yield
    )
  in
  if i<=j then up i j yield else down i j yield

(*$= & ~printer:Q.Print.(list int)
  [0;1;2;3;4;5] (range 0 5 |> Iter.to_list)
  [0]           (range 0 0 |> Iter.to_list)
  [5;4;3;2]     (range 5 2 |> Iter.to_list)
*)

let range' i j yield =
  if i<j then range i (j-1) yield
  else if i=j then ()
  else range i (j+1) yield

(*$= & ~printer:Q.Print.(list int)
  []          (range' 0 0 |> Iter.to_list)
  [0;1;2;3;4] (range' 0 5 |> Iter.to_list)
  [5;4;3]     (range' 5 2 |> Iter.to_list)
*)

let sign i = compare i 0

let neg i = -i

let pow a b =
  let rec aux acc = function
    | 1 -> acc
    | n ->
      if n mod 2 = 0
      then aux (acc*acc) (n/2)
      else acc * (aux (acc*acc) (n/2))
  in
  match b with
    | 0 -> if a = 0 then raise (Invalid_argument "pow: undefined value 0^0") else 1
    | b when b < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
    | b -> aux a b

(*$T
  pow 2 10 = 1024
  pow 2 15 = 32768
  pow 10 5 = 100000
  pow 1 0 = 1
  pow 0 1 = 0
*)

module Infix : sig
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (>) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (--) : t -> t -> t iter
  val (--^) : t -> t -> t iter
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (~-) : t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val ( ** ) : t -> t -> t
  val (mod) : t -> t -> t
  val (land) : t -> t -> t
  val (lor) : t -> t -> t
  val (lxor) : t -> t -> t
  val lnot : t -> t
  val (lsl) : t -> int -> t
  val (lsr) : t -> int -> t
  val (asr) : t -> int -> t
end = struct
  include Stdlib
  let (--) = range
  let (--^) = range'
  let ( ** ) = pow
end
include Infix

let min : t -> t -> t = Stdlib.min
let max : t -> t -> t = Stdlib.max

let floor_div a n =
  if a < 0 && n >= 0 then
    (a + 1) / n - 1
  else if a > 0 && n < 0 then
    (a - 1) / n - 1
  else
    a / n

(*$T
  (floor_div 3 5 = 0)
  (floor_div 5 5 = 1)
  (floor_div 20 5 = 4)
  (floor_div 12 5 = 2)
  (floor_div 0 5 = 0)
  (floor_div (-1) 5 = -1)
  (floor_div (-5) 5 = -1)
  (floor_div (-12) 5 = -3)

  (floor_div 0 (-5) = 0)
  (floor_div 3 (-5) = -1)
  (floor_div 5 (-5) = -1)
  (floor_div 9 (-5) = -2)
  (floor_div 20 (-5) = -4)
  (floor_div (-2) (-5) = 0)
  (floor_div (-8) (-5) = 1)
  (floor_div (-35) (-5) = 7)

  try ignore (floor_div 12 0); false with Division_by_zero -> true
  try ignore (floor_div (-12) 0); false with Division_by_zero -> true
*)

(*$Q
  (Q.pair Q.small_signed_int Q.pos_int) \
      (fun (n, m) -> floor_div n m = int_of_float @@ floor (float n /. float m))
  (Q.pair Q.small_signed_int Q.pos_int) \
      (fun (n, m) -> floor_div n (-m) = int_of_float @@ floor (float n /. float (-m)))
*)

let bool_neq (a : bool) b = Stdlib.(<>) a b

let rem a n =
  let y = a mod n in
  if bool_neq (y < 0) (n < 0) && y <> 0 then
    y + n
  else
    y

(*$T
  (rem 3 5 = 3)
  (rem 5 5 = 0)
  (rem 9 5 = 4)
  (rem (-1) 5 = 4)
  (rem (-5) 5 = 0)
  (rem (-20) 5 = 0)
  (rem (-9) 5 = 1)
  (rem 0 5 = 0)

  (rem 0 (-5) = 0)
  (rem 3 (-5) = -2)
  (rem 5 (-5) = 0)
  (rem 9 (-5) = -1)
  (rem (-2) (-5) = -2)
  (rem (-8) (-5) = -3)
  (rem (-35) (-5) = 0)

  try ignore (rem 12 0); false with Division_by_zero -> true
  try ignore (rem (-12) 0); false with Division_by_zero -> true
*)

(*$Q
  (Q.pair Q.int Q.pos_int) (fun (n, m) -> let y = rem n m in y >= 0 && y < m)
  (Q.pair Q.int Q.pos_int) (fun (n, m) -> let y = rem n (-m) in y > (-m) && y <= 0)
*)

(*$Q
  (Q.pair Q.int Q.pos_int) (fun (n, m) -> n = m * floor_div n m + rem n m)
  (Q.pair Q.int Q.pos_int) (fun (n, m) -> n = (-m) * floor_div n (-m) + rem n (-m))
*)

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random n st = Random.State.int st n
let random_small = random 100
let random_range i j st = i + random (j-i) st

let pp fmt = Format.pp_print_int fmt

let most_significant_bit =
  (-1) lxor ((-1) lsr 1)

let to_string = string_of_int

let of_string s =
  try Some (int_of_string s)
  with Failure _ -> None

(*$=
  None (of_string "moo")
  (Some 42) (of_string "42")
*)

let of_string_exn = Stdlib.int_of_string

let to_float = float_of_int

let of_float = int_of_float

(*$=
  1 (of_float 1.2)
*)

type output = char -> unit

(* abstract printer *)
let to_binary_gen (out:output) n =
  let n = if n<0 then (out '-'; -n) else n in
  out '0'; out 'b';
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

let pp_binary out n =
  to_binary_gen (Format.pp_print_char out) n

let to_string_binary n =
  let buf = Buffer.create 16 in
  to_binary_gen (Buffer.add_char buf) n;
  Buffer.contents buf

(*$= & ~printer:CCFun.id
  "0b111" (to_string_binary 7)
  "-0b111" (to_string_binary (-7))
  "0b0" (to_string_binary 0)
*)


(*$Q & ~count:10_000
  Q.int (fun n -> n = int_of_string (to_string_binary n))
*)

let range_by ~step i j yield =
  let rec range i j yield =
    if i=j then yield i
    else (
      yield i;
      range (i+step) j yield
    )
  in
  if step = 0 then
    raise (Invalid_argument "CCInt.range_by")
  else if (if step > 0 then i>j else i<j) then ()
  else range i ((j-i)/step*step + i) yield

(* note: the last test checks that no error occurs due to overflows. *)
(*$= & ~printer:Q.Print.(list int)
  [0]     (range_by ~step:1   0 0     |> Iter.to_list)
  []      (range_by ~step:1   5 0     |> Iter.to_list)
  []      (range_by ~step:2   1 0     |> Iter.to_list)
  [0;2;4] (range_by ~step:2   0 4     |> Iter.to_list)
  [0;2;4] (range_by ~step:2   0 5     |> Iter.to_list)
  [0]     (range_by ~step:~-1 0 0     |> Iter.to_list)
  []      (range_by ~step:~-1 0 5     |> Iter.to_list)
  []      (range_by ~step:~-2 0 1     |> Iter.to_list)
  [5;3;1] (range_by ~step:~-2 5 1     |> Iter.to_list)
  [5;3;1] (range_by ~step:~-2 5 0     |> Iter.to_list)
  [0]     (range_by ~step:max_int 0 2 |> Iter.to_list)
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    CCList.equal CCInt.equal \
      (CCInt.range_by ~step:1 i j |> Iter.to_list) \
      (CCInt.range i j |> Iter.to_list) )
*)

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
let popcount (b:int) : int =
  let m1 = 0x5555555555555555L in
  let m2 = 0x3333333333333333L in
  let m4 = 0x0f0f0f0f0f0f0f0fL in
  let open Int64 in

  let b = of_int b in (* int->int64 *)
  let b = logand b 0x7fffffffffffffffL in (* remove sign bit, we deal with uint64 here *)

  let b = sub b (logand (shift_right_logical b 1) m1) in
  let b = add (logand b m2) (logand (shift_right_logical b 2) m2) in
  let b = logand (add b (shift_right_logical b 4)) m4 in
  let b = add b (shift_right_logical b 8) in
  let b = add b (shift_right_logical b 16) in
  let b = add b (shift_right_logical b 32) in
  let b = logand b 0x7fL in
  to_int b

(*$=
  0 (popcount 0)
  1 (popcount 1)
  (Sys.word_size-2) (popcount max_int)
  1 (popcount min_int)
  10 (popcount 0b1110010110110001010)
  5 (popcount 0b1101110000000000)
*)

(*$inject
  let simple_popcnt i =
    let rec loop n i =
      if i=0 then n
      else if i land 0b1 = 1 then loop (n+1) (i lsr 1)
      else loop n (i lsr 1)
    in
    loop 0 i
*)

(*$=
  0 (simple_popcnt 0)
  1 (simple_popcnt 1)
  (Sys.word_size-2) (simple_popcnt max_int)
  1 (simple_popcnt min_int)
  5 (simple_popcnt 0b1101110000000000)
*)

(*$QR & ~count:3_000 ~long_factor:10
     Q.(let g = int in
        set_gen (Gen.graft_corners g.gen [min_int; max_int; 0; -1; 1] ()) g)
     (fun i ->
      if simple_popcnt i <> popcount i then (
        Q.Test.fail_reportf "on %d: simple-popcount=%d, popcount=%d"
          i (simple_popcnt i) (popcount i)
      );
      true)
    *)

let logand = (land)

let logor = (lor)

let logxor = (lxor)

let lognot = lnot

let shift_left = (lsl)

let shift_right = (asr)

let shift_right_logical = (lsr)
