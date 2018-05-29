
(* This file is free software, part of containers. See file "license" for more details. *)

type t = int

let equal (a:int) b = Pervasives.(=) a b

let compare a b = compare a b

let leq (a:int) b = Pervasives.(<=) a b

let hash i = i land max_int

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
  [0;1;2;3;4;5] (range 0 5 |> Sequence.to_list)
  [0]           (range 0 0 |> Sequence.to_list)
  [5;4;3;2]     (range 5 2 |> Sequence.to_list)
*)

let range' i j yield =
  if i<j then range i (j-1) yield
  else if i=j then ()
  else range i (j+1) yield

(*$= & ~printer:Q.Print.(list int)
  []          (range' 0 0 |> Sequence.to_list)
  [0;1;2;3;4] (range' 0 5 |> Sequence.to_list)
  [5;4;3]     (range' 5 2 |> Sequence.to_list)
*)

module Infix = struct
  include Pervasives
  let (--) = range
  let (--^) = range'
end
include Infix

let sign i =
  if i < 0 then -1
  else if i>0 then 1
  else 0

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

let bool_neq (a : bool) b = Pervasives.(<>) a b

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
type 'a sequence = ('a -> unit) -> unit

let random n st = Random.State.int st n
let random_small = random 100
let random_range i j st = i + random (j-i) st

let pp fmt = Format.pp_print_int fmt

let most_significant_bit =
  (-1) lxor ((-1) lsr 1)

let to_string = string_of_int

let of_string s =
  try Some (int_of_string s)
  with _ -> None

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
    raise (Invalid_argument "CCList.range_by")
  else if (if step > 0 then i>j else i<j) then ()
  else range i ((j-i)/step*step + i) yield

(* note: the last test checks that no error occurs due to overflows. *)
(*$= & ~printer:Q.Print.(list int)
  [0]     (range_by ~step:1   0 0     |> Sequence.to_list)
  []      (range_by ~step:1   5 0     |> Sequence.to_list)
  []      (range_by ~step:2   1 0     |> Sequence.to_list)
  [0;2;4] (range_by ~step:2   0 4     |> Sequence.to_list)
  [0;2;4] (range_by ~step:2   0 5     |> Sequence.to_list)
  [0]     (range_by ~step:~-1 0 0     |> Sequence.to_list)
  []      (range_by ~step:~-1 0 5     |> Sequence.to_list)
  []      (range_by ~step:~-2 0 1     |> Sequence.to_list)
  [5;3;1] (range_by ~step:~-2 5 1     |> Sequence.to_list)
  [5;3;1] (range_by ~step:~-2 5 0     |> Sequence.to_list)
  [0]     (range_by ~step:max_int 0 2 |> Sequence.to_list)
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    CCList.equal CCInt.equal \
      (CCInt.range_by ~step:1 i j |> Sequence.to_list) \
      (CCInt.range i j |> Sequence.to_list) )
*)
