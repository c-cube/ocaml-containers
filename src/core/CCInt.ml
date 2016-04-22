
(* This file is free software, part of containers. See file "license" for more details. *)

type t = int

let equal (a:int) b = a=b

let compare (a:int) b = Pervasives.compare a b

let hash i = i land max_int

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

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random n st = Random.State.int st n
let random_small = random 100
let random_range i j st = i + random (j-i) st

let pp buf = Printf.bprintf buf "%d"
let print fmt = Format.pp_print_int fmt

let to_string = string_of_int

let of_string s =
  try Some (int_of_string s)
  with _ -> None

module Infix = struct
  let (=) = Pervasives.(=)
  let (<>) = Pervasives.(<>)
  let (<) = Pervasives.(<)
  let (>) = Pervasives.(>)
  let (<=) = Pervasives.(<=)
  let (>=) = Pervasives.(>=)
end
include Infix
let min = min
let max = max
