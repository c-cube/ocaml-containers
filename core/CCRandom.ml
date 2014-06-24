
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Random Generators} *)

type state = Random.State.t

type 'a t = state -> 'a
type 'a random_gen = 'a t

let return x _st = x

let flat_map f g st = f (g st) st

let (>>=) g f st = flat_map f g st

let map f g st = f (g st)

let (>|=) g f st = map f g st

let choose_array a st =
  if Array.length a = 0 then invalid_arg "CCRandom.choose_array";
  a.(Random.State.int st (Array.length a)) st

let choose l = choose_array (Array.of_list l)

let choose_return l = choose_array (Array.of_list (List.map return l))

(** {2 Fuel and Backtracking} *)

module Fuel = struct
  type fuel = int

  exception Backtrack

  (* consume [d] units of fuel and return [x] if it works *)
  let _consume d fuel x =
    if fuel >= d then Some (fuel-d,x) else None

  let _split i st =
    if i < 2 then raise Backtrack
    else
      let j = 1 + Random.State.int st (i-1) in
      (j, i-j)

  let split i st = try Some (_split i st) with Backtrack -> None

  (* partition of an int into [len] integers. We divide-and-conquer on
    the expected length, until it reaches 1. *)
  let split_list i ~len st =
    let rec aux i ~len acc =
      if i < len then raise Backtrack
      else if len = 1 then i::acc
      else
        (* split somewhere in the middle *)
        let len1, len2 = _split len st in
        if i = len
        then aux len1 ~len:len1 (aux len2 ~len:len2 acc)
        else
          let i1, i2 = _split (i-len1-len2) st in
          aux i1 ~len:len1 (aux i2 ~len:len2 acc)
    in
    try Some (aux i ~len []) with Backtrack -> None

  (** {6 Fueled Generators} *)

  type 'a t = fuel -> state -> (fuel * 'a) option

  let return x fuel _st = _consume 1 fuel x

  let return' fuel x fuel' _st = _consume fuel fuel' x

  let flat_map f g fuel st =
    match g fuel st with
      | None -> None
      | Some (fuel, x) -> f x fuel st

  let (>>=) g f = flat_map f g

  let map f g fuel st =
    match g fuel st with
    | None -> None
    | Some (fuel, x) -> Some (fuel, f x)

  let (>|=) g f = map f g

  let consume fuel _st = _consume 1 fuel ()

  let consume' fuel fuel' _st = _consume fuel fuel' ()

  let fail _fuel _st = None

  let retry ?(max=10) g fuel st =
    let rec aux n =
      match g fuel st with
      | None when n=0 -> None
      | None -> aux (n-1)  (* retry *)
      | Some _ as res -> res
    in
    aux max

  let rec try_successively l fuel st = match l with
    | [] -> None
    | g :: l' ->
        begin match g fuel st with
        | None -> try_successively l' fuel st
        | Some _ as res -> res
        end

  let (<?>) a b = try_successively [a;b]

  let rec fix f fuel st = f (fix f) fuel st

  let lift g fuel st = _consume 1 fuel (g st)

  let lift' d g fuel st = _consume d fuel (g st)

  let run ?(fuel=fun st -> Random.State.int st 40) f st =
    match f (fuel st) st with
    | None -> None
    | Some (_fuel, x) -> Some x

  exception GenFailure

  let run_exn ?fuel f st =
    match run ?fuel f st with
    | None -> raise GenFailure
    | Some x -> x
end
