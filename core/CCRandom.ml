
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

let _choose_array a st =
  if Array.length a = 0 then invalid_arg "CCRandom.choose_array";
  a.(Random.State.int st (Array.length a))

let choose_array a st =
  try Some (_choose_array a st st) with Invalid_argument _ -> None

let choose l =
  let a = Array.of_list l in
  choose_array a

let choose_exn l =
  let a = Array.of_list l in
  fun st -> _choose_array a st st

let choose_return l = _choose_array (Array.of_list l)

let int i st = Random.State.int st i

let small_int = int 100

let int_range i j st = i + Random.State.int st (j-i+1)

let replicate n g st =
  let rec aux acc n =
    if n = 0 then acc else aux (g st :: acc) (n-1)
  in aux [] n

exception SplitFail

let _split i st =
  if i < 2 then raise SplitFail
  else
    let j = 1 + Random.State.int st (i-1) in
    (j, i-j)

let split i st = try Some (_split i st) with SplitFail -> None

(* partition of an int into [len] integers. We divide-and-conquer on
  the expected length, until it reaches 1. *)
let split_list i ~len st =
  let rec aux i ~len acc =
    if i < len then raise SplitFail
    else if len = 1 then i::acc
    else
      (* split somewhere in the middle *)
      let len1, len2 = _split len st in
      assert (len = len1+len2);
      if i = len
      then aux len1 ~len:len1 (aux len2 ~len:len2 acc)
      else
        let i1, i2 = _split (i-len) st in
        aux (i1+len1) ~len:len1 (aux (i2+len2) ~len:len2 acc)
  in
  try Some (aux i ~len []) with SplitFail -> None

let retry ?(max=10) g st =
  let rec aux n =
    match g st with
      | None when n=0 -> None
      | None -> aux (n-1)  (* retry *)
      | Some _ as res -> res
  in
  aux max

let rec try_successively l st = match l with
  | [] -> None
  | g :: l' ->
      begin match g st with
      | None -> try_successively l' st
      | Some _ as res -> res
      end

let (<?>) a b = try_successively [a;b]

exception Backtrack

let _choose_array_call a f st =
  try
    f (_choose_array a st)
  with Invalid_argument _ -> raise Backtrack

let fix ?(sub1=[]) ?(sub2=[]) ?(subn=[]) ~base fuel st =
  let sub1 = Array.of_list sub1
  and sub2 = Array.of_list sub2
  and subn = Array.of_list subn in
  (* recursive function with fuel *)
  let rec make fuel st =
    if fuel=0 then raise Backtrack
    else if fuel=1 then base st
    else
      _try_otherwise 0
        [| _choose_array_call sub1 (fun f -> f (make (fuel-1)) st)
        ;  _choose_array_call sub2
          (fun f ->
            match split fuel st with
            | None -> raise Backtrack
            | Some (i,j) -> f (make i) (make j) st
          )
        ; _choose_array_call subn
          (fun (len,f) ->
            let len = len st in
            match split_list fuel ~len st with
            | None -> raise Backtrack
            | Some l' ->
                f (fun st -> List.map (fun x -> make x st) l') st
          )
        ; base (* base case then *)
        |]
  and _try_otherwise i a =
    if i=Array.length a then raise Backtrack
    else try
      a.(i) st
    with Backtrack ->
      _try_otherwise (i+1) a
  in
  make (fuel st) st

let pure x _st = x

let (<*>) f g st = f st (g st)

let __default_state = Random.State.make_self_init ()

let run ?(st=__default_state) g = g st

