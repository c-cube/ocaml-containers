
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
  | 0 -> if a = 0 then raise (Invalid_argument "Undefined value 0^0") else 1
  | b when b < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
  | b -> aux a b

(*$T
  pow 2 10 = 1024
  pow 2 15 = 32768
  pow 10 5 = 100000
*)

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random n st = Random.State.int st n
let random_small = random 100
let random_range i j st = i + random (j-i) st

let pp buf = Printf.bprintf buf "%d"
let print fmt = Format.pp_print_int fmt
