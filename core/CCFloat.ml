(*
copyright (c) 2014, Carmelo Piccione
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

type t = float
type fpclass = Pervasives.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float

let epsilon = Pervasives.epsilon_float

let is_nan x = (x : t) <> x

let add = (+.)
let sub = (-.)
let neg = (~-.)
let abs = Pervasives.abs_float
let scale = ( *. )

let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

let equal (a:float) b = a=b

let hash = Hashtbl.hash 
let compare (a:float) b = Pervasives.compare a b

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let pp buf = Printf.bprintf buf "%f"
let print fmt = Format.pp_print_float fmt

let sign (a:float) = 
  if a < 0.0 then -1 
  else if a > 0.0 then 1 
  else 0

let to_int (a:float) = Pervasives.int_of_float a
let of_int (a:int) = Pervasives.float_of_int a

let to_string (a:float) = Pervasives.string_of_float a
let of_string (a:string) = Pervasives.float_of_string a


let random n st = Random.State.float st n
let random_small = random 100.0
let random_range i j st = i +. random (j-.i) st

let equal_precision ~epsilon a b = abs_float (a-.b) < epsilon

let classify = Pervasives.classify_float

