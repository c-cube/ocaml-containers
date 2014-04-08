
(*
copyright (c) 2014, Simon Cruanes, Gabriel Scherer
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

(** {1 Dynamic Type Representation} *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: ('builder, 'r) record * 'builder -> 'r ty
  | Sum: 's sum_cps -> 's ty
  | Fix : ('a ty -> 'a ty) -> 'a ty
 
and (_, _) record =
  | RecField : string * 'a ty * ('r -> 'a) * ('builder, 'r) record
            -> ('a -> 'builder, 'r) record
  | RecYield : ('r , 'r) record

(* yeah, this is a bit hard to swallow: we need to quantify
   universally over the return type of the pattern-matching, and then
   existentially on the type of the partial matching function   
*)
and 's sum_cps = { cases : 't . ('s, 't) sum_ex }
and ('s, 't) sum_ex = Match : ('matcher, 't, 's) sum * 'matcher -> ('s, 't) sum_ex

and (_, _, _) sum =
  | SumCase: string * 'a ty * ('a -> 's) * ('matcher, 't, 's) sum
          -> (('a -> 't) -> 'matcher, 't, 's) sum
  | SumYield : (('s -> 't), 't, 's) sum

val record_fix : ('a ty -> ('b, 'a) record * 'b) -> 'a ty

val sum_fix : ('a ty -> 'a sum_cps) -> 'a ty

val identity : 'a ty -> 'a -> 'a

val pp : Format.formatter -> _ ty -> unit

(** {2 Tests} *)

type my_record  =
    {
     a: int;
     b: string list;
    }
 
val my_record : my_record ty

type my_sum =
| A of int
| B of string list

val my_sum : my_sum ty

type lambda =
  | Var of string
  | App of lambda * lambda
  | Lambda of string * lambda

val lambda : lambda ty
