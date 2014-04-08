
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

let record_fix f =
  let rec r = lazy (Fix (fun _ ->
    let descr, builder = f (Lazy.force r) in
    Record (descr, builder)))
  in Lazy.force r

let sum_fix f =
  let rec s = lazy (Fix (fun _ -> Sum (f (Lazy.force s)))) in
  Lazy.force s

(* TODO
let rec_field name ty get cont =
  RecField (name, ty, get, cont)

let rec_yield = RecYield

let sum_case name ty matcher cont =
  SumCase (name, ty, matcher, cont)

let sum_yield = SumYield
*)

(** {2 Some Functions} *)

let rec identity : type a . a ty -> a -> a = function
  | Int -> (fun n -> n+0)
  | String -> (fun s -> s^"")
  | List t -> List.map (identity t)
  | Pair (ta, tb) -> (fun (a, b) -> identity ta a, identity tb b)
  | Record (recty, builder) -> fun record ->
    let rec fid : type b . b -> (b, a) record -> a = fun builder -> function
      | RecYield -> builder
      | RecField (_name, ty, read, rest) ->
        let field = identity ty (read record) in
        fid (builder field) rest
    in fid builder recty
  | Sum { cases = Match (sumty, matcher) } -> fun sum ->
    let rec sid : type m . m -> (m, a, a) sum -> a = fun matcher -> function
      | SumYield -> matcher sum
      | SumCase (_name, ty, constr, rest) ->
        let case = fun param -> constr (identity ty param) in
        sid (matcher case) rest
    in sid matcher sumty
  | (Fix f) as ty -> (fun x -> identity (f ty) x)


(** Attempt to print a type. Will terminate on cyclic types, but only
 * after printing a lot of unreadable stuff *)
let pp fmt ty =
  let rec pp : type a. int -> Format.formatter -> a ty -> unit = fun depth fmt ty ->
    if depth > 10 then Format.pp_print_string fmt "..."
    else match ty with
    | Int -> Format.pp_print_string fmt "int"
    | String -> Format.pp_print_string fmt "string"
    | List ty' ->
        Format.fprintf fmt "@[<>%a@] list" (pp (depth+1)) ty'
    | Pair (tya, tyb) ->
        Format.fprintf fmt "@[(%a * %a)@]" (pp (depth+1)) tya (pp (depth+1)) tyb
    | Record (descr, _) ->
        let first = ref true in
        let rec pp_rec : type b. Format.formatter -> (b, a) record -> unit =
          fun fmt ty -> match ty with
          | RecYield -> ()
          | RecField (name, ty', _get, cont) ->
            if !first then first:=false else Format.pp_print_string fmt ", ";
            Format.fprintf fmt "@[<h>%s: %a@]" name (pp (depth+1)) ty';
            pp_rec fmt cont
        in
        Format.fprintf fmt "{@[<hov>%a@]}" pp_rec descr
    | Sum {cases = Match(sumty, _)} ->
        let rec pp_sum : type m. Format.formatter -> (m, unit, a) sum -> unit =
          fun fmt case -> match case with
          | SumYield -> ()
          | SumCase(name, ty', _, cont) ->
            Format.fprintf fmt "@[<h>| %s -> %a@]" name (pp (depth+1)) ty';
            pp_sum fmt cont
        in
        Format.fprintf fmt "@[<hov2>case %a@]" pp_sum sumty
    | Fix f -> pp depth fmt (f ty)
  in pp 0 fmt ty

(** {2 Tests} *)

type my_record  =
    {
     a: int;
     b: string list;
    }
 
let my_record =
  Record(
    RecField ("a", Int, (fun {a} -> a),
    RecField ("b", List String, (fun {b} -> b),
    RecYield)), fun a b -> {a;b})

type my_sum =
| A of int
| B of string list

let my_sum =
  Sum{ cases = Match(
    SumCase ("a", Int, (fun a -> A a),
    SumCase ("b", List String, (fun b -> B b),
    SumYield)), fun pa pb -> function A a -> pa a | B b -> pb b) }

type lambda =
  | Var of string
  | App of lambda * lambda
  | Lambda of string * lambda

let lambda =
  sum_fix (fun lambda -> {cases=Match(
    SumCase("var", String, (fun s -> Var s),
    SumCase("app", Pair(lambda,lambda), (fun (t1,t2) -> App(t1,t2)),
    SumCase("lambda", Pair(String,lambda), (fun (x,t') -> Lambda(x,t')),
    SumYield))),
    fun pvar papp plambda -> function
      | Var s -> pvar s
      | App (t1,t2) -> papp (t1, t2)
      | Lambda (x, t') -> plambda (x, t'))})

