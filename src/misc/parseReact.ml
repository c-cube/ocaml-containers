
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

(** {1 Parser combinators driven by the input} *)

type ('a, 'b) t =
  | Return : 'b -> ('a,'b) t
  | Delay : (unit -> ('a, 'b) t) -> ('a, 'b) t
  | One : ('a, 'a) t
  | Stop : ('a, unit) t
  | Bind : ('a, 'b) t * ('b -> ('a, 'c) t) -> ('a, 'c) t
  | Choice : ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
  | Map : ('a, 'b) t * ('b -> 'c) -> ('a, 'c) t
  | Guard : ('a, 'b) t * ('b -> bool) -> ('a, 'b) t
  | Skip : ('a, unit) t
  | IfThenElse: ('a -> bool) * ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
  | Fail : ('a, 'b) t

let stop = Stop

let return x = Return x

let delay f = Delay f

let return' f = Delay (fun () -> return (f ()))

let fail = Fail

let one = One

let skip = Skip

let bind f p = Bind (p, f)

let (>>=) p f = bind f p

let exact ?(eq=(=)) x =
  one
  >>= fun y ->
  if eq x y then Return () else Fail

let guard f p = Guard (p, f)

let (>>) p1 p2 = p1 >>= fun _ -> p2

let map f p = Map (p, f)

let (>>|) p f = Map (p, f)

let (<|>) p1 p2 = Choice (p1, p2)

let pair p1 p2 =
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  return (x1, x2)

let triple p1 p2 p3 =
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  p3 >>= fun x3 ->
  return (x1, x2, x3)

let if_then_else p a b = IfThenElse (p, a, b)

(** {6 Utils} *)

let take_while pred =
  let rec next acc =
    if_then_else pred
      (one >>= fun x -> next (x::acc))
      (return' (fun () -> List.rev acc))
  in
  next []

let take_n n =
  let rec next acc n =
    if n = 0
      then return (List.rev acc)
      else one >>= fun x -> next (x::acc) (n-1)
  in
  next [] n

let skip_spaces =
  let rec next () =
    if_then_else
      (fun c -> c = ' ' || c = '\t' || c = '\n')
      (skip >> delay next)
      (return ())
  in next ()

let ident =
  let accept = function
    | c when Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' -> true
    | c when Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' -> true
    | c when Char.code c >= Char.code '0' && Char.code c <= Char.code '9' -> true
    | _ -> false
  in
  let rec aggregate buf =
    if_then_else
      accept
      (one >>= fun c -> Buffer.add_char buf c; aggregate buf)
      (return (Buffer.contents buf))
  in
  (* create buffer on demand, to avoid sharing it *)
  delay (fun () -> aggregate (Buffer.create 32))

let many ~sep p =
  let rec next acc =
    (return (List.rev acc))
    <|> (p >>= fun x -> sep >> next (x::acc))
  in
  next []

let many1 ~sep p =
  let rec next acc =
    p >>= fun x ->
    let acc = x :: acc in
    (return (List.rev acc))
    <|> (sep >> next acc)
  in
  next []

(** {6 Run} *)

type 'a sequence = ('a -> unit) -> unit

let _fold_seq f acc seq =
  let acc = ref acc in
  seq (fun x -> acc := f !acc x);
  !acc

(** Partial state during parsing: a tree of continuations *)
type (_, _) state =
  | STBottom : 'b -> ('a, 'b) state
  | STPush : ('a, 'c) t * ('c -> ('a, 'b) state list) -> ('a, 'b) state

let (>>>) p cont = STPush (p, cont)

let run p seq =
  (* normalize the stack (do not let a "return" on top) *)
  let rec reduce : type a b. (a,b)state -> (a,b) state list
  = fun stack -> match stack with
    | STPush (Return x, cont) -> CCList.flat_map reduce (cont x)
    | STPush (Delay f, cont) -> reduce (f () >>> cont)
    | STPush (Bind (p, f), cont) ->
        let stack' = p >>> fun x -> [f x >>> cont] in
        reduce stack'
    | STPush (Choice (a, b), cont) ->
        (* fork into sub-stacks *)
        CCList.append (reduce (a >>> cont)) (reduce (b >>> cont))
    | STPush (Map (p, f), cont) ->
        let stack' = p >>> fun x -> cont (f x) in
        reduce stack'
    | STPush (Guard (p, f), cont) ->
        let stack' = p >>> fun x -> if f x then cont x else [] in
        reduce stack'
    | _ -> [stack]
  in
  (* consume one input token *)
  let rec consume_one : type a b. (a,b) state -> a -> (a,b) state list
  = fun stack x -> match stack with
    | STBottom _ -> [] (* fail *)
    | STPush (Stop, _) -> [] (* fail *)
    | STPush (Fail, _) -> [] (* fail *)
    | STPush (One, cont) -> CCList.flat_map reduce (cont x)
    | STPush (Skip, cont) -> CCList.flat_map reduce (cont ())
    | STPush (IfThenElse (p, yay, nay), cont) ->
        let l = if p x
          then reduce (yay >>> cont)
          else reduce (nay >>> cont)
        in
        CCList.flat_map (fun stack -> consume_one stack x) l
    | STPush (Return _, _) -> assert false
    | STPush (Delay _, _) -> assert false
    | STPush (Bind _, _) -> assert false
    | STPush (Choice _, _) -> assert false
    | STPush (Map _, _) -> assert false
    | STPush (Guard _, _) -> assert false
  in
  (* to be called at the end of input *)
  let finish : type a b. (a,b) state -> (a,b) state list
  = fun stack -> match stack with
    | STPush (Stop, cont) -> CCList.flat_map reduce (cont ())
    | STPush (Fail, _) -> []
    | _ -> [stack]
  in
  (* how to parse the input: step by step, starting with [p] as initial parser *)
  let step l x = CCList.flat_map (fun p -> consume_one p x) l in
  let initial_state = p >>> fun x -> [STBottom x] in
  let res = _fold_seq step (reduce initial_state) seq in
  (* signal "end of input" *)
  let res = CCList.flat_map finish res in
  (* recover results *)
  CCList.filter_map
    (function
      | STBottom x -> Some x
      | _ -> None
    ) res


(*$R
  let module S = struct type t = Atom of string | List of t list end in
  let open S in
  let (%) f g x = f (g x) in
  let atom i = Atom i  in
  let list_ i = List i  in
  let rec p () =
    (skip_spaces >> ident >>= (return % atom))
    <|> (skip_spaces >> exact '(' >> many1 ~sep:(exact ' ') (delay p) >>= fun l ->
        skip_spaces >> exact ')' >> return (list_ l))
  in
  let res = run (p ()) (Sequence.of_str "(a b (c d))") in
  assert_equal res [list_ [atom "a"; atom "b"; list_ [atom "c"; atom "d"]]]
*)
