
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

(** {1 Batch Operations on Collections} *)

module type COLLECTION = sig
  type 'a t

  val empty : 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
end

module type S = sig
  type 'a t

  type ('a,'b) op
  (** Operation that converts an ['a t] into a ['b t] *)

  val apply : ('a,'b) op -> 'a t -> 'b t
  val apply' : 'a t -> ('a,'b) op -> 'b t

  (** {6 Combinators} *)

  val map : ('a -> 'b) -> ('a, 'b) op

  val filter : ('a -> bool) -> ('a,'a) op

  val filter_map : ('a -> 'b option) -> ('a,'b) op

  val flat_map : ('a -> 'b t) -> ('a,'b) op

  val compose : ('b,'c) op -> ('a,'b) op -> ('a,'c) op
  val (>>>) : ('a,'b) op -> ('b,'c) op -> ('a,'c) op
end

module Make(C : COLLECTION) = struct
  type 'a t = 'a C.t
  type (_,_) op =
    | Map : ('a -> 'b) -> ('a, 'b) op
    | Filter : ('a -> bool) -> ('a, 'a) op
    | FilterMap : ('a -> 'b option) -> ('a,'b) op
    | FlatMap : ('a -> 'b t) -> ('a,'b) op
    | Compose : ('a,'b) op * ('b, 'c) op -> ('a, 'c) op

  (* right-associativity *)
  let _compose f g = match f with
    | Compose (f1, f2) -> Compose (f1, Compose (f2, g))
    | _ -> Compose (f, g)

  let compose f g = _compose g f
  let (>>>) f g = _compose f g

  (* function composition *)
  let _compose_fun f g = fun x -> g (f x)

  (* result of one step of optimization, indicates whether the object did
      change or not *)
  type 'a optim_result =
    | Same of 'a
    | New of 'a

  let _new_compose a b = New (Compose (a,b))

  (* optimize a batch operation by fusion *)
  let rec _optimize : type a b. (a,b) op -> (a,b) op
  = fun op -> match op with
    | Compose (a, b) ->
        let a' = _optimize a
        and b' = _optimize b in
        _optimize_rec (Compose (a', b'))
    | op -> op
  (* repeat optimization until a fixpoint is reached *)
  and _optimize_rec : type a b. (a,b) op -> (a,b) op
  = fun op -> match _optimize_head op with
    | Same _ -> op
    | New op' -> _optimize_rec op'
  and _optimize_head : type a b. (a,b) op -> (a,b) op optim_result
  = function
    | Compose (Map f, Compose (Map g, cont)) ->
        _new_compose (Map (fun x -> g (f x))) cont
    | Compose (Filter p, Compose (Map g, cont)) ->
        _new_compose
          (FilterMap (fun x -> if p x then Some (g x) else None)) cont
    | Compose (Filter p, Compose (Filter p', cont)) ->
        _new_compose (Filter (fun x -> p x && p' x)) cont
    | Compose (Filter p, Compose (FlatMap f, cont)) ->
        _new_compose (FlatMap (fun x -> if p x then f x else C.empty)) cont
    | op ->
        Same op  (* cannot optimize *)

  let apply op a =
    let rec _apply : type a b. (a,b) op -> a t -> b t
    = fun op a -> match op with
      | Compose (op1, op2) ->
          let a' = _apply op1 a in
          _apply op2 a'
      | Map f -> C.map f a
      | Filter p -> C.filter p a
      | FlatMap f -> C.flat_map f a
      | FilterMap f -> C.filter_map f a
    in
    (* optimize and run *)
    let op' = _optimize op in
    _apply op' a

  let apply' a op = apply op a

  (** {6 Combinators} *)

  let map f = Map f
  let filter p = Filter p
  let filter_map f = FilterMap f
  let flat_map f = FlatMap f
end

