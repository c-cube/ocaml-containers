
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

  val length : (_,_) op -> int
  (** Number of intermediate structures needed to compute this operation *)

  val optimize : ('a,'b) op -> ('a,'b) op
  (** Try to minimize the length of the operation *)

  (** {6 Combinators} *)

  val id : ('a, 'a) op

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
    | Id : ('a,'a) op
    | Compose : ('a,'b) base_op * ('b, 'c) op -> ('a, 'c) op
  and (_,_) base_op =
    | Map : ('a -> 'b) -> ('a, 'b) base_op
    | Filter : ('a -> bool) -> ('a, 'a) base_op
    | FilterMap : ('a -> 'b option) -> ('a,'b) base_op
    | FlatMap : ('a -> 'b t) -> ('a,'b) base_op


  (* associativity: put parenthesis on the right *)
  let rec _compose : type a b c. (a,b) op -> (b,c) op -> (a,c) op
  = fun f g -> match f with
    | Compose (f1, Id) -> Compose (f1, g)
    | Compose (f1, f2) -> Compose (f1, _compose f2 g)
    | Id -> g

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
        let b' = _optimize b in
        _optimize_rec (Compose (a, b'))
    | Id -> Id
  (* repeat optimization until a fixpoint is reached *)
  and _optimize_rec : type a b. (a,b) op -> (a,b) op
  = fun op -> match _optimize_head op with
    | Same _ -> op
    | New op' -> _optimize_rec op'
  and _optimize_head : type a b. (a,b) op -> (a,b) op optim_result
  = function
    | Id -> Same Id
    | Compose (Map f, Compose (Map g, cont)) ->
        _new_compose (Map (fun x -> g (f x))) cont
    | Compose (Map f, Compose (Filter p, cont)) ->
        _new_compose
          (FilterMap (fun x -> let y = f x in if p y then Some y else None)) cont
    | Compose (Map f, Compose (FilterMap f', cont)) ->
        _new_compose
          (FilterMap (fun x -> f' (f x))) cont
    | Compose (Map f, Compose (FlatMap f', cont)) ->
        _new_compose
          (FlatMap (fun x -> f' (f x))) cont
    | Compose (Filter p, Compose (Filter p', cont)) ->
        _new_compose (Filter (fun x -> p x && p' x)) cont
    | Compose (Filter p, Compose (Map g, cont)) ->
        _new_compose
          (FilterMap (fun x -> if p x then Some (g x) else None)) cont
    | Compose (Filter p, Compose (FilterMap f', cont)) ->
        _new_compose
          (FilterMap (fun x -> if p x then f' x else None)) cont
    | Compose (Filter p, Compose (FlatMap f', cont)) ->
        _new_compose
          (FlatMap (fun x -> if p x then f' x else C.empty)) cont
    | Compose (FilterMap f, Compose (FilterMap f', cont)) ->
        _new_compose
          (FilterMap
            (fun x -> match f x with None -> None | Some y -> f' y))
          cont
    | Compose (FilterMap f, Compose (Filter p, cont)) ->
        _new_compose
          (FilterMap
            (fun x -> match f x with
              | (Some y) as res when p y -> res
              | _ -> None))
          cont
    | Compose (FilterMap f, Compose (Map f', cont)) ->
        _new_compose
          (FilterMap
            (fun x -> match f x with
              | None -> None
              | Some y -> Some (f' y)))
          cont
    | Compose (FilterMap f, Compose (FlatMap f', cont)) ->
        _new_compose
          (FlatMap
            (fun x -> match f x with
              | None -> C.empty
              | Some y -> f' y))
          cont
    | (Compose _) as op ->
        Same op  (* cannot optimize *)

  let rec length : type a b. (a,b) op -> int = function
    | Id -> 0
    | Compose (_, Id) -> 0
    | Compose (_, cont) -> 1 + length cont

  let optimize = _optimize

  let apply op a =
    let rec _apply : type a b. (a,b) op -> a t -> b t
    = fun op a -> match op with
      | Compose (op1, op2) ->
          let a' = _apply_base op1 a in
          _apply op2 a'
      | Id -> a
    and _apply_base : type a b. (a,b) base_op -> a t -> b t
    = fun op a -> match op with
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

  let id = Id
  let map f = Compose (Map f, Id)
  let filter p = Compose (Filter p, Id)
  let filter_map f = Compose (FilterMap f, Id)
  let flat_map f = Compose (FlatMap f, Id)

  let compose f g = _compose g f
  let (>>>) f g = _compose f g
end

