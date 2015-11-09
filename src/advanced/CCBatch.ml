(*
copyright (c) 2013-2014, Simon Cruanes, Gabriel Radanne
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
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
end

module type S = sig
  type 'a t

  type ('a,'b) op
  (** Operation that converts a ['a t] into a ['b t] *)

  val apply : ('a,'b) op -> 'a t -> 'b t
  (** Apply the operation to the collection. *)

  val apply_fold : ('a, 'b) op -> ('c -> 'b -> 'c) -> 'c -> 'a t -> 'c
  (** Apply the operation plus a fold to the collection. *)

  val apply' : 'a t -> ('a,'b) op -> 'b t
  (** Flip of {!apply} *)

  (** {6 Combinators} *)

  val id : ('a, 'a) op

  val map : ('a -> 'b) -> ('a, 'b) op

  val filter : ('a -> bool) -> ('a,'a) op

  val filter_map : ('a -> 'b option) -> ('a,'b) op

  val flat_map : ('a -> 'b t) -> ('a,'b) op

  val extern : ('a t -> 'b t) -> ('a,'b) op

  val compose : ('b,'c) op -> ('a,'b) op -> ('a,'c) op
  val (>>>) : ('a,'b) op -> ('b,'c) op -> ('a,'c) op
end

module Make(C : COLLECTION) = struct
  type 'a t = 'a C.t
  type (_,_) op =
    | Nil : ('a,'a) op
    | Compose : ('a,'b) base_op * ('b, 'c) op -> ('a, 'c) op
  and (_,_) base_op =
    | Map : ('a -> 'b) -> ('a, 'b) base_op
    | Filter : ('a -> bool) -> ('a, 'a) base_op
    | FilterMap : ('a -> 'b option) -> ('a,'b) base_op
    | FlatMap : ('a -> 'b t) -> ('a,'b) base_op
    | Extern : ('a t -> 'b t) -> ('a,'b) base_op

  (* associativity: put parenthesis on the right *)
  let rec _compose : type a b c. (a,b) op -> (b,c) op -> (a,c) op
  = fun f g -> match f with
    | Compose (f1, Nil) -> Compose (f1, g)
    | Compose (f1, f2) -> Compose (f1, _compose f2 g)
    | Nil -> g

  (* After optimization, the op is a list of flatmaps and external operations,
      with maybe something else at the end *)
  type (_,_) optimized_op =
    | OptNil : ('a, 'a) optimized_op
    | OptBase : ('a,'b) base_op * ('b, 'c) optimized_op -> ('a,'c) optimized_op
    | OptFlatMap : ('a -> 'b t) * ('b, 'c) optimized_op -> ('a, 'c) optimized_op
    | OptExtern : ('a t -> 'b t) * ('b, 'c) optimized_op -> ('a, 'c) optimized_op

  (* As compose, but optimize recursively on the way. *)
  let rec optimize_compose
  : type a b c. (a,b) base_op -> (b,c) op -> (a,c) optimized_op
  = fun base_op op -> match base_op, op with
    | f, Nil -> OptBase (f, OptNil)
    | Map f, Compose (Map g, cont) ->
        optimize_compose (Map (fun x -> g (f x))) cont
    | Map f, Compose (Filter p, cont) ->
        optimize_compose
          (FilterMap (fun x -> let y = f x in if p y then Some y else None)) cont
    | Map f, Compose (FilterMap f', cont) ->
        optimize_compose
          (FilterMap (fun x -> f' (f x))) cont
    | Map f, Compose (FlatMap f', cont) ->
        optimize_compose
          (FlatMap (fun x -> f' (f x))) cont
    | Filter p, Compose (Filter p', cont) ->
        optimize_compose (Filter (fun x -> p x && p' x)) cont
    | Filter p, Compose (Map g, cont) ->
        optimize_compose
          (FilterMap (fun x -> if p x then Some (g x) else None)) cont
    | Filter p, Compose (FilterMap f', cont) ->
        optimize_compose
          (FilterMap (fun x -> if p x then f' x else None)) cont
    | Filter p, Compose (FlatMap f', cont) ->
        optimize_compose
          (FlatMap (fun x -> if p x then f' x else C.empty)) cont
    | FilterMap f, Compose (FilterMap f', cont) ->
        optimize_compose
          (FilterMap
            (fun x -> match f x with None -> None | Some y -> f' y))
          cont
    | FilterMap f, Compose (Filter p, cont) ->
        optimize_compose
          (FilterMap
            (fun x -> match f x with
              | (Some y) as res when p y -> res
              | _ -> None))
          cont
    | FilterMap f, Compose (Map f', cont) ->
        optimize_compose
          (FilterMap
            (fun x -> match f x with
              | None -> None
              | Some y -> Some (f' y)))
          cont
    | FilterMap f, Compose (FlatMap f', cont) ->
        optimize_compose
          (FlatMap
            (fun x -> match f x with
              | None -> C.empty
              | Some y -> f' y))
          cont
    | FlatMap f, Compose (f', tail) ->
        merge_flat_map f (optimize_compose f' tail)
    | Extern f, Compose (f', tail) ->
        OptExtern (f, optimize_compose f' tail)
    | op, Compose (Extern f', cont) ->
        OptBase (op, optimize_compose (Extern f') cont)

  and merge_flat_map
  : type a b c. (a -> b C.t) -> (b,c) optimized_op -> (a,c) optimized_op =
  fun f op -> match op with
    | OptNil -> OptFlatMap (f, op)
    | OptFlatMap (f', cont) ->
        merge_flat_map
          (fun x ->
              let a = f x in
              C.flat_map f' a)
          cont
    | OptExtern  _ -> OptFlatMap (f, op)
    | OptBase _ -> OptFlatMap (f, op)

  (* Optimize a batch operation by fusion *)
  let optimize : type a b. (a,b) op -> (a,b) optimized_op
  = fun op -> match op with
    | Compose (a, b) -> optimize_compose a b
    | Nil -> OptNil

  let rec apply_optimized : type a b. (a,b) optimized_op -> a t -> b t
  = fun op a -> match op with
    | OptNil -> a
    | OptBase (f,c) -> apply_optimized c (apply_base f a)
    | OptFlatMap (f,c) -> apply_optimized c (C.flat_map f a)
    | OptExtern (f,c) -> apply_optimized c (f a)
  and apply_base : type a b. (a,b) base_op -> a t -> b t
  = fun op a -> match op with
    | Map f -> C.map f a
    | Filter p -> C.filter p a
    | FlatMap f -> C.flat_map f a
    | FilterMap f -> C.filter_map f a
    | Extern f -> f a

  let fusion_fold : type a b c. (a,b) base_op -> (c -> b -> c) -> c -> a -> c
  = fun op f' -> match op with
    | Map f -> (fun z x -> f' z (f x))
    | Filter p -> (fun z x -> if p x then f' z x else z)
    | FlatMap f -> (fun z x -> C.fold f' z (f x))
    | FilterMap f -> (fun z x -> match f x with Some x' -> f' z x' | None -> z)
    | Extern _ -> assert false

  let rec apply_optimized_with_fold
  : type a b c. (a,b) optimized_op -> (c -> b -> c) -> c -> a t -> c
  = fun op fold z a -> match op with
    | OptNil -> C.fold fold z a
    | OptBase (Extern f, OptNil) ->
        C.fold fold z (f a)
    | OptBase (f,OptNil) ->
        (* terminal fold *)
        C.fold (fusion_fold f fold) z a
    | OptBase (f,c) ->
        (* make intermediate collection and continue *)
        apply_optimized_with_fold c fold z (apply_base f a)
    | OptExtern (f,c) -> apply_optimized_with_fold c fold z (f a)
    | OptFlatMap (f,c) -> apply_optimized_with_fold c fold z (C.flat_map f a)

  (* Optimize and run *)
  let apply op a =
    let op' = optimize op in
    apply_optimized op' a

  let apply_fold op fold z a =
    let op' = optimize op in
    apply_optimized_with_fold op' fold z a

  let apply' a op = apply op a

  (** {6 Combinators} *)

  let id = Nil
  let map f = Compose (Map f, Nil)
  let filter p = Compose (Filter p, Nil)
  let filter_map f = Compose (FilterMap f, Nil)
  let flat_map f = Compose (FlatMap f, Nil)
  let extern f = Compose (Extern f, Nil)

  let compose f g = _compose g f
  let (>>>) f g = _compose f g
end
