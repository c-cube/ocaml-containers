(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Leftist Heaps} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

module type PARTIAL_ORD = sig
  type t
  val leq : t -> t -> bool
  (** [leq x y] shall return [true] iff [x] is lower or equal to [y] *)
end

module type S = sig
  type elt
  type t

  val empty : t
  (** Empty heap *)

  val is_empty : t -> bool
  (** Is the heap empty? *)

  exception Empty

  val merge : t -> t -> t
  (** Merge two heaps *)

  val insert : elt -> t -> t
  (** Insert a value in the heap *)

  val add : t -> elt -> t
  (** Synonym to {!insert} *)

  val filter :  (elt -> bool) -> t -> t
  (** Filter values, only retaining the ones that satisfy the predicate.
      Linear time at least. *)

  val find_min : t -> elt option
  (** Find minimal element *)

  val find_min_exn : t -> elt
  (** Same as {!find_min} but can fail
      @raise Empty if the heap is empty *)

  val take : t -> (t * elt) option
  (** Extract and return the minimum element, and the new heap (without
      this element), or [None] if the heap is empty *)

  val take_exn : t -> t * elt
  (** Same as {!take}, but can fail.
      @raise Empty if the heap is empty *)

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on elements *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Fold on all values *)

  val size : t -> int
  (** Number of elements (linear complexity) *)

  (** {2 Conversions} *)

  val to_list : t -> elt list
  val of_list : elt list -> t

  val of_seq : t -> elt sequence -> t
  val to_seq : t -> elt sequence

  val of_klist : t -> elt klist -> t
  val to_klist : t -> elt klist

  val of_gen : t -> elt gen -> t
  val to_gen : t -> elt gen

  val to_tree : t -> elt ktree
end

module Make(E : PARTIAL_ORD) = struct
  type elt = E.t

  type t =
    | E
    | N of int * elt * t * t

  let empty = E

  let is_empty = function
    | E -> true
    | N _ -> false

  exception Empty

  (* Rank of the tree *)
  let _rank = function
    | E -> 0
    | N (r, _, _, _) -> r

  (* Make a balanced node labelled with [x], and subtrees [a] and [b].
    We ensure that the right child's rank is ≤ to the rank of the
    left child (leftist property). The rank of the resulting node
    is the length of the rightmost path. *)
  let _make_node x a b =
    if _rank a >= _rank b
      then N (_rank b + 1, x, a, b)
      else N (_rank a + 1, x, b, a)

  let rec merge t1 t2 =
    match t1, t2 with
    | t, E -> t
    | E, t -> t
    | N (_, x, a1, b1), N (_, y, a2, b2) ->
      if E.leq x y
        then _make_node x a1 (merge b1 t2)
        else _make_node y a2 (merge t1 b2)

  let insert x h =
    merge (N(1,x,E,E)) h

  let add h x = insert x h

  let rec filter p h = match h with
    | E -> E
    | N(_, x, l, r) when p x -> _make_node x (filter p l) (filter p r)
    | N(_, _, l, r) ->
        merge (filter p l) (filter p r)

  let find_min_exn = function
    | E -> raise Empty
    | N (_, x, _, _) -> x

  let find_min = function
    | E -> None
    | N (_, x, _, _) -> Some x

  let take = function
    | E -> None
    | N (_, x, l, r) -> Some (merge l r, x)

  let take_exn = function
    | E -> raise Empty
    | N (_, x, l, r) -> merge l r, x

  let rec iter f h = match h with
    | E -> ()
    | N(_,x,l,r) -> f x; iter f l; iter f r

  let rec fold f acc h = match h with
    | E -> acc
    | N (_, x, a, b) ->
        let acc = f acc x in
        let acc = fold f acc a in
        fold f acc b

  let rec size = function
    | E -> 0
    | N (_,_,l,r) -> 1 + size l + size r

  (** {2 Conversions} *)

  let to_list h =
    let rec aux acc h = match h with
      | E -> acc
      | N(_,x,l,r) ->
          x::aux (aux acc l) r
    in aux [] h

  let of_list l = List.fold_left add empty l

  let of_seq h seq =
    let h = ref h in
    seq (fun x -> h := insert x !h);
    !h

  let to_seq h k = iter k h

  let rec of_klist h l = match l() with
    | `Nil -> h
    | `Cons (x, l') ->
        let h' = add h x in
        of_klist h' l'

  let to_klist h =
    let rec next stack () = match stack with
      | [] -> `Nil
      | E :: stack' -> next stack' ()
      | N (_, x, a, b) :: stack' ->
          `Cons (x, next (a :: b :: stack'))
    in
    next [h]

  let rec of_gen h g = match g () with
    | None -> h
    | Some x ->
        of_gen (add h x) g

  let to_gen h =
    let stack = Stack.create () in
    Stack.push h stack;
    let rec next () =
      if Stack.is_empty stack
      then None
      else match Stack.pop stack with
        | E -> next()
        | N (_, x, a, b) ->
            Stack.push a stack;
            Stack.push b stack;
            Some x
    in next

  let rec to_tree h () = match h with
    | E -> `Nil
    | N (_, x, l, r) -> `Node(x, [to_tree l; to_tree r])
end
