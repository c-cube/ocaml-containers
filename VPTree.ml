(*
copyright (c) 2013, simon cruanes
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

(** {1 Vantage-Point Tree} *)

module type METRIC_SPACE = sig
  type t
    (** Elements of the metric space *)

  type num
    (** Numeric type used for distances *)

  val distance : t -> t -> num
    (** Distance between two points. It must satisfy the following invariants:

        - [distance x y = distance y x]
        - [distance x y <= distance x z + distance z y]
        - [distance x y >= 0]
        - [distance x y = 0] if and only if [x] and [y] are the same point.
    *)

  val add : num -> num -> num
    (** Addition of two distances (associative, commutative) *)

  val compare : num -> num -> int
    (** Total ordering on the numeric type used for distances *)
end
(** {2 Utils} *)

type 'a sequence = ('a -> unit) -> unit

module LazyList = struct
  type 'a t =
    | Nil
    | Cons of 'a * 'a t Lazy.t

  let rec take n l = match l with
    | _ when n = 0 -> Nil
    | Nil -> Nil
    | Cons (x, (lazy l')) -> Cons (x, lazy (take (n-1) l'))

  let rec iter l k = match l with
    | Nil -> ()
    | Cons (x, (lazy l')) -> k x; iter l' k

  let rec to_list l = match l with
    | Nil -> []
    | Cons (x, (lazy l')) -> x :: (to_list l')
end

(** {3 Mutable heap} *)
module Heap = struct
  (** Implementation from http://en.wikipedia.org/wiki/Skew_heap *)

  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A pairing tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  let rec union ~cmp t1 t2 = match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (x1, l1, r1), Node (x2, l2, r2) ->
    if cmp x1 x2 <= 0
      then Node (x1, union ~cmp t2 r1, l1)
      else Node (x2, union ~cmp t1 r2, l2)

  let insert h x =
    h.tree <- union ~cmp:h.cmp (Node (x, Empty, Empty)) h.tree

  let pop h = match h.tree with
    | Empty -> raise Not_found
    | Node (x, l, r) ->
      h.tree <- union ~cmp:h.cmp l r;
      x
end

(** {2 VPTree functorial interface} *)

module type S = sig
  module Space : METRIC_SPACE

  type key = Space.t

  type +'a t
    (** VPTree that maps to data of type 'a *)

  val member : _ t -> key -> bool
    (** Is the given key, member of the tree? *)

  val find_closest : 'a t -> key -> (key * 'a * int) LazyList.t
    (** [find_closest tree key] finds the points of [tree]
        that are the closest to [key], in increasing order. Enumerates all
        the points eventually, if the whole list is explored.
        Each tuple also contains the distance to the [key]. *)

  val of_array : (key * 'a) array -> 'a t
    (** Build a VPTree from an array of bindings. The array is modified in
        place (values are swapped).
        If a key occurs several times in the sequence, one of the value
        it maps to is chosen arbitrarily. *)

  val of_list : (key * 'a) list -> 'a t
    (** Build a VPTree from a list *)

  val of_seq : (key * 'a) sequence -> 'a t

  val of_lazy_list : (key * 'a) LazyList.t -> 'a t

  val size : _ t -> int
    (** Find the number of key/value pairs in the tree (linear time) *)
end

(* we follow François Berenger's implementation, but simpler, because we
  want to retrieve several points and yet keep the implementation reasonably
  simple. *)
module Make(Space : METRIC_SPACE) = struct
  module Space = Space

  type key = Space.t
  type num = Space.num

  type 'a t =
    | Empty
    | Node of 'a node * num * 'a t * 'a t

  and 'a node = {
    key : key;
    value : 'a;
    tree : 'a t;
  }

  (* insert a tree in the heap of trees to explore *)
  let _insert_heap h key tree = match tree with
    | Empty -> ()
    | Node (node, _, _, _) ->
      let d = Space.distance key node.key in
      Heap.insert h (tree, d)

  (* explore the given heap *)
  let rec _explore_heap h key =
    if Heap.is_empty h
    then LazyList.Nil
    else match Heap.pop h with
    | Empty -> _explore_heap h key
    |

  (* find closest points to [key] *)
  let find_closest tree key =
    match tree with
    | Empty -> LazyList.Nil
    | Node _ ->
      let cmp (t1,d1) (t2,d2) = Space.compare d1 d2 in
      let h = Heap.empty ~cmp in
      _insert_heap h key tree;
      _explore_heap h key

  let member tree key =
    match tree with
    | None -> false
    | Some tree ->
      match find_closest tree key with
      | LazyList.Nil -> false
      | LazyList.Cons ((_, _, 0), _) -> true
      | _ -> false

  let size t =
    let rec size t = match t with
    | Leaf _ -> 1
    | Pair _ -> 2
    | Node (_, _, _, t1, t2) -> size t1 + size t2 + 1
    in match t with
    | None -> 0
    | Some t -> size t

  (* utilities to build the tree *)

  let of_array a = assert false  (* TODO *)

  let of_list l = of_array (Array.of_list l)

  let of_seq seq =
    let l = ref [] in
    seq (fun x -> l := x :: !l);
    of_list !l

  let of_lazy_list l = of_seq (LazyList.iter l)  (* TODO optimize *)
end
