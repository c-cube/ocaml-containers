(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Leftist Heaps} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [ `Nil | `Node of 'a * 'a ktree list ]

let[@inline] _iter_map f xs k = xs (fun x -> k (f x))

let rec _gen_iter k g =
  match g () with
  | None -> ()
  | Some x ->
    k x;
    _gen_iter k g

module type PARTIAL_ORD = sig
  type t

  val leq : t -> t -> bool
  (** [leq x y] shall return [true] iff [x] is lower or equal to [y]. *)
end

module type TOTAL_ORD = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] shall return
      a negative value if [a] is smaller than [b],
      [0] if [a] and [b] are equal or
      a positive value if [a] is greater than [b] *)
end

module type S = sig
  type elt
  type t

  exception Empty

  (** {2 Basing heap operations} *)

  val empty : t
  (** Empty heap. *)

  val is_empty : t -> bool
  (** Is the heap empty? *)

  val merge : t -> t -> t
  (** [merge h1 h2] merges the two heaps [h1] and [h2].
      If one heap is empty, the result is physically equal to the other heap.
      Complexity: [O(log (m+n))] where [m] and [n] are the number of elements in each heap.
  *)

  val insert : elt -> t -> t
  (** [insert x h] inserts an element [x] into the heap [h].
      Complexity: [O(log n)] where [n] is the number of elements in [h].
  *)

  val add : t -> elt -> t
  (** [add h x] is [insert x h]. *)

  val find_min : t -> elt option
  (** [find_min h] returns the minimal element of [h],
      or [None] if [h] is empty.
      Complexity: [O(1)].
  *)

  val find_min_exn : t -> elt
  (** [find_min_exn h] is akin to {!find_min},
      but it raises {!Empty} when the heap is empty.
      @raise Empty if the heap is empty. *)

  val take : t -> (t * elt) option
  (** [take h] returns the minimum element of [h]
      and the new heap without this element,
      or [None] if [h] is empty.
      Complexity: [O(log n)].
  *)

  val take_exn : t -> t * elt
  (** [take_exn h] is akin to {!take},
      but it raises {!Empty} when the heap is empty.
      @raise Empty if the heap is empty. *)

  val size : t -> int
  (** [size h] is the number of elements in the heap [h].
      Complexity: [O(n)].
  *)

  (** {2 Deleting elements} *)

  val delete_one : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_one eq x h] deletes an occurrence of the value [x] from the heap
      [h],
      if there is some.
      If [h] does not contain [x], then [h] itself is returned.
      Elements are identified by the equality function [eq].
      Complexity: [O(n)].
      @since 2.0 *)

  val delete_all : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_all eq x h] deletes all occurrences of the value [x] from the heap [h].
      If [h] does not contain [x], then [h] itself is returned.
      Elements are identified by the equality function [eq].
      This function is more efficient than {!filter}
      because it avoids considering elements greater than [x].
      Complexity: [O(n)].
      @since 2.0 *)

  val filter : (elt -> bool) -> t -> t
  (** [filter p h] filters the elements of [h],
      only retaining those that satisfy the predicate [p].
      If no element in [h] satisfies [p], then [h] itself is returned.
      Complexity: [O(n)].
  *)

  (** {2 Iterating on elements} *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f h] invokes [f] on every element of the heap [h]. *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** [fold f acc h] folds on all elements of [h]. *)

  (** {2 Adding many elements at once} *)

  val add_list : t -> elt list -> t
  (** [add_list h l] adds the elements of the list [l] into the heap [h].
      An element occurring several times will be added that many times to the heap.
      Elements need not be given in any particular order.
      This function is more efficient than repeated insertions.
      Complexity: [O(log m + n)]
      where [m] and [n] are the number of elements in [h] and [l], respectively.
      @since 0.16 *)

  val add_iter : t -> elt iter -> t
  (** [add_iter h iter] is akin to {!add_list},
      but taking an [iter] of elements as input.
      @since 2.8 *)

  val add_seq : t -> elt Seq.t -> t
  (** [add_seq h seq] is akin to {!add_list},
      but taking a [Seq.t] of elements as input.
      Renamed from [add_std_seq] since 3.0.
      @since 3.0 *)

  val add_gen : t -> elt gen -> t
  (** [add_gen h gen] is akin to {!add_list},
      but taking a [gen] of elements as input.
      @since 0.16 *)

  val add_iter_almost_sorted : t -> elt iter -> t
  (** [add_iter_almost_sorted h iter] is equivalent to
      [merge h (of_iter_almost_sorted iter)].
      See {!of_iter_almost_sorted}.
      Complexity: [O(log m + n)].
      @since 3.14
  *)

  (** {2 Conversions} *)

  val of_list : elt list -> t
  (** [of_list l] builds a heap from the list of elements [l].
      Elements need not be given in any particular order.
      This function is more efficient than repeated insertions.
      It is equivalent to [add_list empty l].
      Complexity: [O(n)].
  *)

  val of_iter : elt iter -> t
  (** [of_iter iter] is akin to {!of_list},
      but taking an [iter] of elements as input.
      @since 2.8 *)

  val of_seq : elt Seq.t -> t
  (** [of_seq seq] is akin to {!of_list},
      but taking a [Seq.t] of elements as input.
      Renamed from [of_std_seq] since 3.0.
      @since 3.0 *)

  val of_gen : elt gen -> t
  (** [of_gen gen] is akin to {!of_list},
      but taking a [gen] of elements as input. *)

  val of_iter_almost_sorted : elt iter -> t
  (** [of_iter iter] builds a heap from the {!type:iter} sequence of elements.
      Elements need not be given in any particular order.
      However, the heap takes advantage of partial sorting found in the input:
      the closer the input sequence is to being sorted,
      the more efficient it is to convert the heap to a sorted sequence.
      This enables heap-sorting that is faster than [O(n log n)]
      when the input is almost sorted.
      In the best case, when only a constant number of elements are misplaced,
      then successive {!take} run in [O(1)],
      and {!to_list_sorted} runs in [O(n)].
      Complexity: [O(n)].
  *)

  val to_list : t -> elt list
  (** [to_list h] returns a list of the elements of the heap [h],
      in no particular order.
      Complexity: [O(n)].
  *)

  val to_iter : t -> elt iter
  (** [to_iter h] is akin to {!to_list}, but returning an [iter] of elements.
      @since 2.8 *)

  val to_seq : t -> elt Seq.t
  (** [to_seq h] is akin to {!to_list}, but returning a [Seq.t] of elements.
      Renamed from [to_std_seq] since 3.0.
      @since 3.0 *)

  val to_gen : t -> elt gen
  (** [to_gen h] is akin to {!to_list}, but returning a [gen] of elements. *)

  val to_list_sorted : t -> elt list
  (** [to_list_sorted h] returns the list of elements of the heap [h]
      in increasing order.
      Complexity: [O(n log n)].
      @since 1.1 *)

  val to_iter_sorted : t -> elt iter
  (** [to_iter_sorted h] is akin to {!to_list_sorted},
      but returning an [iter] of elements.
      @since 2.8 *)

  val to_seq_sorted : t -> elt Seq.t
  (** [to_seq_sorted h] is akin to {!to_list_sorted},
      but returning a [Seq.t] of elements.
      Renamed from [to_std_seq_sorted] since 3.0.
      @since 3.0 *)

  val to_tree : t -> elt ktree
  (** [to_tree h] returns a [ktree] of the elements of the heap [h].
      The layout is not specified.
      Complexity: [O(n)].
  *)

  (** {2 Pretty-printing} *)

  val to_string : ?sep:string -> (elt -> string) -> t -> string
  (**  Print the heap in a string
       @since 2.7 *)

  val pp :
    ?pp_start:unit printer ->
    ?pp_stop:unit printer ->
    ?pp_sep:unit printer ->
    elt printer ->
    t printer
  (** Printer.
      Renamed from {!print} since 2.0
      @since 0.16 *)
end

module Make (E : PARTIAL_ORD) : S with type elt = E.t = struct
  type elt = E.t

  type t =
    | E
    | N of int * elt * t * t

  let empty = E

  let is_empty = function
    | E -> true
    | N _ -> false

  exception Empty

  let singleton x = N (1, x, E, E)

  (* Rank of the tree *)
  let _rank = function
    | E -> 0
    | N (r, _, _, _) -> r

  (* Make a balanced node labelled with [x], and subtrees [a] and [b].
     We ensure that the right child's rank is ≤ to the rank of the
     left child (leftist property). The rank of the resulting node
     is the length of the rightmost path. *)
  let _make_node x a b =
    if _rank a >= _rank b then
      N (_rank b + 1, x, a, b)
    else
      N (_rank a + 1, x, b, a)

  let rec merge t1 t2 =
    match t1, t2 with
    | t, E -> t
    | E, t -> t
    | N (_, x, a1, b1), N (_, y, a2, b2) ->
      if E.leq x y then
        _make_node x a1 (merge b1 t2)
      else
        _make_node y a2 (merge t1 b2)

  let insert x h = merge (singleton x) h
  let add h x = insert x h

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

  let rec iter f h =
    match h with
    | E -> ()
    | N (_, x, l, r) ->
      f x;
      iter f l;
      iter f r

  let rec fold f acc h =
    match h with
    | E -> acc
    | N (_, x, a, b) ->
      let acc = f acc x in
      let acc = fold f acc a in
      fold f acc b

  let rec size = function
    | E -> 0
    | N (_, _, l, r) -> 1 + size l + size r

  (** {2 Conversions from sequences} *)

  (* Merge an [iter] of k heaps into one.
     Instead of folding [merge] in one pass (which would run in time O(k log N)
     where k is the number of heaps and N is the total number of elements), it
     is more efficient to merge heaps pairwise until only one remains; see e.g.
         Robert Tarjan, "Data Structures and Network Algorithms",
         Chapter 3.3 "Leftist heaps", 1983.
     or:
         Chris Okasaki, "Purely Functional Data Structures",
         Chapter 3.2 "Leftist heaps", Exercise 3.3, 1998
     This is independent of the representation of heaps, and, as long as merging
     is in time O(log n), this runs in time O(k + k*log(N/k)). Notice that this
     is a O(k + N) (if k is small wrt. N, this last upper bound is very loose).
     The code below uses additional space of only O(log(k)) at any moment;
     it avoids storing an intermediate list of length O(k).
     When at most one of the input heaps is non-empty, the result is physically
     equal to it. *)
  let _merge_heap_iter (hs : t iter) : t =
    let rec cons_and_merge h0 hs weights =
      match hs with
      | h1 :: hs' when weights land 1 = 0 ->
        cons_and_merge (merge h0 h1) hs' (weights lsr 1)
      | _ -> h0 :: hs
    in
    (* the i-th heap in this list is a merger of 2^{w_i} input heaps, each
       having gone through w_i merge operations, where the "weights" 2^{w_i} are
       strictly increasing wrt. i: *)
    let mergers = ref [] in
    (* The w_i are the 1-bits in the binary writing of [count], the number of
       input heaps merged so far; adding a heap to the mergers works like binary
       incrementation: *)
    let count = ref 0 in
    hs (fun h ->
        incr count;
        mergers := cons_and_merge h !mergers !count);
    List.fold_left merge E !mergers

  (* To build a heap with n given values, instead of repeated insertions,
     it is more efficient to do pairwise merging, running in time O(n). *)
  let of_iter xs = xs |> _iter_map singleton |> _merge_heap_iter
  let of_list xs = of_iter (fun k -> List.iter k xs)
  let of_seq xs = of_iter (fun k -> Seq.iter k xs)
  let of_gen xs = of_iter (fun k -> _gen_iter k xs)

  (* When input values are sorted in reverse order, then repeated insertions in
     a leftist heap run in time O(n) and build a list-like heap where elements
     are totally sorted, which makes a subsequent conversion to sorted sequence
     run in O(n). *)
  let _of_list_rev_sorted (xs : elt list) : t =
    List.fold_left (fun h x -> N (1, x, h, E)) E xs

  (* We use this to convert an arbitrary input sequence to a heap in time O(n),
     while achieving an efficient heap structure in the common situation when
     the input is almost sorted. This improves heap-sorting, for instance. *)
  let of_iter_almost_sorted xs =
    let sorted_chunk = ref [] in
    let iter_sorted_heaps k =
      xs (fun x ->
          match !sorted_chunk with
          | y :: _ as ys when not (E.leq y x) ->
            k (_of_list_rev_sorted ys);
            sorted_chunk := [ x ]
          | ys -> sorted_chunk := x :: ys);
      k (_of_list_rev_sorted !sorted_chunk)
    in
    _merge_heap_iter iter_sorted_heaps

  (** {2 Adding many elements at once} *)

  let add_list h xs = merge h (of_list xs)
  let add_iter h xs = merge h (of_iter xs)
  let add_seq h xs = merge h (of_seq xs)
  let add_gen h xs = merge h (of_gen xs)
  let add_iter_almost_sorted h xs = merge h (of_iter_almost_sorted xs)

  (** {2 Conversions to sequences} *)

  let to_list h =
    let rec aux acc h =
      match h with
      | E -> acc
      | N (_, x, l, r) -> x :: aux (aux acc l) r
    in
    aux [] h

  let to_iter h k = iter k h

  let to_seq h =
    (* use an explicit stack [st] *)
    let rec aux st () =
      match st with
      | [] -> Seq.Nil
      | E :: st' -> aux st' ()
      | N (_, x, l, r) :: st' -> Seq.Cons (x, aux (l :: r :: st'))
    in
    aux [ h ]

  let to_gen h =
    let stack = Stack.create () in
    Stack.push h stack;
    let rec next () =
      if Stack.is_empty stack then
        None
      else (
        match Stack.pop stack with
        | E -> next ()
        | N (_, x, a, b) ->
          Stack.push a stack;
          Stack.push b stack;
          Some x
      )
    in
    next

  let to_list_sorted heap =
    let rec recurse acc h =
      match take h with
      | None -> List.rev acc
      | Some (h', x) -> recurse (x :: acc) h'
    in
    recurse [] heap

  let to_iter_sorted heap =
    let rec recurse h k =
      match take h with
      | None -> ()
      | Some (h', x) ->
        k x;
        recurse h' k
    in
    fun k -> recurse heap k

  let rec to_seq_sorted h () =
    match take h with
    | None -> Seq.Nil
    | Some (h', x) -> Seq.Cons (x, to_seq_sorted h')

  let rec to_tree h () =
    match h with
    | E -> `Nil
    | N (_, x, l, r) -> `Node (x, [ to_tree l; to_tree r ])

  (** {2 Filtering} *)

  let rec delete_one eq x0 = function
    | N (_, x, l, r) as h when E.leq x x0 ->
      if eq x0 x then
        merge l r
      else (
        let l' = delete_one eq x0 l in
        if CCEqual.physical l' l then (
          let r' = delete_one eq x0 r in
          if CCEqual.physical r' r then
            h
          else
            _make_node x l r'
        ) else
          _make_node x l' r
      )
    | h -> h

  let delete_all eq x0 h =
    (* Iterates [k] on sub-heaps of [h] whose merger is equal to [h] minus
       the deleted elements [x0]; we do this, instead of merging the subheaps
       directly, in order to ensure complexity O(n).
       When no element is deleted, the iterator does nothing and the function
       returns true; this makes sure that the result shares sub-heaps with the
       input as much as possible, and ensures physical equality when no element
       is deleted.
       In [delete_all], by contrast with [filter], we can avoid considering
       elements greater than [x0]. As a consequence, the complexity is more
       precisely O(k + k log(n/k)), where k is the number of elements not
       greater than [x0]. This is a O(n), but it is also a O(k log n), which is
       much smaller than O(n) if k is asymptotically smaller than n.
    *)
    let rec iter_subheaps eq x0 h k =
      match h with
      | N (_, x, l, r) when E.leq x x0 ->
        let keep_x = not (eq x0 x) in
        let keep_l = iter_subheaps eq x0 l k in
        let keep_r = iter_subheaps eq x0 r k in
        if keep_x && keep_l && keep_r then
          true
        else (
          if keep_x then k (singleton x);
          if keep_l then k l;
          if keep_r then k r;
          false
        )
      | _ -> true
    in
    _merge_heap_iter (fun k -> if iter_subheaps eq x0 h k then k h)

  let filter p h =
    (* similar to [delete_all] *)
    let rec iter_subheaps p k h =
      match h with
      | E -> true
      | N (_, x, l, r) ->
        let keep_x = p x in
        let keep_l = iter_subheaps p k l in
        let keep_r = iter_subheaps p k r in
        if keep_x && keep_l && keep_r then
          true
        else (
          if keep_x then k (singleton x);
          if keep_l then k l;
          if keep_r then k r;
          false
        )
    in
    _merge_heap_iter (fun k -> if iter_subheaps p k h then k h)

  (** {2 Pretty-printing} *)

  let to_string ?(sep = ",") elt_to_string h =
    to_list_sorted h |> List.map elt_to_string |> String.concat sep

  let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
      ?(pp_sep = fun out () -> Format.fprintf out ",") pp_elt out h =
    let first = ref true in
    pp_start out ();
    iter
      (fun x ->
        if !first then
          first := false
        else
          pp_sep out ();
        pp_elt out x)
      h;
    pp_stop out ()
end

module Make_from_compare (E : TOTAL_ORD) = Make (struct
  type t = E.t

  let leq a b = E.compare a b <= 0
end)
