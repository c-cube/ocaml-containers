
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Leftist Heaps} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

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

(*$inject
  module H = CCHeap.Make(struct
    type t = int
    let leq x y = x<=y
  end)

  let rec is_sorted l = match l with
    | [_]
    | [] -> true
    | x::((y::_) as l') -> x <= y && is_sorted l'

  let extract_list = H.to_list_sorted
*)

(*$R
  let h = H.of_list [5;3;4;1;42;0] in
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 0 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 1 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 3 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 4 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 5 x;
  let h, x = H.take_exn h in
  OUnit.assert_equal ~printer:string_of_int 42 x;
  OUnit.assert_raises H.Empty (fun () -> H.take_exn h);
*)

(*$QR & ~count:30
  Q.(list_of_size Gen.(return 1_000) int) (fun l ->
    (* put elements into a heap *)
    let h = H.of_iter (Iter.of_list l) in
    OUnit.assert_equal 1_000 (H.size h);
    let l' = extract_list h in
    is_sorted l'
  )
*)

(* test filter *)
(*$QR & ~count:30
  Q.(list_of_size Gen.(return 1_000) int) (fun l ->
    (* put elements into a heap *)
    let h = H.of_iter (Iter.of_list l) in
    let h = H.filter (fun x->x mod 2=0) h in
    OUnit.assert_bool "all odd"
      (H.to_iter h |> Iter.for_all (fun x -> x mod 2 = 0));
    let l' = extract_list h in
    is_sorted l'
  )
*)

(*$QR
  Q.(list_of_size Gen.(return 1_000) int) (fun l ->
    (* put elements into a heap *)
    let h = H.of_iter (Iter.of_list l) in
    let l' = H.to_iter_sorted h |> Iter.to_list in
    is_sorted l'
  )
*)

module type S = sig
  type elt
  type t

  val empty : t
  (** Empty heap. *)

  val is_empty : t -> bool
  (** Is the heap empty? *)

  exception Empty

  val merge : t -> t -> t
  (** Merge two heaps. *)

  val insert : elt -> t -> t
  (** Insert a value in the heap. *)

  val add : t -> elt -> t
  (** Synonym to {!insert}. *)

  val filter :  (elt -> bool) -> t -> t
  (** Filter values, only retaining the ones that satisfy the predicate.
      Linear time at least. *)

  val find_min : t -> elt option
  (** Find minimal element. *)

  val find_min_exn : t -> elt
  (** Like {!find_min} but can fail.
      @raise Empty if the heap is empty. *)

  val take : t -> (t * elt) option
  (** Extract and return the minimum element, and the new heap (without
      this element), or [None] if the heap is empty. *)

  val take_exn : t -> t * elt
  (** Like {!take}, but can fail.
      @raise Empty if the heap is empty. *)

  val delete_one : (elt -> elt -> bool) -> elt -> t -> t
  (** Delete one occurrence of a value if it exist in the heap.
      [delete_one eq x h], use [eq] to find one [x] in [h] and delete it.
      If [h] do not contain [x] then it return [h].
      @since 2.0 *)

  val delete_all : (elt -> elt -> bool) -> elt -> t -> t
  (** Delete all occurrences of a value in the heap.
      [delete_all eq x h], use [eq] to find all [x] in [h] and delete them.
      If [h] do not contain [x] then it return [h].
      The difference with {!filter} is that [delete_all] stops as soon as
      it enters a subtree whose root is bigger than the element.
      @since 2.0 *)

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on elements. *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Fold on all values. *)

  val size : t -> int
  (** Number of elements (linear complexity). *)

  (** {2 Conversions}

      The interface of [of_gen], [of_iter], [of_klist]
      has changed since 0.16 (the old signatures
      are now [add_seq], [add_gen], [add_klist]). *)

  val to_list : t -> elt list
  (** Return the elements of the heap, in no particular order. *)

  val to_list_sorted : t -> elt list
  (** Return the elements in increasing order.
      @since 1.1 *)

  val add_list : t -> elt list -> t
  (** Add the elements of the list to the heap. An element occurring several
      times will be added that many times to the heap.
      @since 0.16 *)

  val of_list : elt list -> t
  (** [of_list l] is [add_list empty l]. Complexity: [O(n log n)]. *)

  val add_iter : t -> elt iter -> t
  (** Like {!add_list}.
      @since 2.8 *)

  val add_std_seq : t -> elt Seq.t -> t
  (** Like {!add_list}.
      @since 2.8 *)

  val of_iter : elt iter -> t
  (** Build a heap from a given [iter]. Complexity: [O(n log n)].
      @since 2.8 *)

  val of_std_seq : elt Seq.t -> t
  (** Build a heap from a given [Seq.t]. Complexity: [O(n log n)].
      @since 2.8 *)

  val to_iter : t -> elt iter
  (** Return a [iter] of the elements of the heap.
      @since 2.8 *)

  val to_std_seq : t -> elt Seq.t
  (** Return a [Seq.t] of the elements of the heap.
      @since 2.8 *)

  val to_iter_sorted : t -> elt iter
  (** Iterate on the elements, in increasing order.
      @since 2.8 *)

  val to_std_seq_sorted : t -> elt Seq.t
  (** Iterate on the elements, in increasing order.
      @since 2.8 *)

  val add_klist : t -> elt klist -> t (** @since 0.16 *)

  val of_klist : elt klist -> t
  (** Build a heap from a given [klist]. Complexity: [O(n log n)]. *)

  val to_klist : t -> elt klist
  (** Return a [klist] of the elements of the heap. *)

  val add_gen : t -> elt gen -> t (** @since 0.16 *)

  val of_gen : elt gen -> t
  (** Build a heap from a given [gen]. Complexity: [O(n log n)]. *)

  val to_gen : t -> elt gen
  (** Return a [gen] of the elements of the heap. *)

  val to_tree : t -> elt ktree
  (** Return a [ktree] of the elements of the heap. *)

  val to_string : ?sep:string -> (elt -> string) -> t -> string
  (**  Print the heap in a string
       @since 2.7 *)

  val pp : ?sep:string -> elt printer -> t printer
  (** Printer.
      Renamed from {!print} since 2.0
      @since 0.16 *)
end

module Make(E : PARTIAL_ORD) : S with type elt = E.t = struct
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

  let delete_one eq x h =
    let rec aux = function
      | E -> false, E
      | N(_, y, l, r) as h ->
        if eq x y then true, merge l r
        else (
          if E.leq y x
          then (
            let found_left, l1 = aux l in
            let found, r1 = if found_left then true, r else aux r in
            if found
            then true, _make_node y l1 r1
            else false, h
          )
          else false, h
        )
    in
    snd (aux h)

  let rec delete_all eq x = function
    | E -> E
    | N (_, y, l, r) as h ->
      if eq x y then merge (delete_all eq x l) (delete_all eq x r)
      else (
        if E.leq y x
        then _make_node y (delete_all eq x l) (delete_all eq x r)
        else h
      )

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

  let to_list_sorted heap =
    let rec recurse acc h = match take h with
      | None -> List.rev acc
      | Some (h',x) -> recurse (x::acc) h'
    in
    recurse [] heap

  let add_list h l = List.fold_left add h l

  let of_list l = add_list empty l

  let add_iter h i =
    let h = ref h in
    i (fun x -> h := insert x !h);
    !h

  let add_std_seq h seq =
    let h = ref h in
    Seq.iter (fun x -> h := insert x !h) seq;
    !h

  let of_iter i = add_iter empty i
  let of_std_seq seq = add_std_seq empty seq

  let to_iter h k = iter k h

  let to_std_seq h =
    (* use an explicit stack [st] *)
    let rec aux st () =
      match st with
      | [] -> Seq.Nil
      | E :: st' -> aux st' ()
      | N(_,x,l,r) :: st' -> Seq.Cons (x, aux (l::r::st'))
    in aux [h]

  let to_iter_sorted heap =
    let rec recurse h k = match take h with
      | None -> ()
      | Some (h',x) -> k x; recurse h' k
    in
    fun k -> recurse heap k

  let rec to_std_seq_sorted h () = match take h with
    | None -> Seq.Nil
    | Some (h', x) -> Seq.Cons (x, to_std_seq_sorted h')

  let rec add_klist h l = match l() with
    | `Nil -> h
    | `Cons (x, l') ->
      let h' = add h x in
      add_klist h' l'

  let of_klist l = add_klist empty l

  let to_klist h =
    let rec next stack () = match stack with
      | [] -> `Nil
      | E :: stack' -> next stack' ()
      | N (_, x, a, b) :: stack' ->
        `Cons (x, next (a :: b :: stack'))
    in
    next [h]

  let rec add_gen h g = match g () with
    | None -> h
    | Some x ->
      add_gen (add h x) g

  let of_gen g = add_gen empty g

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

  (*$Q
    Q.(list int) (fun l -> \
      extract_list (H.of_list l) = \
        extract_list (H.of_gen (CCList.to_gen l)))
    Q.(list int) (fun l -> \
      let h = H.of_list l in \
      (H.to_gen h |> CCList.of_gen |> List.sort Stdlib.compare) \
        = (H.to_list h |> List.sort Stdlib.compare))
  *)

  let rec to_tree h () = match h with
    | E -> `Nil
    | N (_, x, l, r) -> `Node(x, [to_tree l; to_tree r])

  let to_string ?(sep=",") elt_to_string h =
    to_list_sorted h
    |> List.map elt_to_string
    |> String.concat sep

  (*$Q
    Q.(list int) (fun l -> \
      let h = H.of_list l in \
      (H.to_string string_of_int h) \
        = (List.sort Stdlib.compare l |> List.map string_of_int |> String.concat ","))
    Q.(list int) (fun l -> \
      let h = H.of_list l in \
      (H.to_string ~sep:" " string_of_int h) \
        = (List.sort Stdlib.compare l |> List.map string_of_int |> String.concat " "))
  *)

  let pp ?(sep=",") pp_elt out h =
    let first=ref true in
    iter
      (fun x ->
         if !first then first := false else Format.fprintf out "%s@," sep;
         pp_elt out x)
      h
end

module Make_from_compare(E : TOTAL_ORD) =
  Make(struct
    type t = E.t
    let leq a b = E.compare a b <= 0
  end)

(*$QR
  Q.(list_of_size Gen.(return 1_000) int) (fun l ->
    let module H' = Make_from_compare(CCInt) in
    let h = H'.of_list l in
    let l' = H'.to_list_sorted h in
    is_sorted l'
  )
*)
