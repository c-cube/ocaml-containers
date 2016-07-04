
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Leftist Heaps} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

module type PARTIAL_ORD = sig
  type t
  val leq : t -> t -> bool
  (** [leq x y] shall return [true] iff [x] is lower or equal to [y] *)
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

  let extract_list heap =
    let rec recurse acc h =
      if H.is_empty h
        then List.rev acc
        else
          let h', x = H.take_exn h in
          recurse (x::acc) h'
    in
    recurse [] heap
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
    let h = H.of_seq (Sequence.of_list l) in
    OUnit.assert_equal 1_000 (H.size h);
    let l' = extract_list h in
    is_sorted l'
  )
*)

(* test filter *)
(*$QR & ~count:30
  Q.(list_of_size Gen.(return 1_000) int) (fun l ->
    (* put elements into a heap *)
    let h = H.of_seq (Sequence.of_list l) in
    let h = H.filter (fun x->x mod 2=0) h in
    OUnit.assert_bool "all odd"
      (H.to_seq h |> Sequence.for_all (fun x -> x mod 2 = 0));
    let l' = extract_list h in
    is_sorted l'
  )
*)

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

  (** {2 Conversions}

      The interface of [of_gen], [of_seq], [of_klist]
      has changed @since 0.16 (the old signatures
      are now [add_seq], [add_gen], [add_klist]) *)

  val to_list : t -> elt list

  val add_list : t -> elt list -> t (** @since 0.16 *)

  val of_list : elt list -> t

  val add_seq : t -> elt sequence -> t (** @since 0.16 *)

  val of_seq : elt sequence -> t

  val to_seq : t -> elt sequence

  val add_klist : t -> elt klist -> t (** @since 0.16 *)

  val of_klist : elt klist -> t

  val to_klist : t -> elt klist

  val add_gen : t -> elt gen -> t (** @since 0.16 *)

  val of_gen : elt gen -> t

  val to_gen : t -> elt gen

  val to_tree : t -> elt ktree

  val print : ?sep:string -> elt printer -> t printer
  (** @since 0.16 *)
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

  let add_list h l = List.fold_left add h l

  let of_list l = add_list empty l

  let add_seq h seq =
    let h = ref h in
    seq (fun x -> h := insert x !h);
    !h

  let of_seq seq = add_seq empty seq

  let to_seq h k = iter k h

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
      (H.to_gen h |> CCList.of_gen |> List.sort Pervasives.compare) \
        = (H.to_list h |> List.sort Pervasives.compare))
  *)

  let rec to_tree h () = match h with
    | E -> `Nil
    | N (_, x, l, r) -> `Node(x, [to_tree l; to_tree r])

  let print ?(sep=",") pp_elt out h =
    let first=ref true in
    iter
      (fun x ->
        if !first then first := false else Format.fprintf out "%s@," sep;
        pp_elt out x)
      h
end
