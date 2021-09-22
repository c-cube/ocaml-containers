
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Complements to list} *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

include module type of ListLabels
(** {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/ListLabels.html} Documentation for the standard ListLabels module}*)

type 'a t = 'a list

val empty : 'a t
(** [empty] is [[]]. *)

val is_empty : _ t -> bool
(** [is_empty l] returns [true] iff [l = []].
    @since 0.11 *)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** [map ~f [a0; a1; …; an]] applies function [f] in turn to [[a0; a1; …; an]].
    Safe version of {!List.map}. *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** [l >|= f] is the infix version of [map] with reversed arguments.
    @since 0.5 *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] is [x::l].
    @since 0.12 *)

val append : 'a t -> 'a t -> 'a t
(** [append l1 l2] returns the list that is the concatenation of [l1] and [l2].
    Safe version of {!List.append}. *)

val cons' : 'a t -> 'a -> 'a t
(** [cons' l x] is the same as [x :: l]. This is convenient for fold
    functions such as {!List.fold_left} or {!Array.fold_left}.
    @since 3.3 *)

val cons_maybe : 'a option -> 'a t -> 'a t
(** [cons_maybe (Some x) l] is [x :: l].
    [cons_maybe None l] is [l].
    @since 0.13 *)

val (@) : 'a t -> 'a t -> 'a t
(** [l1 @ l2] is like [append l1 l2].
    Concatenate the two lists [l1] and [l2]. *)

val filter : f:('a -> bool) -> 'a t -> 'a t
(** [filter ~f l] returns all the elements of the list [l]
    that satisfy the predicate [f].  The order of the elements
    in the input list [l] is preserved.
    Safe version of {!List.filter}. *)

val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
(** [fold_right ~f [a1; …; an] ~init] is
    [f a1 (f a2 ( … (f an init) … ))].
    Safe version of {!List.fold_right}. *)

val fold_while : f:('a -> 'b -> 'a * [`Stop | `Continue]) -> init:'a -> 'b t -> 'a
(** [fold_while ~f ~init l] folds until a stop condition via [('a, `Stop)] is
    indicated by the accumulator.
    @since 0.8 *)

val fold_map : f:('acc -> 'a -> 'acc * 'b) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_map ~f ~init l] is a [fold_left]-like function, but it also maps the
    list to another list.
    @since 0.14 *)

val fold_map_i : f:('acc -> int -> 'a -> 'acc * 'b) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_map_i ~f ~init l] is a [foldi]-like function, but it also maps the
    list to another list.
    @since 2.8 *)

val fold_on_map : f:('a -> 'b) -> reduce:('acc -> 'b -> 'acc) -> init:'acc -> 'a list -> 'acc
(** [fold_on_map ~f ~reduce ~init l] combines [map ~f] and [fold_left ~reduce ~init]
    in one operation.
    @since 2.8 *)

val scan_left : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a list -> 'acc list
(** [scan_left ~f ~init l] returns the list [[init; f init x0; f (f init x0) x1; …]]
    where [x0], [x1], etc. are the elements of [l].
    @since 1.2, but only
    @since 2.2 with labels *)

val reduce : f:('a -> 'a -> 'a) -> 'a list -> 'a option
(** [reduce f (hd::tl)] returns [Some (fold_left f hd tl)].  If [l] is empty,
    then [None] is returned.
    @since 3.2 *)

val reduce_exn : f:('a -> 'a -> 'a) -> 'a list -> 'a
(** [reduce_exn] is the unsafe version of {!reduce}.
    @raise Invalid_argument if the given list is empty.
    @since 3.2 *)

val fold_map2 : f:('acc -> 'a -> 'b -> 'acc * 'c) -> init:'acc -> 'a list -> 'b list -> 'acc * 'c list
(** [fold_map2 ~f ~init l1 l2] is to [fold_map] what [List.map2] is to [List.map].
    @raise Invalid_argument if the lists do not have the same length.
    @since 0.16 *)

val fold_filter_map : f:('acc -> 'a -> 'acc * 'b option) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_filter_map ~f ~init l] is a [fold_left]-like function, but also
    generates a list of output in a way similar to {!filter_map}.
    @since 0.17 *)

val fold_filter_map_i : f:('acc -> int -> 'a -> 'acc * 'b option) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_filter_map_i ~f ~init l] is a [foldi]-like function, but also
    generates a list of output in a way similar to {!filter_map}.
    @since 2.8 *)

val fold_flat_map : f:('acc -> 'a -> 'acc * 'b list) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_flat_map ~f ~init l] is a [fold_left]-like function, but it also maps the
    list to a list of lists that is then [flatten]'d.
    @since 0.14 *)

val fold_flat_map_i : f:('acc -> int -> 'a -> 'acc * 'b list) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_flat_map_i ~f ~init l] is a [fold_left]-like function, but it also maps the
    list to a list of lists that is then [flatten]'d.
    @since 2.8 *)

val count : f:('a -> bool) -> 'a list -> int
(** [count ~f l] counts how many elements of [l] satisfy predicate [f].
    @since 1.5, but only
    @since 2.2 with labels *)

val count_true_false : f:('a -> bool) -> 'a list -> int * int
(** [count_true_false ~f l] returns a pair [(int1,int2)] where [int1] is the number of elements in [l]
    that satisfy the predicate [f], and [int2] the number of elements that do not satisfy [f].
    @since 2.4 *)

val init : int -> f:(int -> 'a) -> 'a t
(** [init len ~f] is [f 0; f 1; …; f (len-1)].
    @raise Invalid_argument if len < 0.
    @since 0.6 *)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** [combine [a1; …; an] [b1; …; bn]] is [[(a1,b1); …; (an,bn)]].
    Transform two lists into a list of pairs.
    Like {!List.combine} but tail-recursive.
    @raise Invalid_argument if the lists have distinct lengths.
    @since 1.2, but only
    @since 2.2 with labels *)

val combine_gen : 'a list -> 'b list -> ('a * 'b) gen
(** [combine_gen l1 l2] transforms two lists into a [gen] of pairs.
    Lazy version of {!combine}.
    Unlike {!combine}, it does not fail if the lists have different
    lengths;
    instead, the output has as many pairs as the smallest input list.
    @since 1.2, but only
    @since 2.2 with labels *)

val combine_shortest : 'a list -> 'b list -> ('a * 'b) list
(** [combine_shortest [a1; …; am] [b1; …; bn]] is [[(a1,b1); …; (am,bm)]] if m <= n.
    Like {!combine} but stops at the shortest list rather than raising.
    @since 3.1 *)

val split : ('a * 'b) t -> 'a t * 'b t
(** [split [(a1,b1); …; (an,bn)]] is [([a1; …; an], [b1; …; bn])].
    Transform a list of pairs into a pair of lists.
    A tail-recursive version of {!List.split}.
    @since 1.2, but only
    @since 2.2 with labels *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare cmp l1 l2] compares the two lists [l1] and [l2]
    using the given comparison function [cmp]. *)
    
val compare_lengths : 'a t -> 'b t -> int
(** [compare_lengths l1 l2] compare the lengths of the two lists [l1] and [l2].
    Equivalent to [compare (length l1) (length l2)] but more efficient.
    @since 1.5, but only
    @since 2.2 with labels *)

val compare_length_with : 'a t -> int -> int
(** [compare_length_with l x] compares the length of the list [l] to an integer [x].
    Equivalent to [compare (length l) x] but more efficient.
    @since 1.5, but only
    @since 2.2 with labels *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal p l1 l2] returns [true] if [l1] and [l2] are equal. *)

val flat_map : f:('a -> 'b t) -> 'a t -> 'b t
(** [flat_map ~f l] maps and flattens at the same time (safe). Evaluation order is not guaranteed. *)

val flat_map_i : f:(int -> 'a -> 'b t) -> 'a t -> 'b t
(** [flat_map_i ~f l] maps with index and flattens at the same time (safe).
    Evaluation order is not guaranteed.
    @since 2.8 *)

val flatten : 'a t t -> 'a t
(** [flatten [l1]; [l2]; …] concatenates a list of lists. 
    Safe version of {!List.flatten}. *)
    
val product : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [product ~f l1 l2] computes the cartesian product of the two lists, with the given combinator [f]. *)

val fold_product : f:('c -> 'a -> 'b -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
(** [fold_product ~f ~init l1 l2] applies the function [f] with the accumulator [init] on all 
    the pair of elements of [l1] and [l2]. Fold on the cartesian product. *)
    
val cartesian_product : 'a t t -> 'a t t
(** [cartesian_product [[l1];[l2]; …; [ln]]] produces the cartesian product of this list of lists,
    by returning all the ways of picking one element per sublist.
    {b NOTE} the order of the returned list is unspecified.
    For example:
    {[
      # cartesian_product [[1;2];[3];[4;5;6]] |> sort =
      [[1;3;4];[1;3;5];[1;3;6];[2;3;4];[2;3;5];[2;3;6]];;
      # cartesian_product [[1;2];[];[4;5;6]] = [];;
      # cartesian_product [[1;2];[3];[4];[5];[6]] |> sort =
      [[1;3;4;5;6];[2;3;4;5;6]];;
    ]}
    invariant: [cartesian_product l = map_product id l].
    @since 1.2, but only
    @since 2.2 with labels *)

val map_product_l : f:('a -> 'b list) -> 'a list -> 'b list list
(** [map_product_l ~f l] maps each element of [l] to a list of
    objects of type ['b] using [f].
    We obtain [[l1; l2; …; ln]] where [length l=n] and [li : 'b list].
    Then, it returns all the ways of picking exactly one element per [li].
    @since 1.2, but only
    @since 2.2 with labels *)

val diagonal : 'a t -> ('a * 'a) t
(** [diagonal l] returns all pairs of distinct positions of the list [l],
    that is the list of [List.nth i l, List.nth j l] if [i < j]. *)

val partition_map_either : f:('a -> ('b, 'c) CCEither.t) ->
  'a list -> 'b list * 'c list
(** [partition_map_either ~f l] maps [f] on [l] and gather results in lists:
    - if [f x = Left y], adds [y] to the first list.
    - if [f x = Right z], adds [z] to the second list.
    @since 3.3 *)

val partition_filter_map : f:('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
  'a list -> 'b list * 'c list
(** [partition_filter_map ~f l] maps [f] on [l] and gather results in lists:
    - if [f x = `Left y], adds [y] to the first list.
    - if [f x = `Right z], adds [z] to the second list.
    - if [f x = `Drop], ignores [x].
    @since 0.11 *)

val partition_map : f:('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
  'a list -> 'b list * 'c list
[@@ocaml.deprecated "use CCList.partition_filter_map instead"]
(** @deprecated use {!partition_filter_map} instead *)

val group_by : ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) ->
  'a t -> 'a list t
(** [group_by ?hash ?eq l] groups equal elements, regardless of their order of appearance.
    precondition: for any [x] and [y], if [eq x y] then [hash x = hash y] must hold.
    @since 2.3 *)

val join : join_row:(('a -> 'b -> 'c option) [@keep_label]) -> 'a t -> 'b t -> 'c t
(** [join ~join_row a b] combines every element of [a] with every
    element of [b] using [join_row]. If [join_row] returns [None], then
    the two elements do not combine. Assume that [b] allows for multiple
    iterations.
    @since 2.3 *)

val join_by : ?eq:('key -> 'key -> bool) -> ?hash:('key -> int) ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:(('key -> 'a -> 'b -> 'c option) [@keep_label]) ->
  'a t ->
  'b t ->
  'c t
(** [join_by ?eq ?hash key1 key2 ~merge la lb] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded.
    precondition: for any [x] and [y], if [eq x y] then [hash x = hash y] must hold.
    @since 2.3 *)

val join_all_by : ?eq:('key -> 'key -> bool) -> ?hash:('key -> int) ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:(('key -> 'a list -> 'b list -> 'c option) [@keep_label]) ->
  'a t ->
  'b t ->
  'c t
(** [join_all_by ?eq ?hash key1 key2 ~merge la lb] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and, for each key [k]
    occurring in at least one of them:
    - compute the list [l1] of elements of [a] that map to [k]
    - compute the list [l2] of elements of [b] that map to [k]
    - call [merge k l1 l2]. If [merge] returns [None], the combination
      of values is discarded, otherwise it returns [Some c]
      and [c] is inserted in the result.
    @since 2.3 *)

val group_join_by : ?eq:('a -> 'a -> bool) -> ?hash:('a -> int) ->
  ('b -> 'a) ->
  'a t ->
  'b t ->
  ('a * 'b list) t
(** [group_join_by ?eq ?hash key la lb] associates to every element [x] of
    the first sequence, all the elements [y] of the second
    sequence such that [eq x (key y)]. Elements of the first
    sequences without corresponding values in the second one
    are mapped to [[]]
    precondition: for any [x] and [y], if [eq x y] then [hash x = hash y] must hold.
    @since 2.3 *)

val sublists_of_len :
  ?last:('a list -> 'a list option) ->
  ?offset:int ->
  len:int ->
  'a list ->
  'a list list
(** [sublists_of_len ?last ?offset n l] returns sub-lists of [l] that have length [n].
    By default, these sub-lists are non overlapping:
    [sublists_of_len 2 [1;2;3;4;5;6]] returns [[1;2]; [3;4]; [5;6]].

    Examples:

    - [sublists_of_len 2 [1;2;3;4;5;6] = [[1;2]; [3;4]; [5;6]]].
    - [sublists_of_len 2 ~offset:3 [1;2;3;4;5;6] = [1;2];[4;5]].
    - [sublists_of_len 3 ~last:CCOption.return [1;2;3;4] = [1;2;3];[4]].
    - [sublists_of_len 2 [1;2;3;4;5] = [[1;2]; [3;4]]].

    @param offset the number of elements skipped between two consecutive
      sub-lists. By default it is [n]. If [offset < n], the sub-lists
      will overlap; if [offset > n], some elements will not appear at all.
    @param last if provided and the last group of elements [g] is such
      that [length g < n], [last g] is called. If [last g = Some g'],
      [g'] is appended; otherwise [g] is dropped.
      If [last = CCOption.return], it will simply keep the last group.
      By default, [last = fun _ -> None], i.e. the last group is dropped if shorter than [n].
    @raise Invalid_argument if [offset <= 0] or [n <= 0].
    See {!CCList.sublists_of_len} for more details.

    @since 1.0, but only
    @since 1.5 with labels *)

val chunks : int -> 'a list -> 'a list list
(** [chunks n l] returns consecutives chunks of size at most [n] from [l].
    Each item of [l] will occur in exactly one chunk. Only the last chunk
    might be of length smaller than [n].
    Invariant: [(chunks n l |> List.flatten) = l].
    @since 3.2 *)

val intersperse : x:'a -> 'a list -> 'a list
(** [intersperse ~x l] inserts the element [x] between adjacent elements of the list [l].
    @since 2.1, but only
    @since 2.2 with labels *)

val interleave : 'a list -> 'a list -> 'a list
(** [interleave [x1…xn] [y1…ym]] is [[x1,y1,x2,y2,…]] and finishes with
    the suffix of the longest list.
    @since 2.1, but only
    @since 2.2 with labels *)

val pure : 'a -> 'a t
(** [pure x] is [return x]. *)

val mguard : bool -> unit t
(** [mguard c] is [pure ()] if [c] is true, [[]] otherwise. 
    This is useful to define a list by comprehension, e.g.:
    {[
      # let square_even xs =
            let* x = xs in
            let* () = mguard (x mod 2 = 0) in
            return @@ x * x;;
      val square_even : int list -> int list = <fun>
      # square_even [1;2;4;3;5;2];;
      - : int list = [4; 16; 4]
    ]}
    @since 3.1 *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [funs <*> l] is [product (fun f x -> f x) funs l]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [(<$>)] is [map]. *)

val return : 'a -> 'a t
(** [return x] is [x]. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [l >>= f] is [flat_map f l]. *)

val take : int -> 'a t -> 'a t
(** [take n l] takes the [n] first elements of the list [l], drop the rest. *)

val drop : int -> 'a t -> 'a t
(** [drop n l] drops the [n] first elements of the list [l], keep the rest. *)

val hd_tl : 'a t -> 'a * 'a t
(** [hd_tl (x :: l)] returns [hd, l].
    @raise Failure if the list is empty.
    @since 0.16 *)

val take_drop : int -> 'a t -> 'a t * 'a t
(** [take_drop n l] returns [l1, l2] such that [l1 @ l2 = l] and
    [length l1 = min (length l) n]. *)

val take_while : f:('a -> bool) -> 'a t -> 'a t
(** [take_while ~f l] returns the longest prefix of [l] for which [f] is [true].
    @since 0.13 *)

val drop_while : f:('a -> bool) -> 'a t -> 'a t
(** [drop_while ~f l] drops the longest prefix of [l] for which [f] is [true].
    @since 0.13 *)

val take_drop_while : f:('a -> bool) -> 'a t -> 'a t * 'a t
(** [take_drop_while ~f l] = [take_while ~f l, drop_while ~f l].
    @since 1.2, but only
    @since 2.2 with labels *)

val last : int -> 'a t -> 'a t
(** [last n l] takes the last [n] elements of [l] (or less if
    [l] doesn't have that many elements). *)

val head_opt : 'a t -> 'a option
(** [head_opt l] returns [Some x] (the first element of the list [l]) 
    or [None] if the list [l] is empty.
    @since 0.20 *)

val tail_opt : 'a t -> 'a t option
(** [tail_opt l] returns [Some l'] (the given list [l] without its first element)
    or [None] if the list [l] is empty.
    @since 2.0 *)

val last_opt : 'a t -> 'a option
(** [last_opt l] returns [Some x] (the last element of [l]) or [None] if the list [l] is empty. 
    @since 0.20 *)

val find_pred : f:('a -> bool) -> 'a t -> 'a option
(** [find_pred ~f l] finds the first element of [l] that satisfies [f],
    or returns [None] if no element satisfies [f].
    @since 0.11 *)

val find_opt : f:('a -> bool) -> 'a t -> 'a option
(** [find_opt ~f l] is the safe version of {!find}.
    @since 1.5, but only
    @since 2.2 with labels *)

val find_pred_exn : f:('a -> bool) -> 'a t -> 'a
(** [find_pred_exn ~f l] is the unsafe version of {!find_pred}.
    @raise Not_found if no such element is found.
    @since 0.11 *)

val find_map : f:('a -> 'b option) -> 'a t -> 'b option
(** [find_map ~f l] traverses [l], applying [f] to each element. If for
    some element [x], [f x = Some y], then [Some y] is returned. Otherwise
    the call returns [None].
    @since 0.11 *)

val find_mapi : f:(int -> 'a -> 'b option) -> 'a t -> 'b option
(** [find_mapi ~f l] is like {!find_map}, but also pass the index to the predicate function.
    @since 0.11 *)

val find_idx : f:('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx ~f x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [f x] holds. Otherwise returns [None]. *)

val remove : eq:(('a -> 'a -> bool) [@keep_label]) -> key:('a [@keep_label]) -> 'a t -> 'a t
    (* FIXME: the original CCList.mli uses ~x instead of ~key !! *)
(** [remove ~eq ~key l] removes every instance of [key] from [l]. Tail-recursive.
    @param eq equality function.
    @since 0.11 *)

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
(** [filter_map ~f l] is the sublist of [l] containing only elements for which
    [f] returns [Some e].
    Map and remove elements at the same time. *)

val keep_some : 'a option t -> 'a t
(** [keep_some l] retains only elements of the form [Some x].
    Like [filter_map CCFun.id].
    @since 1.3, but only
    @since 2.2 with labels *)

val keep_ok : ('a, _) result t -> 'a t
(** [keep_ok l] retains only elements of the form [Ok x].
    @since 1.3, but only
    @since 2.2 with labels *)

val all_some : 'a option t -> 'a t option
(** [all_some l] returns [Some l'] if all elements of [l] are of the form [Some x],
    or [None] otherwise.
    @since 1.3, but only
    @since 2.2 with labels *)

val all_ok : ('a, 'err) result t -> ('a t, 'err) result
(** [all_ok l] returns [Ok l'] if all elements of [l] are of the form [Ok x],
    or [Error e] otherwise (with the first error met).
    @since 1.3, but only
    @since 2.2 with labels *)

val sorted_mem : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a -> 'a list -> bool
(** [sorted_mem ~cmp x l] and [mem x l] give the same result for any sorted list [l],
    but potentially more efficiently.
    @since 3.5 *)

val sorted_merge : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a list -> 'a list -> 'a list
(** [sorted_merge ~cmp l1 l2] merges elements from both sorted list using
    the given comparison function [cmp]. *)

val sorted_diff : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a list -> 'a list -> 'a list
(** [sorted_diff ~cmp l1 l2] returns the elements in [l1] that are not in [l2].
    Both lists are assumed to be sorted with respect to [cmp] and
    duplicate elements in the input lists are treated individually;
    for example, [sorted_diff ~cmp [1;1;1;2;2;3] [1;2;2]] would be [[1;1;3]].
    It is the left inverse of [sorted_merge]; that is,
    [sorted_diff ~cmp (sorted_merge ~cmp l1 l2) l2]
    is always equal to [l1] for sorted lists [l1] and [l2].
    @since 3.5 *)
    
val sort_uniq : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a list -> 'a list
(** [sort_uniq ~cmp l] sorts the list [l] using the given comparison function [cmp]
    and remove duplicate elements. *)
    
val sorted_merge_uniq : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a list -> 'a list -> 'a list
(** [sorted_merge_uniq ~cmp l1 l2] merges the sorted lists [l1] and [l2] and
    removes duplicates.
    @since 0.10 *)

val sorted_diff_uniq : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a list -> 'a list -> 'a list
(** [sorted_diff_uniq ~cmp l1 l2] collects the elements in [l1] that are not in [l2]
    and then remove duplicates.
    Both lists are assumed to be sorted with respect to [cmp] and
    duplicate elements in the input lists are treated individually;
    for example, [sorted_diff_uniq ~cmp [1;1;1;2;2] [1;2;2;2]] would be [[1]].
    [sorted_diff_uniq ~cmp l1 l2] and [uniq_succ ~eq (sorted_diff ~cmp l1 l2)]
    always give the same result for sorted [l1] and [l2] and compatible [cmp] and [eq].
    @since 3.5 *)

val is_sorted : cmp:(('a -> 'a -> int) [@keep_label]) -> 'a list -> bool
(** [is_sorted ~cmp l] returns [true] iff [l] is sorted (according to given order).
    @param cmp the comparison function.
    @since 0.17 *)

val sorted_insert : cmp:(('a -> 'a -> int) [@keep_label]) -> ?uniq:bool -> 'a -> 'a list -> 'a list
(** [sorted_insert ~cmp ?uniq x l] inserts [x] into [l] such that, if [l] was sorted,
    then [sorted_insert x l] is sorted too.
    @param uniq if true and [x] is already in sorted position in [l], then
      [x] is not duplicated. Default [false] ([x] will be inserted in any case).
    @since 0.17 *)

val sorted_remove : cmp:(('a -> 'a -> int) [@keep_label]) -> ?all:bool -> 'a -> 'a list -> 'a list
(** [sorted_remove ~cmp x l] removes [x] from a sorted list [l] such that
    the return value is sorted too. By default, it is the left inverse of
    [sorted_insert]; that is, [sorted_remove ~cmp x (sorted_insert ~cmp x l)]
    is equal to [l] for any sorted list [l].
    @param all if true then all occurrences of [x] will be removed. Otherwise, only the first
      [x] will be removed (if any). Default [false] (only the first will be removed).
    @since 3.5 *)

val uniq_succ : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a list -> 'a list
(** [uniq_succ ~eq l] removes duplicate elements that occur one next to the other.
    Examples:
    [uniq_succ [1;2;1] = [1;2;1]].
    [uniq_succ [1;1;2] = [1;2]].
    @since 0.10 *)

val group_succ : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a list -> 'a list list
(** [group_succ ~eq l] groups together consecutive elements that are equal
    according to [eq].
    @since 0.11 *)

(** {2 Indices} *)

val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi ~f l] is like {!map}, but the function [f] is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument. *)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(** [iteri ~f l] is like {!iter}, but the function [f] is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument. *)

val iteri2 : f:(int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iteri2 ~f l1 l2] applies [f] to the two lists [l1] and [l2] simultaneously.
    The integer passed to [f] indicates the index of element.
    @raise Invalid_argument when lists do not have the same length.
    @since 2.0, but only
    @since 2.2 with labels *)

val foldi : f:('b -> int -> 'a -> 'b) -> init:'b -> 'a t -> 'b
(** [foldi ~f ~init l] is like [fold] but it also passes in the index of each element, from [0] to [length l - 1]
    as additional argument to the folded function [f]. Tail-recursive. *)
    
val foldi2 : f:('c -> int -> 'a -> 'b -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
(** [foldi2 ~f ~init l1 l2] folds on the two lists [l1] and [l2],
    with index of each element passed to the function [f].
    Computes [f(… (f init i_0 l1_0 l2_0) …) i_n l1_n l2_n] .
    @raise Invalid_argument when lists do not have the same length.
    @since 2.0, but only
    @since 2.2 with labels *)

val get_at_idx : int -> 'a t -> 'a option
(** [get_at_idx i l] returns [Some i-th] element of the given list [l]
    or [None] if the list [l] is too short.
    If the index is negative, it will get element starting from the end
    of the list [l]. *)

val nth_opt : 'a t -> int -> 'a option
(** [nth_opt l n] returns [Some n-th] element of [l]. Safe version of {!nth}.
    @raise Invalid_argument if the int is negative.
    @since 1.5, but only
    @since 2.2 with labels *)

val get_at_idx_exn : int -> 'a t -> 'a
(** [get_at_idx_exn i l] gets the [i-th] element of [l], or
    @raise Not_found if the index is invalid.
    The first element has index 0.
    If the index is negative, it will get element starting from the end
    of the list. *)

val set_at_idx : int -> 'a -> 'a t -> 'a t
(** [set_at_idx i x l] replaces the [i-th] element with [x] (removes the old one),
    or does nothing if index is too high.
    If the index is negative, it will set element starting from the end
    of the list. *)

val insert_at_idx : int -> 'a -> 'a t -> 'a t
(** [insert_at_idx i x l] inserts [x] at [i-th] position, between the two existing elements.
    If the index is too high, append at the end of the list.
    If the index is negative, it will insert element starting from the end
    of the list. *)

val remove_at_idx : int -> 'a t -> 'a t
(** [remove_at_idx i l] removes element at given index [i].
    Does nothing if the index is too high.
    If the index is negative, it will remove element starting from the end
    of the list. *)

(** {2 Set Operators}

    Those operations maintain the invariant that the list does not
    contain duplicates (if it already satisfies it). *)

val add_nodup : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> 'a t -> 'a t
(** [add_nodup ~eq x set] adds [x] to [set] if it was not already present. Linear time.
    @since 0.11 *)

val remove_one : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> 'a t -> 'a t
(** [remove_one ~eq x set] removes one occurrence of [x] from [set]. Linear time.
    @since 0.11 *)

val mem : ?eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> 'a t -> bool
(** [mem ?eq x l] is [true] iff [x] is equal to an element of [l].
    A comparator function [eq] can be provided. Linear time. *)
    
val subset : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a t -> 'a t -> bool
(** [subset ~eq l1 l2] tests if all elements of the list [l1] are contained
    in the list [l2] by applying [eq]. *)
    
val uniq : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a t -> 'a t
(** [uniq ~eq l] removes duplicates in [l] w.r.t the equality predicate [eq].
    Complexity is quadratic in the length of the list, but the order
    of elements is preserved. If you wish for a faster de-duplication
    but do not care about the order, use {!sort_uniq}. *)

val union : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a t -> 'a t -> 'a t
(** [union ~eq l1 l2] is the union of the lists [l1] and [l2] w.r.t. the equality predicate [eq].
    Complexity is product of length of inputs. *)

val inter : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a t -> 'a t -> 'a t
(** [inter ~eq l1 l2] is the intersection of the lists [l1] and [l2] w.r.t. the equality predicate [eq].
    Complexity is product of length of inputs. *)
    
(** {2 Other Constructors} *)

val range_by : step:(int [@keep_label]) -> int -> int -> int t
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    Use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0].
    @since 0.18 *)

val range : int -> int -> int t
(** [range i j] iterates on integers from [i] to [j] included. It works
    both for decreasing and increasing ranges. *)

val range' : int -> int -> int t
(** [range' i j] is like {!range} but the second bound [j] is excluded.
    For instance [range' 0 5 = [0;1;2;3;4]]. *)

val (--) : int -> int -> int t
(** [i -- j] is the list of integers from [i] to [j] included.
    Infix alias for [range]. *)
    
val (--^) : int -> int -> int t
(** [i --^ j] is the list of integers from [i] to [j] excluded.
    Infix alias for [range'].
    @since 0.17 *)

val replicate : int -> 'a -> 'a t
(** [replicate n x] replicates the given element [x] [n] times. *)

val repeat : int -> 'a t -> 'a t
(** [repeat n l] concatenates the list [l] with itself [n] times. *)

(** {2 Association Lists} *)

module Assoc : sig
  type ('a, 'b) t = ('a*'b) list

  val get : eq:(('a->'a->bool) [@keep_label]) -> 'a -> ('a,'b) t -> 'b option
  (** [get ~eq k alist] returns [Some v] if the given key [k] is present into [alist],
      or [None] if not present. *)
      
  val get_exn : eq:(('a->'a->bool) [@keep_label]) -> 'a -> ('a,'b) t -> 'b
  (** [get_exn ~eq k alist] returns [v] if the element [k] is present into [alist].
      Like [get], but unsafe.
      @raise Not_found if the element is not present. *)

  val set : eq:(('a->'a->bool) [@keep_label]) -> 'a -> 'b -> ('a,'b) t -> ('a,'b) t
  (** [set ~eq k v alist] adds the binding [k, v] into the list [alist] (erase it if already present). *)

  val mem : ?eq:(('a->'a->bool) [@keep_label]) -> 'a -> ('a,_) t -> bool
  (** [mem ?eq k alist] returns [true] iff [k] is a key in [alist].
      @since 0.16 *)
      
  val update :
    eq:(('a->'a->bool) [@keep_label]) -> f:(('b option -> 'b option) [@keep_label]) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [update ~eq ~f k alist] updates [alist] on the key [k], by calling [f (get alist k)]
      and removing [k] if it returns [None], mapping [k] to [v'] if it
      returns [Some v'].
      @since 0.16 *)

  val remove : eq:(('a->'a->bool) [@keep_label]) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [remove ~eq k alist] returns the [alist] without the first pair with key [k], if any.
      @since 0.17 *)
end

val assoc : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> ('a * 'b) t -> 'b
(** [assoc ~eq k alist] returns the value [v] associated with key [k] in [alist].
    Like [Assoc.get_exn].
    @since 2.0 *)

val assoc_opt : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> ('a * 'b) t -> 'b option
(** [assoc_opt ~eq k alist] returns [Some v] if the given key [k] is present into [alist],
    or [None] if not present. Like [Assoc.get].
    @since 1.5, but only
    @since 2.0 with labels *)

val assq_opt : 'a -> ('a * 'b) t -> 'b option
(** [assq_opt k alist] returns [Some v] if the given key [k] is present into [alist].
    Like [Assoc.assoc_opt] but use physical equality instead of structural equality
    to compare keys.
    Safe version of {!assq}.
    @since 1.5, but only
    @since 2.0 with labels *)

val mem_assoc : ?eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> ('a * _) t -> bool
(** [mem_assoc ?eq k alist] returns [true] iff [k] is a key in [alist].
    Like [Assoc.mem].
    @since 2.0 *)

val remove_assoc : eq:(('a -> 'a -> bool) [@keep_label]) -> 'a -> ('a * 'b) t -> ('a * 'b) t
(** [remove_assoc ~eq k alist] returns the [alist] without the first pair with key [k], if any.
    Like [Assoc.remove].
    @since 2.0 *)

(** {2 References on Lists}
    @since 0.3.3 *)

module Ref : sig
  type 'a t = 'a list ref

  val push : 'a t -> 'a -> unit
  (** [push rlist e] adds an element [e] at the head of [rlist]. *)

  val pop : 'a t -> 'a option
  (** [pop rlist] removes and returns [Some e] (the first element of [rlist])
      or [None] if the [rlist] is empty *)
      
  val pop_exn : 'a t -> 'a
  (** [pop_exn rlist] removes and returns the first element of [rlist].
      Unsafe version of {!pop}.
      @raise Failure if the list is empty. *)

  val create : unit -> 'a t
  (** [create ()] creates a new empty reference list. *)

  val clear : _ t -> unit
  (** [clear rlist] removes all elements of [rlist]. *)

  val lift : ('a list -> 'b) -> 'a t -> 'b
  (** [lift f rlist] applies a list function [f] to the content of [rlist]. *)

  val push_list : 'a t -> 'a list -> unit
  (** [push_list rlist l] adds elements of the list [l] at the beginning of the list ref [rlist]. 
      Elements at the end of the list [l] will be at the beginning of the list ref [rlist]. *)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  (** [return] is the Monadic [return]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [ (>>=) ] is the Monadic [bind]. *)
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : f:('b -> 'a -> 'b M.t) -> init:'b -> 'a t -> 'b M.t

  val map_m : f:('a -> 'b M.t) -> 'a t -> 'b t M.t

  val map_m_par : f:('a -> 'b M.t) -> 'a t -> 'b t M.t
  (** [map_m_par ~f (x :: l)] is like {!map_m} but [f x] and [f l] are evaluated
      "in parallel" before combining their result (for instance in Lwt). *)
end

(** {2 Conversions} *)

val random : 'a random_gen -> 'a t random_gen
val random_non_empty : 'a random_gen -> 'a t random_gen
val random_len : int -> 'a random_gen -> 'a t random_gen

val random_choose : 'a t -> 'a random_gen
(** [random_choose l] randomly chooses an element in the list [l].
    @raise Not_found if the list is empty. *)

val random_sequence : 'a random_gen t -> 'a t random_gen

val to_string : ?start:string -> ?stop:string -> ?sep:string ->
  ('a -> string) -> 'a t -> string
(** [to_string ?start ?stop ?sep item_to_string l] print [l] to a string using
    [sep] as a separator between elements of [l].
    @since 2.7 *)

val to_iter : 'a t -> 'a iter
(** [to_iter l] returns a [iter] of the elements of the list [l].
    @since 2.8 *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq l] returns a [Seq.t] of the elements of the list [l].
    Renamed from [to_std_seq] since 3.0.
    @since 3.0 *)

val of_iter : 'a iter -> 'a t
(** [of_iter iter] builds a list from a given [iter].
    In the result, elements appear in the same order as they did in the source [iter].
    @since 2.8 *)

val of_seq_rev : 'a Seq.t -> 'a t
(** [of_seq_rev seq] builds a list from a given [Seq.t], in reverse order.
    Renamed from [of_std_seq_rev] since 3.0.
    @since 3.0 *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq seq] builds a list from a given [Seq.t].
    In the result, elements appear in the same order as they did in the source [Seq.t].
    Renamed from [of_std_seq] since 3.0.
    @since 3.0 *)

val to_gen : 'a t -> 'a gen
(** [to_gen l] returns a [gen] of the elements of the list [l]. *)

val of_gen : 'a gen -> 'a t
(** [of_gen gen] builds a list from a given [gen].
    In the result, elements appear in the same order as they did in the source [gen]. *)

(** {2 Infix Operators}
    It is convenient to {!open CCList.Infix} to access the infix operators
    without cluttering the scope too much.

    @since 0.16 *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** [l >|= f] is the infix version of [map] with reversed arguments. *)

  val (@) : 'a t -> 'a t -> 'a t
  (** [l1 @ l2] concatenates two lists [l1] and [l2].
      As {!append}. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** [funs <*> l] is [product (fun f x -> f x) funs l]. *)

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** [f <$> l] is like {!map}. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [l >>= f] is [flat_map f l]. *)

  val (--) : int -> int -> int t
  (** [i -- j] is the infix alias for [range]. Bounds included. *)

  val (--^) : int -> int -> int t
  (** [i --^ j] is the infix alias for [range']. Second bound [j] excluded.
      @since 0.17 *)

  (** Let operators on OCaml >= 4.08.0, nothing otherwise
      @since 2.8 *)
  include CCShimsMkLet_.S with type 'a t_let := 'a list

  include CCShimsMkLetList_.S
end

(** Let operators on OCaml >= 4.08.0, nothing otherwise
    @since 2.8 *)
include CCShimsMkLet_.S with type 'a t_let := 'a list

(** {2 IO} *)

val pp : ?pp_start:unit printer -> ?pp_stop:unit printer -> ?pp_sep:unit printer ->
  'a printer -> 'a t printer
(** [pp ?pp_start ?pp_stop ?pp_sep ppf l] prints the contents of a list. *)
