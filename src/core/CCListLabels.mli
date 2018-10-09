
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Complements to list} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

include module type of ListLabels

type 'a t = 'a list

val empty : 'a t
(** [empty] is [[]]. *)

val is_empty : _ t -> bool
(** [is_empty l] returns [true] iff [l = []].
    @since 0.11 *)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Safe version of {!List.map}. *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of [map] with reversed arguments.
    @since 0.5 *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] is [x::l].
    @since 0.12 *)

val append : 'a t -> 'a t -> 'a t
(** Safe version of {!List.append}.
    Concatenate two lists. *)

val cons_maybe : 'a option -> 'a t -> 'a t
(** [cons_maybe (Some x) l] is [x :: l].
    [cons_maybe None l] is [l].
    @since 0.13 *)

val (@) : 'a t -> 'a t -> 'a t
(** Like [append].
    Concatenate two lists. *)

val filter : f:('a -> bool) -> 'a t -> 'a t
(** Safe version of {!List.filter}.
    [filter p l] returns all the elements of the list [l]
    that satisfy the predicate [p].  The order of the elements
    in the input list is preserved. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Safe version of [fold_right].
    [fold_right f [a1; ...; an] b] is
    [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

val fold_while : f:('a -> 'b -> 'a * [`Stop | `Continue]) -> init:'a -> 'b t -> 'a
(** Fold until a stop condition via [('a, `Stop)] is
    indicated by the accumulator.
    @since 0.8 *)

val fold_map : f:('acc -> 'a -> 'acc * 'b) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_map ~f ~init l] is a [fold_left]-like function, but it also maps the
    list to another list.
    @since 0.14 *)

val scan_left : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a list -> 'acc list
(** @since 2.2 *)

val fold_map2 : f:('acc -> 'a -> 'b -> 'acc * 'c) -> init:'acc -> 'a list -> 'b list -> 'acc * 'c list
(** [fold_map2] is to [fold_map] what [List.map2] is to [List.map].
    @raise Invalid_argument if the lists do not have the same length.
    @since 0.16 *)

val fold_filter_map : f:('acc -> 'a -> 'acc * 'b option) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_filter_map ~f ~init l] is a [fold_left]-like function, but also
    generates a list of output in a way similar to {!filter_map}.
    @since 0.17 *)

val fold_flat_map : f:('acc -> 'a -> 'acc * 'b list) -> init:'acc -> 'a list -> 'acc * 'b list
(** [fold_flat_map f acc l] is a [fold_left]-like function, but it also maps the
    list to a list of lists that is then [flatten]'d.
    @since 0.14 *)

val count : f:('a -> bool) -> 'a list -> int
(** @since 2.2 *)

val count_true_false : f:('a -> bool) -> 'a list -> int * int
(** @since NEXT_RELEASE *)

val init : int -> f:(int -> 'a) -> 'a t
(** [init len ~f] is [f 0; f 1; ...; f (len-1)].
    @raise Invalid_argument if len < 0.
    @since 0.6 *)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** @since 2.2 *)

val combine_gen : 'a list -> 'b list -> ('a * 'b) gen
(** @since 2.2 *)

val split : ('a * 'b) t -> 'a t * 'b t
(** @since 2.2 *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val compare_lengths : 'a t -> 'b t -> int
(** @since 2.2 *)

val compare_length_with : 'a t -> int -> int
(** @since 2.2 *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val flat_map : f:('a -> 'b t) -> 'a t -> 'b t
(** Map and flatten at the same time (safe). Evaluation order is not guaranteed. *)

val flatten : 'a t t -> 'a t
(** Safe flatten. Concatenate a list of lists. *)

val product : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Cartesian product of the two lists, with the given combinator. *)

val fold_product : f:('c -> 'a -> 'b -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
(** Fold on the cartesian product. *)

val cartesian_product : 'a t t -> 'a t t
(** @since 2.2 *)

val map_product_l : f:('a -> 'b list) -> 'a list -> 'b list list
(** @since 2.2 *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the list. [list_diagonal l] will
    return the list of [List.nth i l, List.nth j l] if [i < j]. *)

val partition_map : f:('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
  'a list -> 'b list * 'c list
(** [partition_map ~f l] maps [f] on [l] and gather results in lists:
    - if [f x = `Left y], adds [y] to the first list.
    - if [f x = `Right z], adds [z] to the second list.
    - if [f x = `Drop], ignores [x].
    @since 0.11 *)

val group_by : ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) ->
  'a t -> 'a list t
(** Group equal elements, regardless of their order of appearance.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 2.3 *)

val join : join_row:('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
(** [join ~join_row a b] combines every element of [a] with every
    element of [b] using [join_row]. If [join_row] returns None, then
    the two elements do not combine. Assume that [b] allows for multiple
    iterations.
    @since 2.3 *)

val join_by : ?eq:('key -> 'key -> bool) -> ?hash:('key -> int) ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:('key -> 'a -> 'b -> 'c option) ->
  'a t ->
  'b t ->
  'c t
(** [join key1 key2 ~merge] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 2.3 *)

val join_all_by : ?eq:('key -> 'key -> bool) -> ?hash:('key -> int) ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:('key -> 'a list -> 'b list -> 'c option) ->
  'a t ->
  'b t ->
  'c t
(** [join_all_by key1 key2 ~merge] is a binary operation
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
(** [group_join_by key2] associates to every element [x] of
    the first sequence, all the elements [y] of the second
    sequence such that [eq x (key y)]. Elements of the first
    sequences without corresponding values in the second one
    are mapped to [[]]
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 2.3 *)


val sublists_of_len :
  ?last:('a list -> 'a list option) ->
  ?offset:int ->
  len:int ->
  'a list ->
  'a list list
(** [sublists_of_len n l] returns sub-lists of [l] that have length [n].
    By default, these sub-lists are non overlapping:
    [sublists_of_len 2 [1;2;3;4;5;6]] returns [[1;2]; [3;4]; [5;6]].

    See {!CCList.sublists_of_len} for more details.

    @since 1.5 *)

val intersperse : x:'a -> 'a list -> 'a list
(** Insert the first argument between every element of the list.
    @since 2.2 *)

val interleave : 'a list -> 'a list -> 'a list
(** [interleave [x1…xn] [y1…ym]] is [x1,y1,x2,y2,…] and finishes with
    the suffix of the longest list.
    @since 2.2 *)

val pure : 'a -> 'a t
(** [pure] is [return]. *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [funs <*> l] is [product fun f x -> f x) funs l]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [(<$>)] is [map]. *)

val return : 'a -> 'a t
(** [return x] is [x]. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [l >>= f] is [flat_map f l]. *)

val take : int -> 'a t -> 'a t
(** Take the [n] first elements, drop the rest. *)

val drop : int -> 'a t -> 'a t
(** Drop the [n] first elements, keep the rest. *)

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
(** @since 2.2 *)

val last : int -> 'a t -> 'a t
(** [last n l] takes the last [n] elements of [l] (or less if
    [l] doesn't have that many elements). *)

val head_opt : 'a t -> 'a option
(** First element.
    @since 0.20 *)

val tail_opt : 'a t -> 'a t option
(** Return the given list without its first element.
    @since 2.0 *)

val last_opt : 'a t -> 'a option
(** Last element.
    @since 0.20 *)

val find_pred : f:('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p].
    @since 0.11 *)

val find_opt : f:('a -> bool) -> 'a t -> 'a option
(** @since 2.2 *)

val find_pred_exn : f:('a -> bool) -> 'a t -> 'a
(** Unsafe version of {!find_pred}.
    @raise Not_found if no such element is found.
    @since 0.11 *)

val find_map : f:('a -> 'b option) -> 'a t -> 'b option
(** [find_map ~f l] traverses [l], applying [f] to each element. If for
    some element [x], [f x = Some y], then [Some y] is returned. Otherwise
    the call returns [None].
    @since 0.11 *)

val find_mapi : f:(int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find_map}, but also pass the index to the predicate function.
    @since 0.11 *)

val find_idx : f:('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None]. *)

val remove : eq:('a -> 'a -> bool) -> key:'a -> 'a t -> 'a t
(** [remove ~key l] removes every instance of [key] from [l]. Tail-recursive.
    @param eq equality function.
    @since 0.11 *)

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
(** [filter_map ~f l] is the sublist of [l] containing only elements for which
    [f] returns [Some e].
    Map and remove elements at the same time. *)

val keep_some : 'a option t -> 'a t
(** @since 2.2 *)

val keep_ok : ('a, _) Result.result t -> 'a t
(** @since 2.2 *)

val all_some : 'a option t -> 'a t option
(** @since 2.2 *)

val all_ok : ('a, 'err) Result.result t -> ('a t, 'err) Result.result
(** @since 2.2 *)

val sorted_merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merges elements from both sorted list. *)

val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Sort the list and remove duplicate elements. *)

val sorted_merge_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** [sorted_merge_uniq l1 l2] merges the sorted lists [l1] and [l2] and
    removes duplicates.
    @since 0.10 *)

val is_sorted : cmp:('a -> 'a -> int) -> 'a list -> bool
(** [is_sorted l] returns [true] iff [l] is sorted (according to given order).
    @param cmp the comparison function (default [Pervasives.compare]).
    @since 0.17 *)

val sorted_insert : cmp:('a -> 'a -> int) -> ?uniq:bool -> 'a -> 'a list -> 'a list
(** [sorted_insert x l] inserts [x] into [l] such that, if [l] was sorted,
    then [sorted_insert x l] is sorted too.
    @param uniq if true and [x] is already in sorted position in [l], then
      [x] is not duplicated. Default [false] ([x] will be inserted in any case).
    @since 0.17 *)

val uniq_succ : eq:('a -> 'a -> bool) -> 'a list -> 'a list
(** [uniq_succ l] removes duplicate elements that occur one next to the other.
    Examples:
    [uniq_succ [1;2;1] = [1;2;1]].
    [uniq_succ [1;1;2] = [1;2]].
    @since 0.10 *)

val group_succ : eq:('a -> 'a -> bool) -> 'a list -> 'a list list
(** [group_succ ~eq l] groups together consecutive elements that are equal
    according to [eq].
    @since 0.11 *)

(** {2 Indices} *)

val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** Like {!map}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument. *)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(** Like {!iter}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument. *)

val iteri2 : f:(int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** @since 2.2 *)

val foldi : f:('b -> int -> 'a -> 'b) -> init:'b -> 'a t -> 'b
(** Like [fold] but it also passes in the index of each element to the folded function. Tail-recursive. *)

val foldi2 : f:('c -> int -> 'a -> 'b -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
(** @since 2.2 *)

val get_at_idx : int -> 'a t -> 'a option
(** Get by index in the list.
    If the index is negative, it will get element starting from the end
    of the list. *)

val nth_opt : 'a t -> int -> 'a option
(** @since 2.2 *)

val get_at_idx_exn : int -> 'a t -> 'a
(** Get the i-th element, or
    @raise Not_found if the index is invalid.
    If the index is negative, it will get element starting from the end
    of the list. *)

val set_at_idx : int -> 'a -> 'a t -> 'a t
(** Set i-th element (removes the old one), or does nothing if
    index is too high.
    If the index is negative, it will set element starting from the end
    of the list. *)

val insert_at_idx : int -> 'a -> 'a t -> 'a t
(** Insert at i-th position, between the two existing elements. If the
    index is too high, append at the end of the list.
    If the index is negative, it will insert element starting from the end
    of the list. *)

val remove_at_idx : int -> 'a t -> 'a t
(** Remove element at given index. Does nothing if the index is
    too high.
    If the index is negative, it will remove element starting from the end
    of the list. *)

(** {2 Set Operators}

    Those operations maintain the invariant that the list does not
    contain duplicates (if it already satisfies it). *)

val add_nodup : eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
(** [add_nodup x set] adds [x] to [set] if it was not already present. Linear time.
    @since 0.11 *)

val remove_one : eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
(** [remove_one x set] removes one occurrence of [x] from [set]. Linear time.
    @since 0.11 *)

val mem : eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** Membership to the list. Linear time. *)

val subset : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test for inclusion. *)

val uniq : eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** Remove duplicates w.r.t the equality predicate.
    Complexity is quadratic in the length of the list, but the order
    of elements is preserved. If you wish for a faster de-duplication
    but do not care about the order, use {!sort_uniq}. *)

val union : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** List union. Complexity is product of length of inputs. *)

val inter : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** List intersection. Complexity is product of length of inputs. *)

(** {2 Other Constructors} *)

val range_by : step:int -> int -> int -> int t
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0].
    @since 0.18 *)

val range : int -> int -> int t
(** [range i j] iterates on integers from [i] to [j] included. It works
    both for decreasing and increasing ranges. *)

val range' : int -> int -> int t
(** Like {!range} but the second bound is excluded.
    For instance [range' 0 5 = [0;1;2;3;4]]. *)

val (--) : int -> int -> int t
(** Infix alias for [range]. *)

val (--^) : int -> int -> int t
(** Infix alias for [range'].
    @since 0.17 *)

val replicate : int -> 'a -> 'a t
(** Replicate the given element [n] times. *)

val repeat : int -> 'a t -> 'a t
(** Concatenate the list with itself [n] times. *)

(** {2 Association Lists} *)

module Assoc : sig
  type ('a, 'b) t = ('a*'b) list

  val get : eq:('a->'a->bool) -> 'a -> ('a,'b) t -> 'b option
  (** Find the element. *)

  val get_exn : eq:('a->'a->bool) -> 'a -> ('a,'b) t -> 'b
  (** Like [get], but unsafe.
      @raise Not_found if the element is not present. *)

  val set : eq:('a->'a->bool) -> 'a -> 'b -> ('a,'b) t -> ('a,'b) t
  (** Add the binding into the list (erase it if already present). *)

  val mem : eq:('a->'a->bool) -> 'a -> ('a,_) t -> bool
  (** [mem x l] returns [true] iff [x] is a key in [l].
      @since 0.16 *)

  val update :
    eq:('a->'a->bool) -> f:('b option -> 'b option) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [update k ~f l] updates [l] on the key [k], by calling [f (get l k)]
      and removing [k] if it returns [None], mapping [k] to [v'] if it
      returns [Some v'].
      @since 0.16 *)

  val remove : eq:('a->'a->bool) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [remove x l] removes the first occurrence of [k] from [l].
      @since 0.17 *)
end

val assoc : eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b
(** Like [Assoc.get_exn].
    @since 2.0 *)

val assoc_opt : eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b option
(** Like [Assoc.get].
    @since 2.0 *)

val assq_opt : 'a -> ('a * 'b) t -> 'b option
(** Safe version of {!assq}.
    @since 2.0 *)

val mem_assoc : eq:('a -> 'a -> bool) -> 'a -> ('a * _) t -> bool
(** Like [Assoc.mem].
    @since 2.0 *)

val remove_assoc : eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> ('a * 'b) t
(** Like [Assoc.remove].
    @since 2.0 *)

(** {2 References on Lists}
    @since 0.3.3 *)

module Ref : sig
  type 'a t = 'a list ref

  val push : 'a t -> 'a -> unit

  val pop : 'a t -> 'a option

  val pop_exn : 'a t -> 'a
  (** Unsafe version of {!pop}.
      @raise Failure if the list is empty. *)

  val create : unit -> 'a t
  (** Create a new list reference. *)

  val clear : _ t -> unit
  (** Remove all elements. *)

  val lift : ('a list -> 'b) -> 'a t -> 'b
  (** Apply a list function to the content. *)

  val push_list : 'a t -> 'a list -> unit
  (** Add elements of the list at the beginning of the list ref. Elements
      at the end of the list will be at the beginning of the list ref. *)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  (** Monadic [return]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic [bind]. *)

end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : f:('b -> 'a -> 'b M.t) -> init:'b -> 'a t -> 'b M.t

  val map_m : f:('a -> 'b M.t) -> 'a t -> 'b t M.t

  val map_m_par : f:('a -> 'b M.t) -> 'a t -> 'b t M.t
  (** Like {!map_m} but [map_m_par f (x::l)] evaluates [f x] and
      [f l] "in parallel" before combining their result (for instance
      in Lwt). *)
end

(** {2 Conversions} *)

val random : 'a random_gen -> 'a t random_gen
val random_non_empty : 'a random_gen -> 'a t random_gen
val random_len : int -> 'a random_gen -> 'a t random_gen

val random_choose : 'a t -> 'a random_gen
(** Randomly choose an element in the list.
    @raise Not_found if the list is empty. *)

val random_sequence : 'a random_gen t -> 'a t random_gen

val to_seq : 'a t -> 'a sequence
(** Return a [sequence] of the elements of the list. *)

val of_seq : 'a sequence -> 'a t
(** Build a list from a given [sequence]. *)

val to_gen : 'a t -> 'a gen
(** Return a [gen] of the elements of the list. *)

val of_gen : 'a gen -> 'a t
(** Build a list from a given [gen]. *)

val to_klist : 'a t -> 'a klist
(** Return a [klist] of the elements of the list. *)

val of_klist : 'a klist -> 'a t
(** Build a list from a given [klist]. *)

(** {2 Infix Operators}
    It is convenient to {!open CCList.Infix} to access the infix operators
    without cluttering the scope too much.

    @since 0.16 *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix version of [map] with reversed arguments. *)

  val (@) : 'a t -> 'a t -> 'a t
  (** As {!append}. Concatenate two lists. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** [funs <*> l] is [product (fun f x -> f x) funs l]. *)

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** As {!map}. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [l >>= f] is [flat_map f l]. *)

  val (--) : int -> int -> int t
  (** Infix alias for [range]. Bounds included. *)

  val (--^) : int -> int -> int t
  (** Infix alias for [range']. Second bound excluded. *)

  (** @since 0.17 *)
end

(** {2 IO} *)

val pp : ?start:string -> ?stop:string -> ?sep:string ->
  'a printer -> 'a t printer
(** Print the contents of a list. *)
