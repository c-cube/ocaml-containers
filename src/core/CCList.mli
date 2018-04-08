
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Complements to list} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

include module type of List

type 'a t = 'a list

val empty : 'a t
(** [empty] is [[]]. *)

val is_empty : _ t -> bool
(** [is_empty l] returns [true] iff [l = []].
    @since 0.11 *)

val map : ('a -> 'b) -> 'a t -> 'b t
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

val filter : ('a -> bool) -> 'a t -> 'a t
(** Safe version of {!List.filter}.
    [filter p l] returns all the elements of the list [l]
    that satisfy the predicate [p].  The order of the elements
    in the input list is preserved. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Safe version of [fold_right].
    [fold_right f [a1; ...; an] b] is
    [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Fold until a stop condition via [('a, `Stop)] is
    indicated by the accumulator.
    @since 0.8 *)

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_map f acc l] is a [fold_left]-like function, but it also maps the
    list to another list.
    @since 0.14 *)

val scan_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc list
(** [scan_left f acc l] returns the list [[acc; f acc x0; f (f acc x0) x1; ...]]
    where [x0], [x1], etc. are the elements of [l].
    @since 1.2 *)

val fold_map2 : ('acc -> 'a -> 'b -> 'acc * 'c) -> 'acc -> 'a list -> 'b list -> 'acc * 'c list
(** [fold_map2] is to [fold_map] what [List.map2] is to [List.map].
    @raise Invalid_argument if the lists do not have the same length.
    @since 0.16 *)

val fold_filter_map : ('acc -> 'a -> 'acc * 'b option) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_filter_map f acc l] is a [fold_left]-like function, but also
    generates a list of output in a way similar to {!filter_map}.
    @since 0.17 *)

val fold_flat_map : ('acc -> 'a -> 'acc * 'b list) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_flat_map f acc l] is a [fold_left]-like function, but it also maps the
    list to a list of lists that is then [flatten]'d.
    @since 0.14 *)

val count : ('a -> bool) -> 'a list -> int
(** [count f l] counts how much elements of [l] comply with the function [f].
    @since 1.5 *)

val init : int -> (int -> 'a) -> 'a t
(** [init len f] is [f 0; f 1; ...; f (len-1)].
    @raise Invalid_argument if len < 0.
    @since 0.6 *)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** Like {!List.combine} but tail-recursive.
    Transform a pair of lists into a list of pairs:
    [combine [a1; ...; an] [b1; ...; bn]] is
    [[(a1,b1); ...; (an,bn)]].
    @raise Invalid_argument if the lists have distinct lengths.
    @since 1.2 *)

val combine_gen : 'a list -> 'b list -> ('a * 'b) gen
(** Lazy version of {!combine}.
    Unlike {!combine}, it does not fail if the lists have different
    lengths;
    instead, the output has as many pairs as the smallest input list.
    @since 1.2 *)

val split : ('a * 'b) t -> 'a t * 'b t
(** A tail-recursive version of {!List.split}.
    Transform a list of pairs into a pair of lists:
    [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])]. *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val compare_lengths : 'a t -> 'b t -> int
(** Equivalent to [compare (length l1) (length l2)] but more efficient.
    Compare the lengths of two lists.
    @since 1.5 *)

val compare_length_with : 'a t -> int -> int
(** Equivalent to [compare (length l) x] but more efficient.
    Compare the length of a list to an integer.
    @since 1.5 *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map and flatten at the same time (safe). Evaluation order is not guaranteed. *)

val flatten : 'a t t -> 'a t
(** Safe flatten. Concatenate a list of lists. *)

val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Cartesian product of the two lists, with the given combinator. *)

val fold_product : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** Fold on the cartesian product. *)

val cartesian_product : 'a t t -> 'a t t
(** Produce the cartesian product of this list of lists,
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
    @since 1.2 *)

val map_product_l : ('a -> 'b list) -> 'a list -> 'b list list
(** [map_product_l f l] maps each element of [l] to a list of
    objects of type ['b] using [f].
    We obtain [[l1;l2;...;ln]] where [length l=n] and [li : 'b list].
    Then, it returns all the ways of picking exactly one element per [li].
    @since 1.2 *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the list. [list_diagonal l] will
    return the list of [List.nth i l, List.nth j l] if [i < j]. *)

val partition_map : ('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
  'a list -> 'b list * 'c list
(** [partition_map f l] maps [f] on [l] and gather results in lists:
    - if [f x = `Left y], adds [y] to the first list.
    - if [f x = `Right z], adds [z] to the second list.
    - if [f x = `Drop], ignores [x].
    @since 0.11 *)

val sublists_of_len :
  ?last:('a list -> 'a list option) ->
  ?offset:int ->
  int ->
  'a list ->
  'a list list
(** [sublists_of_len n l] returns sub-lists of [l] that have length [n].
    By default, these sub-lists are non overlapping:
    [sublists_of_len 2 [1;2;3;4;5;6]] returns [[1;2]; [3;4]; [5;6]].

    Examples:

    - [sublists_of_len 2 [1;2;3;4;5;6] = [[1;2]; [3;4]; [5;6]]].
    - [sublists_of_len 2 ~offset:3 [1;2;3;4;5;6] = [1;2];[4;5]].
    - [sublists_of_len 3 ~last:CCOpt.return [1;2;3;4] = [1;2;3];[4]].
    - [sublists_of_len 2 [1;2;3;4;5] = [[1;2]; [3;4]]].

    @param offset the number of elements skipped between two consecutive
      sub-lists. By default it is [n]. If [offset < n], the sub-lists
      will overlap; if [offset > n], some elements will not appear at all.
    @param last if provided and the last group of elements [g] is such
      that [length g < n], [last g] is called. If [last g = Some g'],
      [g'] is appended; otherwise [g] is dropped.
      If [last = CCOpt.return], it will simply keep the last group.
      By default, [last = fun _ -> None], i.e. the last group is dropped if shorter than [n].
    @raise Invalid_argument if [offset <= 0] or [n <= 0].
    @since 1.0 *)

val intersperse : 'a -> 'a list -> 'a list
(** Insert the first argument between every element of the list.
    @since 2.1 *)

val interleave : 'a list -> 'a list -> 'a list
(** [interleave [x1…xn] [y1…ym]] is [x1,y1,x2,y2,…] and finishes with
    the suffix of the longest list.
    @since 2.1 *)

val pure : 'a -> 'a t
(** [pure] is [return]. *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [funs <*> l] is [product (fun f x -> f x) funs l]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [(<$>)] = [map]. *)

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

val take_while : ('a -> bool) -> 'a t -> 'a t
(** [take_while f l] returns the longest prefix of [l] for which [f] is [true].
    @since 0.13 *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** [drop_while f l] drops the longest prefix of [l] for which [f] is [true].
    @since 0.13 *)

val take_drop_while : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [take_drop_while p l] = [take_while p l, drop_while p l].
    @since 1.2 *)

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

val find_pred : ('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p].
    @since 0.11 *)

val find_opt : ('a -> bool) -> 'a t -> 'a option
(** Safe version of {!find}.
    @since 1.5 *)

val find_pred_exn : ('a -> bool) -> 'a t -> 'a
(** Unsafe version of {!find_pred}.
    @raise Not_found if no such element is found.
    @since 0.11 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f l] traverses [l], applying [f] to each element. If for
    some element [x], [f x = Some y], then [Some y] is returned. Otherwise
    the call returns [None].
    @since 0.11 *)

val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find_map}, but also pass the index to the predicate function.
    @since 0.11 *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None]. *)

val remove : eq:('a -> 'a -> bool) -> x:'a -> 'a t -> 'a t
(** [remove ~x l] removes every instance of [x] from [l]. Tail-recursive.
    @param eq equality function.
    @since 0.11 *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f l] is the sublist of [l] containing only elements for which
    [f] returns [Some e].
    Map and remove elements at the same time. *)

val keep_some : 'a option t -> 'a t
(** [keep_some l] retains only elements of the form [Some x].
    Like [filter_map CCFun.id].
    @since 1.3 *)

val keep_ok : ('a, _) Result.result t -> 'a t
(** [keep_ok l] retains only elements of the form [Ok x].
    @since 1.3 *)

val all_some : 'a option t -> 'a t option
(** [all_some l] returns [Some l'] if all elements of [l] are of the form [Some x],
    or [None] otherwise.
    @since 1.3 *)

val all_ok : ('a, 'err) Result.result t -> ('a t, 'err) Result.result
(** [all_ok l] returns [Ok l'] if all elements of [l] are of the form [Ok x],
    or [Error e] otherwise (with the first error met).
    @since 1.3 *)

val sorted_merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merge elements from both sorted list. *)

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

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Like {!map}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Like {!iter}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument. *)

val iteri2 : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iter on two lists.
    @raise Invalid_argument when lists do not have the same length.
    @since 2.0 *)

val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Like [fold] but it also passes in the index of each element to the folded function. Tail-recursive. *)

val foldi2 : ('c -> int -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** Fold on two lists, with index.
    @raise Invalid_argument when lists do not have the same length.
    @since 2.0 *)

val get_at_idx : int -> 'a t -> 'a option
(** Get by index in the list.
    If the index is negative, it will get element starting from the end
    of the list. *)

val nth_opt : 'a t -> int -> 'a option
(** Safe version of {!nth}.
    @raise Invalid_argument if the int is negative.
    @since 1.5 *)

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
    Use a negative [step] for a decreasing list.
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
    @since 1.5 *)

val assq_opt : 'a -> ('a * 'b) t -> 'b option
(** Safe version of {!assq}.
    @since 1.5 *)

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

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t

  val map_m_par : ('a -> 'b M.t) -> 'a t -> 'b t M.t
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
(** Build a list from a given [sequence].
    In the result, elements appear in the same order as they did in the source [sequence]. *)

val to_gen : 'a t -> 'a gen
(** Return a [gen] of the elements of the list. *)

val of_gen : 'a gen -> 'a t
(** Build a list from a given [gen].
    In the result, elements appear in the same order as they did in the source [gen]. *)

val to_klist : 'a t -> 'a klist
(** Return a [klist] of the elements of the list. *)

val of_klist : 'a klist -> 'a t
(** Build a list from a given [klist].
    In the result, elements appear in the same order as they did in the source [klist]. *)

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
