
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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 complements to list} *)

type 'a t = 'a list

val empty : 'a t

val is_empty : _ t -> bool
(** [is_empty l] returns [true] iff [l = []]
    @since 0.11 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Safe version of map *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of [map] with reversed arguments
    @since 0.5 *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] is [x::l]
    @since 0.12 *)

val append : 'a t -> 'a t -> 'a t
(** Safe version of append *)

val cons_maybe : 'a option -> 'a t -> 'a t
(** [cons_maybe (Some x) l] is [x :: l]
    [cons_maybe None l] is [l]
    @since NEXT_RELEASE *)

val (@) : 'a t -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t
(** Safe version of {!List.filter} *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Safe version of [fold_right] *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Fold until a stop condition via [('a, `Stop)] is
    indicated by the accumulator
    @since 0.8 *)

val init : int -> (int -> 'a) -> 'a t
(** Similar to {!Array.init}
    @since 0.6 *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** map and flatten at the same time (safe). Evaluation order is not guaranteed. *)

val flatten : 'a t t -> 'a t
(** Safe flatten *)

val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** cartesian product of the two lists, with the given combinator *)

val fold_product : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** Fold on the cartesian product *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the list. [list_diagonal l] will
    return the list of [List.nth i l, List.nth j l] if [i < j]. *)

val partition_map : ('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
                    'a list -> 'b list * 'c list
(** [partition_map f l] maps [f] on [l] and gather results in lists:
    - if [f x = `Left y], adds [y] to the first list
    - if [f x = `Right z], adds [z] to the second list
    - if [f x = `Drop], ignores [x]
    @since 0.11 *)

val pure : 'a -> 'a t

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val (<$>) : ('a -> 'b) -> 'a t -> 'b t

val return : 'a -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val take : int -> 'a t -> 'a t
(** take the [n] first elements, drop the rest *)

val drop : int -> 'a t -> 'a t
(** drop the [n] first elements, keep the rest *)

val take_drop : int -> 'a t -> 'a t * 'a t
(** [take_drop n l] returns [l1, l2] such that [l1 @ l2 = l] and
    [length l1 = min (length l) n] *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** @since NEXT_RELEASE *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** @since NEXT_RELEASE *)

val split : int -> 'a t -> 'a t * 'a t
(** synonym to {!take_drop}
    @deprecated since NEXT_RELEASE: conflict with the {!List.split} standard function *)

val last : int -> 'a t -> 'a t
(** [last n l] takes the last [n] elements of [l] (or less if
    [l] doesn't have that many elements *)

val find_pred : ('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p]
    @since 0.11 *)

val find_pred_exn : ('a -> bool) -> 'a t -> 'a
(** Unsafe version of {!find_pred}
    @raise Not_found if no such element is found
    @since 0.11 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find f l] traverses [l], applying [f] to each element. If for
    some element [x], [f x = Some y], then [Some y] is returned. Otherwise
    the call returns [None]
    @since 0.11 *)

val find : ('a -> 'b option) -> 'a list -> 'b option
(** @deprecated since 0.11 in favor of {!find_map}, for the name is too confusing *)

val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find_map}, but also pass the index to the predicate function.
    @since 0.11 *)

val findi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** @deprecated since 0.11 in favor of {!find_mapi}, name is too confusing
    @since 0.3.4 *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None] *)

val remove : ?eq:('a -> 'a -> bool) -> x:'a -> 'a t -> 'a t
(** [remove ~x l] removes every instance of [x] from [l]. Tailrec.
    @param eq equality function
    @since 0.11 *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map and remove elements at the same time *)

val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** merges elements from both sorted list *)

val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Sort the list and remove duplicate elements *)

val sorted_merge_uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** [sorted_merge_uniq l1 l2] merges the sorted lists [l1] and [l2] and
    removes duplicates
    @since 0.10 *)

val uniq_succ : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
(** [uniq_succ l] removes duplicate elements that occur one next to the other.
    Examples:
    [uniq_succ [1;2;1] = [1;2;1]]
    [uniq_succ [1;1;2] = [1;2]]
    @since 0.10 *)

val group_succ : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list list
(** [group_succ ~eq l] groups together consecutive elements that are equal
    according to [eq]
    @since 0.11 *)

(** {2 Indices} *)

module Idx : sig
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** fold on list, with index *)

  val get : 'a t -> int -> 'a option

  val get_exn : 'a t -> int -> 'a
  (** get the i-th element, or
      @raise Not_found if the index is invalid *)

  val set : 'a t -> int -> 'a -> 'a t
  (** set i-th element (removes the old one), or does nothing if
      index too high *)

  val insert : 'a t -> int -> 'a -> 'a t
  (** insert at i-th position, between the two existing elements. If the
      index is too high, append at the end of the list *)

  val remove : 'a t -> int -> 'a t
  (** Remove element at given index. Does nothing if the index is
      too high. *)
end

(** {2 Set Operators} *)

module Set : sig
  val add : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  (** [add x set] adds [x] to [set] if it was not already present. Linear time.
      @since 0.11 *)

  val remove : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  (** [remove x set] removes one occurrence of [x] from [set]. Linear time.
      @since 0.11 *)

  val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
  (** membership to the list. Linear time *)

  val subset : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** test for inclusion *)

  val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
  (** list uniq: remove duplicates w.r.t the equality predicate.
      Complexity is quadratic in the length of the list, but the order
      of elements is preserved. If you wish for a faster de-duplication
      but do not care about the order, use {!sort_uniq}*)

  val union : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  (** list union. Complexity is product of length of inputs. *)

  val inter : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  (** list intersection. Complexity is product of length of inputs., *)
end

(** {2 Other Constructors} *)

val range : int -> int -> int t
(** [range i j] iterates on integers from [i] to [j] included . It works
    both for decreasing and increasing ranges *)

val range' : int -> int -> int t
(** Same as {!range} but the second bound is excluded.
    For instance [range' 0 5 = [0;1;2;3;4]] *)

val (--) : int -> int -> int t
(** Infix alias for [range] *)

val replicate : int -> 'a -> 'a t
(** replicate the given element [n] times *)

val repeat : int -> 'a t -> 'a t
(** concatenate the list with itself [n] times *)

(** {2 Association Lists} *)

module Assoc : sig
  type ('a, 'b) t = ('a*'b) list

  val get : ?eq:('a->'a->bool) -> ('a,'b) t -> 'a -> 'b option
  (** Find the element *)

  val get_exn : ?eq:('a->'a->bool) -> ('a,'b) t -> 'a -> 'b
  (** Same as [get]
      @raise Not_found if the element is not present *)

  val set : ?eq:('a->'a->bool) -> ('a,'b) t -> 'a -> 'b -> ('a,'b) t
  (** Add the binding into the list (erase it if already present) *)
end

(** {2 Zipper} *)

module Zipper : sig
  type 'a t = 'a list * 'a list

  val empty : 'a t
  (** Empty zipper *)

  val is_empty : _ t -> bool
  (** Empty zipper, or at the end of the zipper? *)

  val to_list : 'a t -> 'a list
  (** Convert the zipper back to a list *)

  val make : 'a list -> 'a t
  (** Create a zipper pointing at the first element of the list *)

  val left : 'a t -> 'a t
  (** Go to the left, or do nothing if the zipper is already at leftmost pos *)

  val right : 'a t -> 'a t
  (** Go to the right, or do nothing if the zipper is already at rightmost pos *)

  val modify : ('a option -> 'a option) -> 'a t -> 'a t
  (** Modify the current element, if any, by returning a new element, or
      returning [None] if the element is to be deleted *)

  val focused : 'a t -> 'a option
  (** Returns the focused element, if any. [focused zip = Some _] iff
      [empty zip = false] *)

  val focused_exn : 'a t -> 'a
  (** Returns the focused element, or
      @raise Not_found if the zipper is at an end *)
end

(** {2 References on Lists}
@since 0.3.3 *)

module Ref : sig
  type 'a t = 'a list ref

  val push : 'a t -> 'a -> unit

  val pop : 'a t -> 'a option

  val pop_exn : 'a t -> 'a
  (** Unsafe version of {!pop}.
      @raise Failure if the list is empty *)

  val create : unit -> 'a t
  (** Create a new list reference *)

  val clear : _ t -> unit
  (** Remove all elements *)

  val lift : ('a list -> 'b) -> 'a t -> 'b
  (** Apply a list function to the content *)

  val push_list : 'a t -> 'a list -> unit
  (** Add elements of the list at the beginning of the list ref. Elements
      at the end of the list will be at the beginning of the list ref *)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t

  val map_m_par : ('a -> 'b M.t) -> 'a t -> 'b t M.t
  (** Same as {!map_m} but [map_m_par f (x::l)] evaluates [f x] and
      [f l] "in parallel" before combining their result (for instance
      in Lwt). *)
end

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val random : 'a random_gen -> 'a t random_gen
val random_non_empty : 'a random_gen -> 'a t random_gen
val random_len : int -> 'a random_gen -> 'a t random_gen

val random_choose : 'a t -> 'a random_gen
(** Randomly choose an element in the list.
    @raise Not_found if the list is empty *)

val random_sequence : 'a random_gen t -> 'a t random_gen

val to_seq : 'a t -> 'a sequence
val of_seq : 'a sequence -> 'a t

val to_gen : 'a t -> 'a gen
val of_gen : 'a gen -> 'a t

val to_klist : 'a t -> 'a klist
val of_klist : 'a klist -> 'a t

(** {2 IO} *)

val pp : ?start:string -> ?stop:string -> ?sep:string ->
         'a printer -> 'a t printer

val print : ?start:string -> ?stop:string -> ?sep:string ->
            'a formatter -> 'a t formatter
