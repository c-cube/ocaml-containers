
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Map specialized for Int keys}

    {b status: stable}
    @since 0.10 *)

type +'a t

val empty : 'a t

val is_empty : _ t -> bool
(** Is the map empty?
    @since 2.3 *)

val singleton : int -> 'a -> 'a t

val doubleton : int -> 'a -> int -> 'a -> 'a t

val mem : int -> _ t -> bool

val find : int -> 'a t -> 'a option

val find_exn : int -> 'a t -> 'a
(** Same as {!find} but unsafe.
    @raise Not_found if key is not present. *)

val add : int -> 'a -> 'a t -> 'a t

val remove : int -> 'a t -> 'a t

val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal ~eq a b] checks whether [a] and [b] have the same set of pairs
    (key, value), comparing values with [eq].
    @since 0.13 *)

val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Total order between maps; the precise order is unspecified.
    @since 0.13 *)

val update : int -> ('a option -> 'a option) -> 'a t -> 'a t

val filter : (int -> 'a -> bool) -> 'a t -> 'a t
(** Filter values using the given predicate
    @since 2.3 *)

val filter_map : (int -> 'a -> 'b option) -> 'a t -> 'b t
(** Filter-map values using the given function
    @since 2.3 *)

val cardinal : _ t -> int
(** Number of bindings in the map. Linear time. *)

val iter : (int -> 'a -> unit) -> 'a t -> unit

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** @since 0.17 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** @since 0.17 *)

val choose : 'a t -> (int * 'a) option

val choose_exn : 'a t -> int * 'a
(** @raise Not_found if not pair was found. *)

val union : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val inter : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val merge :
  f:(int -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
  'a t -> 'b t -> 'c t
(** [merge ~f m1 m2] merges [m1] and [m2] together, calling [f] once on every
    key that occurs in at least one of [m1] and [m2].
    if [f k binding = Some c] then [k -> c] is part of the result,
    else [k] is not part of the result.
    @since 2.3 *)

(** {2 Whole-collection operations} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

val add_list : 'a t -> (int * 'a) list -> 'a t

val of_list : (int * 'a) list -> 'a t

val to_list : 'a t -> (int * 'a) list

val add_iter : 'a t -> (int * 'a) iter -> 'a t

val of_iter : (int * 'a) iter -> 'a t

val to_iter : 'a t -> (int * 'a) iter

val keys : _ t -> int iter

val values : 'a t -> 'a iter

val add_gen : 'a t -> (int * 'a) gen -> 'a t
(** @since 0.13 *)

val of_gen : (int * 'a) gen -> 'a t
(** @since 0.13 *)

val to_gen : 'a t -> (int * 'a) gen
(** @since 0.13 *)

val add_klist : 'a t -> (int * 'a) klist -> 'a t
(** @since 0.13 *)

val of_klist : (int * 'a) klist -> 'a t
(** @since 0.13 *)

val to_klist : 'a t -> (int * 'a) klist
(** @since 0.13 *)

type 'a tree = unit -> [`Nil | `Node of 'a * 'a tree list]

val as_tree : 'a t -> [`Node of int * int | `Leaf of int * 'a ] tree

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : 'a printer -> 'a t printer
(** @since 0.13 *)

(** Helpers *)

(**/**)

module Bit : sig
  type t = private int
  val min_int : t
  val highest : int -> t
  val equal_int : int -> t -> bool
end
val check_invariants : _ t -> bool

(**/**)
