
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extension to the standard Hashtbl}

@since 0.4 *)

type 'a sequence = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

val get : ('a,'b) Hashtbl.t -> 'a -> 'b option
(** Safe version of {!Hashtbl.find} *)

val get_or : ('a,'b) Hashtbl.t -> 'a -> or_:'b -> 'b
(** [get_or tbl k ~or_] returns the value associated to [k] if present,
    and returns [or_] otherwise (if [k] doesn't belong in [tbl])
    @since 0.16 *)

val keys : ('a,'b) Hashtbl.t -> 'a sequence
(** Iterate on keys (similar order as {!Hashtbl.iter}) *)

val values : ('a,'b) Hashtbl.t -> 'b sequence
(** Iterate on values in the table *)

val keys_list : ('a, 'b) Hashtbl.t -> 'a list
(** [keys_list t] is the list of keys in [t].
    @since 0.8 *)

val values_list : ('a, 'b) Hashtbl.t -> 'b list
(** [values_list t] is the list of values in [t].
    @since 0.8 *)

val map_list : ('a -> 'b -> 'c) -> ('a, 'b) Hashtbl.t -> 'c list
(** Map on a hashtable's items, collect into a list *)

val incr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
(** [incr ?by tbl x] increments or initializes the counter associated with [x].
    If [get tbl x = None], then after update, [get tbl x = Some 1];
    otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
    @param by if specified, the int value is incremented by [by] rather than 1
    @since 0.16 *)

val decr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
(** Same as {!incr} but substract 1 (or the value of [by]).
    If the value reaches 0, the key is removed from the table.
    This does nothing if the key is not already present in the table.
    @since 0.16 *)

val to_seq : ('a,'b) Hashtbl.t -> ('a * 'b) sequence
(** Iterate on bindings in the table *)

val add_list : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
(** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
    not bound, it becomes bound to [[y]].
    @since 0.16 *)

val add_seq : ('a,'b) Hashtbl.t -> ('a * 'b) sequence -> unit
(** Add the corresponding pairs to the table, using {!Hashtbl.add}.
    @since 0.16 *)

val of_seq : ('a * 'b) sequence -> ('a,'b) Hashtbl.t
(** From the given bindings, added in order *)

val add_seq_count : ('a, int) Hashtbl.t -> 'a sequence -> unit
(** [add_seq_count tbl seq] increments the count of each element of [seq]
    by calling {!incr}. This is useful for counting how many times each
    element of [seq] occurs.
    @since 0.16 *)

val of_seq_count : 'a sequence -> ('a, int) Hashtbl.t
(** Similar to {!add_seq_count}, but allocates a new table and returns it
    @since 0.16 *)

val to_list : ('a,'b) Hashtbl.t -> ('a * 'b) list
(** List of bindings (order unspecified)  *)

val of_list : ('a * 'b) list -> ('a,'b) Hashtbl.t
(** From the given list of bindings, added in order *)

val update : ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
(** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
    [k] was mapped to [v], or [f k None] otherwise; if the call
    returns [None] then [k] is removed/stays removed, if the call
    returns [Some v'] then the binding [k -> v'] is inserted
    using {!Hashtbl.replace}
    @since 0.14 *)

val print : 'a printer -> 'b printer -> ('a, 'b) Hashtbl.t printer
(** Printer for table
    @since 0.13 *)

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** Safe version of {!Hashtbl.find} *)

  val get_or : 'a t -> key -> or_:'a -> 'a
  (** [get_or tbl k ~or_] returns the value associated to [k] if present,
      and returns [or_] otherwise (if [k] doesn't belong in [tbl])
      @since 0.16 *)

  val add_list : 'a list t -> key -> 'a -> unit
  (** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
      not bound, it becomes bound to [[y]].
      @since 0.16 *)

  val incr : ?by:int -> int t -> key -> unit
  (** [incr ?by tbl x] increments or initializes the counter associated with [x].
      If [get tbl x = None], then after update, [get tbl x = Some 1];
      otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
      @param by if specified, the int value is incremented by [by] rather than 1
      @since 0.16 *)

  val decr : ?by:int -> int t -> key -> unit
  (** Same as {!incr} but substract 1 (or the value of [by]).
      If the value reaches 0, the key is removed from the table.
      This does nothing if the key is not already present in the table.
      @since 0.16 *)

  val keys : 'a t -> key sequence
  (** Iterate on keys (similar order as {!Hashtbl.iter}) *)

  val values : 'a t -> 'a sequence
  (** Iterate on values in the table *)

  val keys_list : _ t -> key list
  (** [keys t] is the list of keys in [t].
      @since 0.8 *)

  val values_list : 'a t -> 'a list
  (** [values t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Iterate on values in the table *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** From the given bindings, added in order *)

  val add_seq : 'a t -> (key * 'a) sequence -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 0.16 *)

  val add_seq_count : int t -> key sequence -> unit
  (** [add_seq_count tbl seq] increments the count of each element of [seq]
      by calling {!incr}. This is useful for counting how many times each
      element of [seq] occurs.
      @since 0.16 *)

  val of_seq_count : key sequence -> int t
  (** Similar to {!add_seq_count}, but allocates a new table and returns it
      @since 0.16 *)

  val to_list : 'a t -> (key * 'a) list
  (** List of bindings (order unspecified)  *)

  val of_list : (key * 'a) list -> 'a t
  (** From the given list of bindings, added in order *)

  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}
      @since 0.14 *)

  val print : key printer -> 'a printer -> 'a t printer
  (** Printer for tables
      @since 0.13 *)
end

module Make(X : Hashtbl.HashedType) :
  S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t

(** {2 Default Table}

    A table with a default element for keys that were never added.

    @deprecated since 0.16, should be merged into [Make] itself *)

module type DEFAULT = sig
  type key

  type 'a t
  (** A hashtable for keys of type [key] and values of type ['a] *)

  val create : ?size:int -> 'a -> 'a t
  (** [create d] makes a new table that maps every key to [d] by default.
      @param size optional size of the initial table *)

  val create_with : ?size:int -> (key -> 'a) -> 'a t
  (** Similar to [create d] but here [d] is a function called to obtain a
      new default value for each distinct key. Useful if the default
      value is stateful. *)

  val get : 'a t -> key -> 'a
  (** Unfailing retrieval (possibly returns the default value). This will
      modify the table if the key wasn't present. *)

  val set : 'a t -> key -> 'a -> unit
  (** Replace the current binding for this key *)

  val remove : 'a t -> key -> unit
  (** Remove the binding for this key. If [get tbl k] is called later, the
      default value for the table will be returned *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Pairs of [(elem, value)] for all elements on which [get] was called *)
end

module MakeDefault(X : Hashtbl.HashedType) : DEFAULT with type key = X.t

(** {2 Count occurrences using a Hashtbl}

    @deprecated since 0.16, should be merged into [Make] itself *)

module type COUNTER = sig
  type elt
  (** Elements that are to be counted *)

  type t

  val create : int -> t
  (** A counter maps elements to natural numbers (the number of times this
      element occurred) *)

  val incr : t -> elt -> unit
  (** Increment the counter for the given element *)

  val incr_by : t -> int -> elt -> unit
  (** Add or remove several occurrences at once. [incr_by c x n]
      will add [n] occurrences of [x] if [n>0],
      and remove [abs n] occurrences if [n<0]. *)

  val get : t -> elt -> int
  (** Number of occurrences for this element *)

  val decr : t -> elt -> unit
  (** Remove one occurrence of the element
      @since 0.14 *)

  val length : t -> int
  (** Number of distinct elements
      @since 0.14 *)

  val add_seq : t -> elt sequence -> unit
  (** Increment each element of the sequence *)

  val of_seq : elt sequence -> t
  (** [of_seq s] is the same as [add_seq (create ())] *)

  val to_seq : t -> (elt * int) sequence
  (** [to_seq tbl] returns elements of [tbl] along with their multiplicity
      @since 0.14 *)

  val add_list : t -> (elt * int) list -> unit
  (** Similar to {!add_seq}
      @since 0.14 *)

  val of_list : (elt * int) list -> t
  (** Similar to {!of_seq}
      @since 0.14 *)

  val to_list : t -> (elt * int) list
  (** @since 0.14 *)
end

module MakeCounter(X : Hashtbl.HashedType)
  : COUNTER
  with type elt = X.t
  and type t = int Hashtbl.Make(X).t
(** Create a new counter type
    The type [t] is exposed
    @since 0.14 *)
