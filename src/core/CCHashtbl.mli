
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extension to the standard Hashtbl}

    @since 0.4 *)

type 'a sequence = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

(** This sub-module contains the extension of the standard polymorphic hashtbl. *)

module Poly : sig
  val get : ('a,'b) Hashtbl.t -> 'a -> 'b option
  (** Safe version of {!Hashtbl.find} *)

  val get_or : ('a,'b) Hashtbl.t -> 'a -> default:'b -> 'b
  (** [get_or tbl k ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [tbl])
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
  (** Build a table from the given list of bindings [k_i -> v_i],
      added in order using {!add}. If a key occurs several times,
      it will be added several times, and the visible binding
      will be the last one. *)

  val update : ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}
      @since 0.14 *)

  val get_or_add : ('a, 'b) Hashtbl.t -> f:('a -> 'b) -> k:'a -> 'b
  (** [get_or_add tbl ~k ~f] finds and returns the binding of [k]
      in [tbl], if it exists. If it does not exist, then [f k]
      is called to obtain a new binding [v]; [k -> v] is added
      to [tbl] and [v] is returned.
      @since 1.0 *)

  val print : 'a printer -> 'b printer -> ('a, 'b) Hashtbl.t printer
  (** Printer for table
      @since 0.13 *)
end

include module type of Poly

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** Safe version of {!Hashtbl.find} *)

  val get_or : 'a t -> key -> default:'a -> 'a
  (** [get_or tbl k ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [tbl])
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
  (** Build a table from the given list of bindings [k_i -> v_i],
      added in order using {!add}. If a key occurs several times,
      it will be added several times, and the visible binding
      will be the last one. *)

  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}
      @since 0.14 *)

  val get_or_add : 'a t -> f:(key -> 'a) -> k:key -> 'a
  (** [get_or_add tbl ~k ~f] finds and returns the binding of [k]
      in [tbl], if it exists. If it does not exist, then [f k]
      is called to obtain a new binding [v]; [k -> v] is added
      to [tbl] and [v] is returned.
      @since 1.0 *)

  val print : key printer -> 'a printer -> 'a t printer
  (** Printer for tables
      @since 0.13 *)
end

module Make(X : Hashtbl.HashedType) :
  S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t

