
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extension to the standard Hashtbl}

    @since 0.4 *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

(** This sub-module contains the extension of the standard polymorphic Hashtbl. *)

module Poly : sig
  val get : ('a,'b) Hashtbl.t -> 'a -> 'b option
  (** [get tbl k] finds a binding for the key [k] if present,
      or returns [None] if no value is found.
      Safe version of {!Hashtbl.find}. *)

  val get_or : ('a,'b) Hashtbl.t -> 'a -> default:'b -> 'b
  (** [get_or tbl k ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [tbl]).
      @since 0.16 *)

  val keys : ('a,'b) Hashtbl.t -> 'a iter
  (** [keys tbl f] iterates on keys (similar order as {!Hashtbl.iter}). *)

  val values : ('a,'b) Hashtbl.t -> 'b iter
  (** [values tbl f] iterates on values in the table [tbl]. *)

  val keys_list : ('a, 'b) Hashtbl.t -> 'a list
  (** [keys_list tbl] is the list of keys in [tbl].
      If the key is in the Hashtable multiple times, all occurrences will be returned.
      @since 0.8 *)

  val values_list : ('a, 'b) Hashtbl.t -> 'b list
  (** [values_list tbl] is the list of values in [tbl].
      @since 0.8 *)

  val map_list : ('a -> 'b -> 'c) -> ('a, 'b) Hashtbl.t -> 'c list
  (** [map_list f tbl] maps on a [tbl]'s items. Collect into a list. *)

  val incr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  (** [incr ?by tbl x] increments or initializes the counter associated with [x].
      If [get tbl x = None], then after update, [get tbl x = Some 1];
      otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
      @param by if specified, the int value is incremented by [by] rather than 1.
      @since 0.16 *)

  val decr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  (** [decr ?by tbl x] is like {!incr} but subtract 1 (or the value of [by]).
      If the value reaches 0, the key is removed from the table.
      This does nothing if the key is not already present in the table.
      @since 0.16 *)

  val to_iter : ('a,'b) Hashtbl.t -> ('a * 'b) iter
  (** Iterate on bindings in the table.
      @since 2.8 *)

  val add_list : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
  (** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
      not bound, it becomes bound to [y].
      @since 0.16 *)

  val add_iter : ('a,'b) Hashtbl.t -> ('a * 'b) iter -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 2.8 *)

  val add_std_seq : ('a,'b) Hashtbl.t -> ('a * 'b) Seq.t -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 2.8 *)

  val of_iter : ('a * 'b) iter -> ('a,'b) Hashtbl.t
  (** From the given bindings, added in order.
      @since 2.8 *)

  val of_std_seq : ('a * 'b) Seq.t -> ('a,'b) Hashtbl.t
  (** From the given bindings, added in order.
      @since 2.8 *)

  val add_iter_count : ('a, int) Hashtbl.t -> 'a iter -> unit
  (** [add_iter_count tbl i] increments the count of each element of [i]
      by calling {!incr}. This is useful for counting how many times each
      element of [i] occurs.
      @since 2.8 *)

  val add_std_seq_count : ('a, int) Hashtbl.t -> 'a Seq.t -> unit
  (** [add_seq_count tbl seq] increments the count of each element of [seq]
      by calling {!incr}. This is useful for counting how many times each
      element of [seq] occurs.
      @since 2.8 *)

  val of_iter_count : 'a iter -> ('a, int) Hashtbl.t
  (** Like {!add_seq_count}, but allocates a new table and returns it.
      @since 2.8 *)

  val of_std_seq_count : 'a Seq.t -> ('a, int) Hashtbl.t
  (** Like {!add_seq_count}, but allocates a new table and returns it.
      @since 2.8 *)

  val to_list : ('a,'b) Hashtbl.t -> ('a * 'b) list
  (** [to_list tbl] returns the list of (key,value) bindings (order unspecified). *)

  val of_list : ('a * 'b) list -> ('a,'b) Hashtbl.t
  (** [of_list l] builds a table from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}. If a key occurs several times,
      it will be added several times, and the visible binding
      will be the last one. *)

  val update : ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}.
      @since 0.14 *)

  val get_or_add : ('a, 'b) Hashtbl.t -> f:('a -> 'b) -> k:'a -> 'b
  (** [get_or_add tbl ~k ~f] finds and returns the binding of [k]
      in [tbl], if it exists. If it does not exist, then [f k]
      is called to obtain a new binding [v]; [k -> v] is added
      to [tbl] and [v] is returned.
      @since 1.0 *)

  val pp : 'a printer -> 'b printer -> ('a, 'b) Hashtbl.t printer
  (** [pp pp_k pp_v] returns a table printer given a [pp_k] printer
      for individual key and a [pp_v] printer for individual value.
      Renamed from [print] since 2.0.
      @since 0.13 *)
end

include module type of Poly

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** [get tbl k] finds a binding for the key [k] if present,
      or returns [None] if no value is found.
      Safe version of {!Hashtbl.find}. *)

  val get_or : 'a t -> key -> default:'a -> 'a
  (** [get_or tbl k ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [tbl]).
      @since 0.16 *)

  val add_list : 'a list t -> key -> 'a -> unit
  (** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
      not bound, it becomes bound to [y].
      @since 0.16 *)

  val incr : ?by:int -> int t -> key -> unit
  (** [incr ?by tbl x] increments or initializes the counter associated with [x].
      If [get tbl x = None], then after update, [get tbl x = Some 1];
      otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
      @param by if specified, the int value is incremented by [by] rather than 1.
      @since 0.16 *)

  val decr : ?by:int -> int t -> key -> unit
  (** [decr ?by tbl x] is like {!incr} but subtract 1 (or the value of [by]).
      If the value reaches 0, the key is removed from the table.
      This does nothing if the key is not already present in the table.
      @since 0.16 *)

  val keys : 'a t -> key iter
  (**  [keys tbl f] iterates on keys (similar order as {!Hashtbl.iter}). *)

  val values : 'a t -> 'a iter
  (**  [values tbl f] iterates on values in the table. *)

  val keys_list : _ t -> key list
  (** [keys_list tbl] is the list of keys in [tbl].
      If the key is in the Hashtable multiple times, all occurrences will be returned.
      @since 0.8 *)

  val values_list : 'a t -> 'a list
  (** [values_list t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list. *)

  val to_iter : 'a t -> (key * 'a) iter
  (** Iterate on bindings in the table.
      @since 2.8 *)

  val add_iter : 'a t -> (key * 'a) iter -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 2.8 *)

  val add_std_seq : 'a t -> (key * 'a) Seq.t -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 2.8 *)

  val of_iter : (key * 'a) iter -> 'a t
  (** From the given bindings, added in order.
      @since 2.8 *)

  val of_std_seq : (key * 'a) Seq.t -> 'a t
  (** From the given bindings, added in order.
      @since 2.8 *)

  val add_iter_count : int t -> key iter -> unit
  (** [add_iter_count tbl i] increments the count of each element of [i]
      by calling {!incr}. This is useful for counting how many times each
      element of [i] occurs.
      @since 2.8 *)

  val add_std_seq_count : int t -> key Seq.t -> unit
  (** [add_seq_count tbl seq] increments the count of each element of [seq]
      by calling {!incr}. This is useful for counting how many times each
      element of [seq] occurs.
      @since 2.8 *)

  val of_iter_count : key iter -> int t
  (** Like {!add_seq_count}, but allocates a new table and returns it.
      @since 2.8 *)

  val of_std_seq_count : key Seq.t -> int t
  (** Like {!add_seq_count}, but allocates a new table and returns it.
      @since 2.8 *)

  val to_list : 'a t -> (key * 'a) list
  (** [to_list tbl] returns the list of (key,value) bindings (order unspecified). *)

  val of_list : (key * 'a) list -> 'a t
  (** [of_list l] builds a table from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}. If a key occurs several times,
      it will be added several times, and the visible binding
      will be the last one. *)

  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}.
      @since 0.14 *)

  val get_or_add : 'a t -> f:(key -> 'a) -> k:key -> 'a
  (** [get_or_add tbl ~k ~f] finds and returns the binding of [k]
      in [tbl], if it exists. If it does not exist, then [f k]
      is called to obtain a new binding [v]; [k -> v] is added
      to [tbl] and [v] is returned.
      @since 1.0 *)

  val pp : key printer -> 'a printer -> 'a t printer
  (** [pp pp_k pp_v] returns a table printer given a [pp_k] printer
      for individual key and a [pp_v] printer for individual value. 
      Renamed from [print] since 2.0.
      @since 0.13 *)
end

module Make(X : Hashtbl.HashedType) :
  S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t

