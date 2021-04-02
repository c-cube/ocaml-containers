
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map}

    Provide useful functions and iterators on [Map.S]
    @since 0.5 *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Map.OrderedType
(** @since 1.5 *)

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** [get k m] returns [Some v] if the current binding of [k] in [m] is [v],
      or [None] if the key [k] is not present.
      Safe version of {!find}. *)

  val get_or : key -> 'a t -> default:'a -> 'a
  (** [get_or k m ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [m]).
      @since 0.16 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val choose_opt : 'a t -> (key * 'a) option
  (** [choose_opt m] returns one binding of the given map [m], or [None] if [m] is empty.
      Safe version of {!choose}.
      @since 1.5 *)

  val min_binding_opt : 'a t -> (key * 'a) option
  (** [min_binding_opt m] returns the smallest binding of the given map [m],
      or [None] if [m] is empty.
      Safe version of {!min_binding}.
      @since 1.5 *)

  val max_binding_opt : 'a t -> (key * 'a) option
  (** [max_binding_opt m] returns the largest binding of the given map [m],
      or [None] if [m] is empty.
      Safe version of {!max_binding}.
      @since 1.5 *)

  val find_opt : key -> 'a t -> 'a option
  (** [find_opt k m] returns [Some v] if the current binding of [k] in [m] is [v],
      or [None] if the key [k] is not present.
      Safe version of {!find}.
      @since 1.5 *)

  val find_first : (key -> bool) -> 'a t -> key * 'a
  (** [find_first f m] where [f] is a monotonically increasing function, returns the binding of [m]
      with the lowest key [k] such that [f k], or raises [Not_found] if no such key exists.
      See {!Map.S.find_first}.
      @since 1.5 *)

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  (** [find_first_opt f m] where [f] is a monotonically increasing function, returns an option containing
      the binding of [m] with the lowest key [k] such that [f k], or [None] if no such key exists.
      Safe version of {!find_first}.
      @since 1.5 *)

  val merge_safe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [merge_safe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val add_seq : 'a t -> (key * 'a) Seq.t -> 'a t
  (** [add_seq m seq] adds the given [Seq.t] of bindings to the map [m].
      Like {!add_list}.
      Renamed from [add_std_seq] since 3.0.
      @since 3.0 *)

  val add_seq_with : f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) Seq.t -> 'a t
  (** [add_seq ~f m l] adds the given seq [l] of bindings to the map [m],
      using [f] to combine values that have the same key.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the seq than [v2].
      @since 3.3 *)

  val of_seq : (key * 'a) Seq.t -> 'a t
  (** [of_seq seq] builds a map from the given [Seq.t] of bindings.
      Like {!of_list}.
      Renamed from [of_std_seq] since 3.0.
      @since 3.0 *)

  val of_seq_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) Seq.t -> 'a t
  (** [of_seq_with ~f l] builds a map from the given seq [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the seq than [v2].
      @since 3.3 *)

  val add_iter : 'a t -> (key * 'a) iter -> 'a t
  (** [add_iter m iter] adds the given [iter] of bindings to the map [m].
      Like {!add_list}.
      @since 2.8 *)

  val add_iter_with : f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) iter -> 'a t
  (** [add_iter ~f m l] adds the given iter [l] of bindings to the map [m],
      using [f] to combine values that have the same key.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the seq than [v2].
      @since 3.3 *)

  val of_iter : (key * 'a) iter -> 'a t
  (** [of_iter iter] builds a map from the given [iter] of bindings.
      Like {!of_list}.
      @since 2.8 *)

  val of_iter_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) iter -> 'a t
  (** [of_iter_with ~f l] builds a map from the given iter [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the iter than [v2].
      @since 3.3 *)

  val to_iter : 'a t -> (key * 'a) iter
  (** [to_iter m] iterates on the whole map [m], creating an [iter] of bindings.
      Like {!to_list}.
      @since 2.8 *)

  val of_list : (key * 'a) list -> 'a t
  (** [of_list l] builds a map from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, only its last binding
      will be present in the result. *)

  val of_list_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) list -> 'a t
  (** [of_list_with ~f l] builds a map from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the list than [v2].
      @since 3.3 *)

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** [add_list m l] adds the given list [l] of bindings to the map [m].
      @since 0.14 *)

  val add_list_with : f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) list -> 'a t
  (** [add_list ~f m l] adds the given list [l] of bindings to the map [m],
      using [f] to combine values that have the same key.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the seq than [v2].
      @since 3.3 *)

  val keys : _ t -> key iter
  (** [keys m] iterates on the keys of [m] only, creating an [iter] of keys.
      @since 0.15 *)

  val values : 'a t -> 'a iter
  (** [values m] iterates on the values of [m] only, creating an [iter] of values.
      @since 0.15 *)

  val to_list : 'a t -> (key * 'a) list
  (** [to_list m] builds a list of the bindings of the given map [m].
      The order is unspecified. *)

  val pp : ?pp_start:unit printer -> ?pp_stop:unit printer -> ?pp_arrow:unit printer ->
    ?pp_sep:unit printer -> key printer -> 'a printer -> 'a t printer
  (** [pp ?pp_start ?pp_stop ?pp_arrow ?pp_sep pp_key pp_v m] pretty-prints the
      contents of the map. *)
end

module Make(O : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(O).t
   and type key = O.t
