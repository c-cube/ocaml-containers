
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map}

    Provide useful functions and iterators on [Map.S]
    @since 0.5 *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Map.OrderedType
(** @since 1.5 *)

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find}. *)

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
  (** Safe version of {!choose}.
      @since 1.5 *)

  val min_binding_opt : 'a t -> (key * 'a) option
  (** Safe version of {!min_binding}.
      @since 1.5 *)

  val max_binding_opt : 'a t -> (key * 'a) option
  (** Safe version of {!max_binding}.
      @since 1.5 *)

  val find_opt : key -> 'a t -> 'a option
  (** Safe version of {!find}.
      @since 1.5 *)

  val find_first : (key -> bool) -> 'a t -> key * 'a
  (** Find smallest binding satisfying the monotonic predicate.
      See {!Map.S.find_first}.
      @since 1.5 *)

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  (** Safe version of {!find_first}.
      @since 1.5 *)

  val merge_safe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [merge_safe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  (** Union of both maps, using the function to combine bindings
      that belong to both inputs.
      @since 1.4 *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** Like {!of_list}. *)

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t
  (** @since 0.14 *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_list : (key * 'a) list -> 'a t
  (** Build a map from the given list of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, only its last binding
      will be present in the result. *)

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

  val keys : _ t -> key sequence
  (** Iterate on keys only.
      @since 0.15 *)

  val values : 'a t -> 'a sequence
  (** Iterate on values only.
      @since 0.15 *)

  val to_list : 'a t -> (key * 'a) list

  val pp :
    ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
    key printer -> 'a printer -> 'a t printer
end

module Make(O : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(O).t
   and type key = O.t
