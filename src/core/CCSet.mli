(* This file is free software, part of containers. See file "license" for more details. *)

(** Wrapper around Set

    @since 0.9 *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Set.OrderedType
(** @since 1.5 *)

module type S = sig
  include Set.S

  val find_first_map : (elt -> 'a option) -> t -> 'a option
  (** [find_first_map f s] find the minimum element [x] of [s] such that [f x = Some y]
      and return [Some y]. Otherwise returns [None].
      @since 3.12 *)

  val find_last_map : (elt -> 'a option) -> t -> 'a option
  (** [find_last_map f s] find the maximum element [x] of [s] such that [f x = Some y]
      and return [Some y]. Otherwise returns [None].
      @since 3.12 *)

  val of_iter : elt iter -> t
  (** Build a set from the given [iter] of elements.
      @since 2.8 *)

  val add_iter : t -> elt iter -> t
  (** @since 2.8 *)

  val to_iter : t -> elt iter
  (** [to_iter t] converts the set [t] to a [iter] of the elements.
      @since 2.8 *)

  val add_list : t -> elt list -> t
  (** @since 0.14 *)

  val to_list : t -> elt list
  (** [to_list t] converts the set [t] to a list of the elements. *)

  val to_string :
    ?start:string ->
    ?stop:string ->
    ?sep:string ->
    (elt -> string) ->
    t ->
    string
  (**  Print the set in a string
       @since 2.7 *)

  val pp :
    ?pp_start:unit printer ->
    ?pp_stop:unit printer ->
    ?pp_sep:unit printer ->
    elt printer ->
    t printer
  (** Print the set. *)
end

module Make (O : Set.OrderedType) :
  S with type t = Set.Make(O).t and type elt = O.t
