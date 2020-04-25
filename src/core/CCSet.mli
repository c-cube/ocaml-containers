
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Wrapper around Set}

    @since 0.9 *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Set.OrderedType
(** @since 1.5 *)

module type S = sig
  include Set.S

  val min_elt_opt : t -> elt option
  (** Safe version of {!min_elt}.
      @since 1.5 *)

  val max_elt_opt : t -> elt option
  (** Safe version of {!max_elt}.
      @since 1.5 *)

  val choose_opt : t -> elt option
  (** Safe version of {!choose}.
      @since 1.5 *)

  val find_opt : elt -> t -> elt option
  (** Safe version of {!find}.
      @since 1.5 *)

  val find_first : (elt -> bool) -> t -> elt
  (** Find minimum element satisfying predicate.
      @since 1.5 *)

  val find_first_opt : (elt -> bool) -> t -> elt option
  (** Safe version of {!find_first}.
      @since 1.5 *)

  val find_last : (elt -> bool) -> t -> elt
  (** Find maximum element satisfying predicate.
      @since 1.5 *)

  val find_last_opt : (elt -> bool) -> t -> elt option
  (** Safe version of {!find_last}.
      @since 1.5 *)

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
    ?start:string -> ?stop:string -> ?sep:string ->
    (elt -> string) -> t -> string
  (**  Print the set in a string
       @since 2.7 *)

  val pp :
    ?start:string -> ?stop:string -> ?sep:string ->
    elt printer -> t printer
    (** Print the set. *)
end

module Make(O : Set.OrderedType) : S
  with type t = Set.Make(O).t
   and type elt = O.t
