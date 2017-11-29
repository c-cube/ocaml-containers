
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Wrapper around Set}

    @since 0.9 *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type S = sig
  include Set.S

  val min_elt_opt : t -> elt option
  (** Safe version of {!min_elt}
      @since NEXT_RELEASE *)

  val max_elt_opt : t -> elt option
  (** Safe version of {!max_elt}
      @since NEXT_RELEASE *)

  val choose_opt : t -> elt option
  (** Safe version of {!choose}
      @since NEXT_RELEASE *)

  val find_opt : elt -> t -> elt option
  (** Safe version of {!find}
      @since NEXT_RELEASE *)

  val find_first : (elt -> bool) -> t -> elt
  (** Find minimum element satisfying predicate
      @since NEXT_RELEASE *)

  val find_first_opt : (elt -> bool) -> t -> elt option
  (** Safe version of {!find_first}
      @since NEXT_RELEASE *)

  val find_last : (elt -> bool) -> t -> elt
  (** Find maximum element satisfying predicate
      @since NEXT_RELEASE *)

  val find_last_opt : (elt -> bool) -> t -> elt option
  (** Safe version of {!find_last}
      @since NEXT_RELEASE *)

  val of_seq : elt sequence -> t

  val add_seq : t -> elt sequence -> t
  (** @since 0.14 *)

  val to_seq : t -> elt sequence

  val of_list : elt list -> t
  (** Build a set from the given list of elements,
      added in order using {!add}. *)

  val add_list : t -> elt list -> t
  (** @since 0.14 *)

  val to_list : t -> elt list

  val pp :
    ?start:string -> ?stop:string -> ?sep:string ->
    elt printer -> t printer
end

module Make(O : Set.OrderedType) : S
  with type t = Set.Make(O).t
   and type elt = O.t
