
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map}

    Provide useful functions and iterators on [Map.S]
    @since 0.5 *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find} *)

  val get_or : key -> 'a t -> or_:'a -> 'a
  (** [get_or k m ~or_] returns the value associated to [k] if present,
      and returns [or_] otherwise (if [k] doesn't belong in [m])
      @since 0.16 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val merge_safe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [merge_safe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val of_seq : (key * 'a) sequence -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t
  (** @since 0.14 *)

  val to_seq : 'a t -> (key * 'a) sequence

  val of_list : (key * 'a) list -> 'a t

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

  val keys : _ t -> key sequence
  (** Iterate on keys only
      @since 0.15 *)

  val values : 'a t -> 'a sequence
  (** Iterate on values only
      @since 0.15 *)

  val to_list : 'a t -> (key * 'a) list

  val pp :
    ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
    key printer -> 'a printer -> 'a t printer

  val print :
    ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
    key formatter -> 'a formatter -> 'a t formatter
end

module Make(O : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(O).t
   and type key = O.t
