
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Random-Access Lists}

    This is an OCaml implementation of Okasaki's paper
    "Purely Functional Random Access Lists". It defines a list-like data
    structure with O(1) cons/tail operations, and O(log(n)) lookup/modification
    operations.

    This module used to be part of [containers.misc]

    {b status: stable}

    @since NEXT_RELEASE
*)

type +'a t
(** List containing elements of type 'a *)

val empty : 'a t
(** Empty list *)

val is_empty : _ t -> bool
(** Check whether the list is empty *)

val cons : 'a -> 'a t -> 'a t
(** Add an element at the front of the list *)

val return : 'a -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map on elements *)

val hd : 'a t -> 'a
(** First element of the list, or
    @raise Invalid_argument if the list is empty *)

val tl : 'a t -> 'a t
(** Remove the first element from the list, or
    @raise Invalid_argument if the list is empty *)

val front : 'a t -> ('a * 'a t) option
(** Remove and return the first element of the list *)

val front_exn : 'a t -> 'a * 'a t
(** Unsafe version of {!front}.
    @raise Invalid_argument if the list is empty *)

val length : 'a t -> int
(** Number of elements *)

val get : 'a t -> int -> 'a
(** [get l i] accesses the [i]-th element of the list. O(log(n)).
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val set : 'a t -> int -> 'a -> 'a t
(** [set l i v] sets the [i]-th element of the list to [v]. O(log(n)).
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val remove : 'a t -> int -> 'a t
(** [remove l i] removes the [i]-th element of [v].
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val append : 'a t -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val flatten : 'a t t -> 'a t

val app : ('a -> 'b) t -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on the list's elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on the list's elements *)

val fold_rev : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on the list's elements, in reverse order (starting from the tail) *)

val rev : 'a t -> 'a t
(** Reverse the list *)

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val add_list : 'a t -> 'a list -> 'a t

val of_list : 'a list -> 'a t
(** Convert a list to a RAL. {b Caution}: non tail-rec *)

val to_list : 'a t -> 'a list

val of_list_map : ('a -> 'b) -> 'a list -> 'b t
(** Combination of {!of_list} and {!map} *)

val add_seq : 'a t -> 'a sequence -> 'a t

val of_seq : 'a sequence -> 'a t

val to_seq : 'a t -> 'a sequence

val add_gen : 'a t -> 'a gen -> 'a t

val of_gen : 'a gen -> 'a t

val to_gen : 'a t -> 'a gen

(** {2 Infix} *)

module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

include module type of Infix

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val print : ?sep:string -> 'a printer -> 'a t printer


