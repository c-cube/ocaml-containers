(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Random-Access Lists}

    This is an OCaml implementation of Okasaki's paper
    "Purely Functional Random Access Lists". It defines a list-like data
    structure with O(1) cons/tail operations, and O(log(n)) lookup/modification
    operations.

    This module used to be part of [containers.misc]

    {b status: stable}

    @since 0.13
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
(** Singleton *)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Map on elements *)

val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** Map with index *)

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
(** Number of elements. Complexity O(ln n) where n=number of elements *)

val get : 'a t -> int -> 'a option
(** [get l i] accesses the [i]-th element of the list. O(log(n)). *)

val get_exn : 'a t -> int -> 'a
(** Unsafe version of {!get}
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val set : 'a t -> int -> 'a -> 'a t
(** [set l i v] sets the [i]-th element of the list to [v]. O(log(n)).
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val remove : 'a t -> int -> 'a t
(** [remove l i] removes the [i]-th element of [v].
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val append : 'a t -> 'a t -> 'a t

val filter : f:('a -> bool) -> 'a t -> 'a t

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val flatten : 'a t t -> 'a t

val app : ('a -> 'b) t -> 'a t -> 'b t

val take : int -> 'a t -> 'a t

val take_while : f:('a -> bool) -> 'a t -> 'a t

val drop : int -> 'a t -> 'a t

val drop_while : f:('a -> bool) -> 'a t -> 'a t

val take_drop : int -> 'a t -> 'a t * 'a t
(** [take_drop n l] splits [l] into [a, b] such that [length a = n]
    if [length l >= n], and such that [append a b = l] *)

val iter : f:('a -> unit) -> 'a t -> unit
(** Iterate on the list's elements *)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

val fold : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b
(** Fold on the list's elements *)

val fold_rev : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b
(** Fold on the list's elements, in reverse order (starting from the tail) *)

val rev_map : f:('a -> 'b) -> 'a t -> 'b t
(** [rev_map f l] is the same as [map f (rev l)] *)

val rev : 'a t -> 'a t
(** Reverse the list *)

val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Lexicographic comparison *)

(** {2 Utils} *)

val make : int -> 'a -> 'a t

val repeat : int -> 'a t -> 'a t
(** [repeat n l] is [append l (append l ... l)] [n] times *)

val range : int -> int -> int t
(** [range i j] is [i; i+1; ... ; j] or [j; j-1; ...; i] *)

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val add_list : 'a t -> 'a list -> 'a t

val of_list : 'a list -> 'a t
(** Convert a list to a RAL. {b Caution}: non tail-rec *)

val to_list : 'a t -> 'a list

val of_list_map : f:('a -> 'b) -> 'a list -> 'b t
(** Combination of {!of_list} and {!map} *)

val of_array : 'a array -> 'a t

val add_array : 'a t -> 'a array -> 'a t

val to_array : 'a t -> 'a array
(** More efficient than on usual lists *)

val add_seq : 'a t -> 'a sequence -> 'a t

val of_seq : 'a sequence -> 'a t

val to_seq : 'a t -> 'a sequence

val add_gen : 'a t -> 'a gen -> 'a t

val of_gen : 'a gen -> 'a t

val to_gen : 'a t -> 'a gen

(** {2 Infix} *)

module Infix : sig
  val (@+) : 'a -> 'a t -> 'a t
  (** Cons (alias to {!cons}) *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Alias to {!flat_map} *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Alias to {!map} *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Alias to {!app} *)

  val (--) : int -> int -> int t
  (** Alias to {!range} *)

  val (--^) : int -> int -> int t
  (** [a -- b] is the integer range from [a] to [b], where [b] is excluded.
      @since 0.17 *)
end

include module type of Infix

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val print : ?sep:string -> 'a printer -> 'a t printer
