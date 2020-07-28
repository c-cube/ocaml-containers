(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type +'a t = 'a option

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f o] applies the function [f] to the element inside [o], if any. *)

val map_or : default:'b -> ('a -> 'b) -> 'a t -> 'b
(** [map_or ~default f o] is [f x] if [o = Some x], [default] otherwise.
    @since 0.16 *)

val map_lazy : (unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
(** [map_lazy default_fn f o] if [f o] if [o = Some x], [default_fn ()] otherwise.
    @since 1.2 *)

val is_some : _ t -> bool
(** [is_some (Some x)] returns [true] otherwise it returns [false]. *)

val is_none : _ t -> bool
(** [is_none None] returns [true] otherwise it returns [false].
    @since 0.11 *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare comp o1 o2] compares two options [o1] and [o2],
    using custom comparators [comp] for the value.
    [None] is always assumed to be less than [Some _]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal p o1 o2] tests for equality between option types [o1] and [o2],
    using a custom equality predicate [p]. *)

val return : 'a -> 'a t
(** [return x] is a monadic return, that is [return x = Some x]. *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** [o >|= f] is the infix version of {!map}. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** [flat_map f o] is equivalent to {!map} followed by {!flatten}.
    Flip version of {!>>=}. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind o f] is [f v] if [o] is [Some v], [None] otherwise.
    Monadic bind.
    @since 3.0 *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [o >>= f] is the infix version of {!bind}. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f o1 o2] maps ['a option] and ['b option] to a ['c option] using [f]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f o] applies [f] to [o]. Iterate on 0 or 1 element. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f init o] is [f init x] if [o] is [Some x], or [init] if [o] is [None].
    Fold on 0 or 1 element. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f o] returns [Some x] if [f (Some x)] is [true],
    or [None] if [f (Some x)] is [false] or if [o] is [None].
    Filter on 0 or 1 element.
    @since 0.5 *)

val if_ : ('a -> bool) -> 'a -> 'a option
(** [if_ f x] is [Some x] if [f x], [None] otherwise.
    @since 0.17 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f o] returns [true] iff there exists an element for which
    the provided function [f] evaluates to [true].
    @since 0.17 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all f o] returns [true] iff the provided function [f] evaluates to [true] for all elements.
    @since 0.17 *)

val get_or : default:'a -> 'a t -> 'a
(** [get_or ~default o] extracts the value from [o], or
    returns [default] if [o] is [None].
    @since 0.18 *)

val value : 'a t -> default:'a -> 'a
(** [value o ~default] is similar to the Stdlib's [Option.value] and to {!get_or}.
    @since 2.8 *)

val get_exn : 'a t -> 'a
(** [get_exn o] returns [x] if [o] is [Some x] or fails if [o] is [None].
    @raise Invalid_argument if the option is [None]. *)

val get_lazy : (unit -> 'a) -> 'a t -> 'a
(** [get_lazy default_fn o] unwraps [o], but if [o] is [None] it returns [default_fn ()] instead.
    @since 0.6.1 *)

val sequence_l : 'a t list -> 'a list t
(** [sequence_l [x1; x2; …; xn]] returns [Some [y1; y2; …; yn]] if
    every [xi] is [Some yi]. Otherwise, if the list contains at least
    one [None], the result is [None]. *)

val wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
(** [wrap ?handler f x] calls [f x] and returns [Some y] if [f x = y]. If [f x] raises
    any exception, the result is [None]. This can be useful to wrap functions
    such as [Map.S.find].
    @param handler the exception handler, which returns [true] if the
        exception is to be caught. *)

val wrap2 : ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option
(** [wrap2 ?handler f x y] is similar to {!wrap} but for binary functions. *)

(** {2 Applicative} *)

val pure : 'a -> 'a t
(** [pure x] is an alias to {!return}. *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [f <*> (Some x)] returns [Some (f x)] and [f <*> None] returns [None]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [f <$> o] is like [map f o].  *)

(** {2 Alternatives} *)

val or_ : else_:('a t) -> 'a t -> 'a t
(** [or_ ~else_ o] is [o] if [o] is [Some _], [else_] if [o] is [None].
    @since 1.2 *)

val or_lazy : else_:(unit -> 'a t) -> 'a t -> 'a t
(** [or_lazy ~else_ o] is [o] if [o] is [Some _], [else_ ()] if [o] is [None].
    @since 1.2 *)

val (<+>) : 'a t -> 'a t -> 'a t
(** [o1 <+> o2] is [o1] if [o1] is [Some _], [o2] if [o1] is [None]. *)

val choice : 'a t list -> 'a t
(** [choice lo] returns the first non-[None] element of the list [lo], or [None]. *)

val flatten : 'a t t -> 'a t
(** [flatten oo] transforms [Some x] into [x].
    @since 2.2 *)

val return_if : bool -> 'a -> 'a t
(** [return_if b x] applies [Some] or [None] depending on the boolean [b].
    More precisely, [return_if false x] is [None],
    and [return_if true x] is [Some x].
    @since 2.2 *)

(** {2 Infix Operators}
    @since 0.16 *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** [o >|= f] is [map f o]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [o >>= f] is the monadic bind. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** [f <*> o] returns [Some (f x)] if [o] is [Some x] and [None] if [o] is [None]. *)

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** [f <$> o] is like [map f o].  *)

  val (<+>) : 'a t -> 'a t -> 'a t
  (** [o1 <+> o2] is [o1] if [o1] is [Some _], [o2] if [o1] is [None]. *)

  (** Let operators on OCaml >= 4.08.0, nothing otherwise
      @since 2.8 *)
  include CCShimsMkLet_.S with type 'a t_let := 'a option

end


(** Let operators on OCaml >= 4.08.0, nothing otherwise
    @since 2.8 *)
include CCShimsMkLet_.S with type 'a t_let := 'a option

(** {2 Conversion and IO} *)

val to_list : 'a t -> 'a list
(** [to_list o] returns [[x]] if [o] is [Some x] or the empty list [[]] if [o] is [None]. *)
    
val of_list : 'a list -> 'a t
(** [of_list l] returns [Some x] (x being the head of the list l), or [None] if [l] is the empty list. *)

val to_result : 'e -> 'a t -> ('a, 'e) result
(** [to_result e o] returns [Ok x] if [o] is [Some x], or [Error e] if [o] is [None].
    @since 1.2 *)

val to_result_lazy : (unit -> 'e) -> 'a t -> ('a, 'e) result
(** [to_result_lazy f o] returns [Ok x] if [o] is [Some x] or [Error f] if [o] is [None].
    @since 1.2 *)

val of_result : ('a, _) result -> 'a t
(** [of_result result] returns an option from a [result].
    @since 1.2 *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val random : 'a random_gen -> 'a t random_gen

val choice_iter : 'a t iter -> 'a t
(** [choice_iter iter] is similar to {!choice}, but works on [iter].
    It returns the first [Some x] occurring in [iter], or [None] otherwise.
    @since 3.0 *)

val choice_seq : 'a t Seq.t -> 'a t
(** [choice_seq seq] works on [Seq.t].
    It returns the first [Some x] occurring in [seq], or [None] otherwise.
    @since 3.0 *)

val to_gen : 'a t -> 'a gen
(** [to_gen o] is [o] as a [gen]. [Some x] is the singleton [gen] containing [x]
    and [None] is the empty [gen]. *)
    
val to_seq : 'a t -> 'a Seq.t
(** [to_seq o] is [o] as a sequence [Seq.t]. [Some x] is the singleton sequence containing [x]
    and [None] is the empty sequence.
    Same as {!Stdlib.Option.to_seq}
    Renamed from [to_std_seq] since 3.0.
    @since 3.0 *)

val to_iter : 'a t -> 'a iter
(** [to_iter o] returns an internal iterator, like in the library [Iter].
    @since 2.8 *)

val pp : 'a printer -> 'a t printer
(** [pp ppf o] pretty-prints option [o] using [ppf]. *)
