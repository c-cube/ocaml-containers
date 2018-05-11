
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type +'a t = 'a option

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform the element inside, if any. *)

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
(** Compare two options, using custom comparators for the value.
    [None] is always assumed to be less than [Some _]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test for equality between option types using a custom equality predicat. *)

val return : 'a -> 'a t
(** Monadic return, that is [return x = Some x]. *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of {!map}. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Flip version of {!>>=}. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f o1 o2] maps ['a option] and ['b option] to a ['c option] using [f]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on 0 or 1 element. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on 0 or 1 element. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter on 0 or 1 element.
    @since 0.5 *)

val if_ : ('a -> bool) -> 'a -> 'a option
(** [if_ f x] is [Some x] if [f x], [None] otherwise.
    @since 0.17 *)

val exists : ('a -> bool) -> 'a t -> bool
(** Return [true] iff there exists an element for which the provided function evaluates to [true].
    @since 0.17 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Return [true] iff the provided function evaluates to [true] for all elements.
    @since 0.17 *)

val get_or : default:'a -> 'a t -> 'a
(** [get_or ~default o] extracts the value from [o], or
    returns [default] if [o = None].
    @since 0.18 *)

val get_exn : 'a t -> 'a
(** Open the option, possibly failing if it is [None].
    @raise Invalid_argument if the option is [None]. *)

val get_lazy : (unit -> 'a) -> 'a t -> 'a
(** [get_lazy default_fn x] unwraps [x], but if [x = None] it returns [default_fn ()] instead.
    @since 0.6.1 *)

val sequence_l : 'a t list -> 'a list t
(** [sequence_l [x1; x2; ...; xn]] returns [Some [y1;y2;...;yn]] if
    every [xi] is [Some yi]. Otherwise, if the list contains at least
    one [None], the result is [None]. *)

val wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
(** [wrap f x] calls [f x] and returns [Some y] if [f x = y]. If [f x] raises
    any exception, the result is [None]. This can be useful to wrap functions
    such as [Map.S.find].
    @param handler the exception handler, which returns [true] if the
        exception is to be caught. *)

val wrap2 : ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option
(** [wrap2 f x y] is similar to {!wrap} but for binary functions. *)

(** {2 Applicative} *)

val pure : 'a -> 'a t
(** Alias to {!return}. *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [f <*> (Some x)] returns [Some (f x)] and [f <*> None] returns [None]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** Like [map].  *)

(** {2 Alternatives} *)

val or_ : else_:('a t) -> 'a t -> 'a t
(** [or_ ~else_ a] is [a] if [a] is [Some _], [else_] otherwise.
    @since 1.2 *)

val or_lazy : else_:(unit -> 'a t) -> 'a t -> 'a t
(** [or_lazy ~else_ a] is [a] if [a] is [Some _], [else_ ()] otherwise.
    @since 1.2 *)

val (<+>) : 'a t -> 'a t -> 'a t
(** [a <+> b] is [a] if [a] is [Some _], [b] otherwise. *)

val choice : 'a t list -> 'a t
(** [choice] returns the first non-[None] element of the list, or [None]. *)

val flatten : 'a t t -> 'a t
(** [flatten] transforms [Some x] into [x].
    @since 2.2 *)

val return_if : bool -> 'a -> 'a t
(** Apply [Some] or [None] depending on a boolean.
    More precisely, [return_if false x] is [None],
    and [return_if true x] is [Some x].
    @since 2.2 *)

(** {2 Infix Operators}
    @since 0.16 *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** [x >|= f] is [map f x]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** [f <*> (Some x)] returns [Some (f x)] and [f <*> None] returns [None]. *)

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** Like [map].  *)

  val (<+>) : 'a t -> 'a t -> 'a t
  (** [a <+> b] is [a] if [a] is [Some _], [b] otherwise. *)

end

(** {2 Conversion and IO} *)

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t
(** Head of list, or [None]. *)

val to_result : 'e -> 'a t -> ('a, 'e) Result.result
(** @since 1.2 *)

val to_result_lazy : (unit -> 'e) -> 'a t -> ('a, 'e) Result.result
(** @since 1.2 *)

val of_result : ('a, _) Result.result -> 'a t
(** @since 1.2 *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val random : 'a random_gen -> 'a t random_gen

val choice_seq : 'a t sequence -> 'a t
(** [choice_seq s] is similar to {!choice}, but works on sequences.
    It returns the first [Some x] occurring in [s], or [None] otherwise.
    @since 0.13 *)

val to_gen : 'a t -> 'a gen
val to_seq : 'a t -> 'a sequence

val pp : 'a printer -> 'a t printer
