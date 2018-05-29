
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Bool functions} *)

type t = bool

val compare : t -> t -> int
(** Total ordering on booleans, similar to {!Pervasives.compare}. *)

val leq : t -> t -> bool
(** Equivalent of [(<=)]
    Allows to be passed to {!Heap.Make} *)

val equal : t -> t -> bool

val negate : t -> t
(** Negation on booleans (functional version of [not]).
    @deprecated since 1.3, simply use {!not} instead. *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : t printer
