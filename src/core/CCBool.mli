
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Bool functions} *)

type t = bool

val compare : t -> t -> int
(** Total ordering on booleans, similar to {!Pervasives.compare} *)

val equal : t -> t -> bool

val negate : t -> t
(** Negation on booleans (functional version of [not]) *)

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

val pp : t printer
(** Printer for booleans *)

val print : t formatter
