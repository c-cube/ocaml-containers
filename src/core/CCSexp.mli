(* This file is free software, part of containers. See file "license" for more details. *)

(** Handling S-expressions

    @since 3.0 moved into containers-core, previously in [containers.sexp]
*)

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

module type SEXP = CCSexp_intf.SEXP
(** {2 Abstract representation of S-expressions (extended)}

    @since 2.7 *)

module type S = CCSexp_intf.S
(** {2 Operations over S-expressions}

    @since 2.7 *)

(** {2 Functorized operations}

    This builds a parser and printer for S-expressions represented as
    in the [Sexp] argument.

    @since 2.7

    @since 3.4 re-bind [loc] to [Sexp.loc]
*)
module Make (Sexp : SEXP) : S with type t = Sexp.t and type loc = Sexp.loc

(** {2 Basics} *)

type t =
  [ `Atom of string
  | `List of t list
  ]
(** A simple, structural representation of S-expressions. *)

include S with type t := t

val equal : t -> t -> bool
(** @since 3.0 *)

val compare : t -> t -> int
(** @since 3.0 *)

val atom : string -> t
(** Build an atom directly from a string. *)
