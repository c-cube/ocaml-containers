(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Handling S-expressions}

    @since 3.0 moved into containers-core, previously in [containers.sexp]
*)

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

(** {2 Abstract representation of S-expressions}

    @since 2.7 *)
module type SEXP = CCSexp_intf.SEXP

(** {2 Operations over S-expressions}

    @since 2.7 *)
module type S = CCSexp_intf.S

(** {2 Functorized operations}

    This builds a parser and printer for S-expressions represented as
    in the [Sexp] argument.

    @since 2.7 *)
module Make(Sexp : SEXP) : S with type t = Sexp.t

(** {2 Basics} *)

(** A simple, structural representation of S-expressions. *)
type t = [
  | `Atom of string
  | `List of t list
]

include S with type t := t

val equal : t -> t -> bool
(** @since 3.0 *)

val compare : t -> t -> int
(** @since 3.0 *)

val atom : string -> t
(** Build an atom directly from a string. *)
