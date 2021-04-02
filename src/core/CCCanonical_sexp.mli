
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Canonical S-expressions}

    See {{: https://en.wikipedia.org/wiki/Canonical_S-expressions} wikipedia}.
    These S-expressions are binary safe.

    @since 3.3
*)

type 'a or_error = ('a, string) result
type 'a gen = unit -> 'a option

module type SEXP = CCSexp_intf.BASIC_SEXP
module type S = CCSexp_intf.S0

(** {2 Parser and printer} *)
module Make(Sexp : SEXP) : S with type t = Sexp.t

(** {2 Basics} *)

(** A simple, structural representation of S-expressions.
    Compatible with {!CCSexp}. *)
type t = [
  | `Atom of string
  | `List of t list
]

include S with type t := t

val equal : t -> t -> bool

val compare : t -> t -> int

val atom : string -> t
