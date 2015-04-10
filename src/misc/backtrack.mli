
(** {1 Experiment with Backtracking Monad}

Playing stuff, don't use (yet?).

{b status: experimental}
@since 0.10
*)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(** Taken from Coq "logic_monad.mli" *)

module NonLogical : sig
  type 'a t = unit -> 'a
  include MONAD with type 'a t := 'a t
end

(** {6 Logical layer} *)
(** The logical monad is a backtracking monad on top of which is
    layered a state monad (which is used to implement all of read/write,
    read only, and write only effects). The state monad being layered on
    top of the backtracking monad makes it so that the state is
    backtracked on failure.
    Backtracking differs from regular exception in that, writing (+)
    for exception catching and (>>=) for bind, we require the
    following extra distributivity laws:
    x+(y+z) = (x+y)+z
    zero+x = x
    x+zero = x
    (x+y)>>=k = (x>>=k)+(y>>=k) *)
(** A view type for the logical monad, which is a form of list, hence
    we can decompose it with as a list. *)
type ('a, 'b) list_view =
  | Nil of exn
  | Cons of 'a * 'b

(** The monad is parametrised in the types of state, environment and
    writer. *)
module type Param = sig
  (** Read only *)
  type e
(** Write only *)
  type w
(** [w] must be a monoid *)
  val wunit : w
  val wprod : w -> w -> w
(** Read-write *)
  type s
(** Update-only. Essentially a writer on [u->u]. *)
  type u
(** [u] must be pointed. *)
  val uunit : u
end

module Logical (P:Param) : sig
  include MONAD
  val map : ('a -> 'b) -> 'a t -> 'b t
  val ignore : 'a t -> unit t
  val set : P.s -> unit t
  val get : P.s t
  val modify : (P.s -> P.s) -> unit t
  val put : P.w -> unit t
  val current : P.e t
  val local : P.e -> 'a t -> 'a t
  val update : (P.u -> P.u) -> unit t
  val zero : exn -> 'a t
  val plus : 'a t -> (exn -> 'a t) -> 'a t
  val split : 'a t -> (('a,(exn->'a t)) list_view) t
  val once : 'a t -> 'a t
  val break : (exn -> exn option) -> 'a t -> 'a t
  (* val lift : 'a NonLogical.t -> 'a t *)
  type 'a reified

  type state = {
    e: P.e;
    w: P.w;
    s: P.s;
    u: P.u;
  }

  val repr : 'a reified -> ('a, exn -> 'a reified) list_view NonLogical.t
  val run : 'a t -> P.e -> P.s -> ('a * state) reified
end
