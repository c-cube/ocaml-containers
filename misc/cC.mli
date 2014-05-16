(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Functional Congruence Closure} *)

(** {2 Curryfied terms} *)

module type CurryfiedTerm = sig
  type symbol
  type t = private {
    shape : shape;      (** Which kind of term is it? *)
    tag : int;          (** Unique ID *)
  } (** A curryfied term *)
  and shape = private
    | Const of symbol   (** Constant *)
    | Apply of t * t    (** Curryfied application *)

  val mk_const : symbol -> t
  val mk_app : t -> t -> t
  val get_id : t -> int
  val eq : t -> t -> bool
  val pp_skel : out_channel -> t -> unit  (* print tags recursively *)
end

module Curryfy(X : Hashtbl.HashedType) : CurryfiedTerm with type symbol = X.t

(** {2 Congruence Closure} *)

module type S = sig
  module CT : CurryfiedTerm

  type t
    (** Congruence Closure instance *)

  exception Inconsistent of t * CT.t * CT.t * CT.t * CT.t
    (** Exception raised when equality and inequality constraints are
        inconsistent. [Inconsistent (a, b, a', b')] means that [a=b, a=a', b=b'] in
        the congruence closure, but [a' != b'] was asserted before. *)

  val create : int -> t
    (** Create an empty CC of given size *)

  val eq : t -> CT.t -> CT.t -> bool
    (** Check whether the two terms are equal *)

  val merge : t -> CT.t -> CT.t -> t
    (** Assert that the two terms are equal (may raise Inconsistent) *)

  val distinct : t -> CT.t -> CT.t -> t
    (** Assert that the two given terms are distinct (may raise Inconsistent) *)

  type action =
    | Merge of CT.t * CT.t
    | Distinct of CT.t * CT.t
    (** Action that can be performed on the CC *)

  val do_action : t -> action -> t
    (** Perform the given action (may raise Inconsistent) *)

  val can_eq : t -> CT.t -> CT.t -> bool
    (** Check whether the two terms can be equal *)

  val iter_equiv_class : t -> CT.t -> (CT.t -> unit) -> unit
    (** Iterate on terms that are congruent to the given term *)

  type explanation =
    | ByCongruence of CT.t * CT.t  (* direct congruence of terms *)
    | ByMerge of CT.t * CT.t       (* user merge of terms *)

  val explain : t -> CT.t -> CT.t -> explanation list
    (** Explain why those two terms are equal (assuming they are,
        otherwise raises Invalid_argument) by returning a list
        of merges. *)
end

module Make(T : CurryfiedTerm) : S with module CT = T

module StrTerm : CurryfiedTerm with type symbol = string

module StrCC : S with module CT = StrTerm

val parse : string -> StrTerm.t
val pp : Format.formatter -> StrTerm.t -> unit
