(*
copyright (c) 2013-2015, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 Hashconsed Sets}

    Sets are hashconsed, so that set equality is physical equality. Some
    sub-structure that is common to several sets is also perfectly shared.

    {b status: unstable}

    @since 0.12
*)

module type ELT = sig
  type t

  val compare : t -> t -> int
  (** Total order *)

  val hash : t -> int
  (** Deterministic *)
end

module type S = sig
  type elt

  type t
  (** Set of elements *)

  val empty : t

  val singleton : elt -> t

  val doubleton : elt -> elt -> t

  val mem : elt -> t -> bool

  val equal : t -> t -> bool
  (** Fast equality test [O(1)] *)

  val compare : t -> t -> int
  (** Fast (arbitrary) comparison test [O(1)] *)

  val hash : t -> int
  (** Fast (arbitrary, deterministic) hash [O(1)] *)

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val cardinal : t -> int

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on elements, in no particular order *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold on elements, in arbitrary order *)

  val choose : t -> elt option

  val choose_exn : t -> elt

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  (** {2 Whole-collection operations} *)

  type 'a sequence = ('a -> unit) -> unit
  type 'a gen = unit -> 'a option

  val add_list : t -> elt list -> t

  val of_list : elt list -> t

  val to_list : t -> elt list

  val add_seq : t -> elt sequence -> t

  val of_seq : elt sequence -> t

  val to_seq : t -> elt sequence
end

module Make(E : ELT) : S with type elt = E.t
