
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

(** {2 Hypergraph Representation}

Generalized Hypergraphs. Objects are either constants, or hyperedges that
connect [n] other objets together (a [n]-tuple). Each hyperedge can contain
additional data.

Hashconsing is used to ensure that structural equality implies physical
equality. This makes this module non thread safe.
*)

module type S = sig
  type const
    (** Constants. Those are what can annotate hyperedges or make single,
        leaf, nodes. *)

  type data
    (** Additional data carried by the hypergraph elements *)

  type t
    (** An element of the hypergraph. It can be parametrized by a ['a option]
        additional data (use 'a = unit if you don't care). *)

  val eq : t -> t -> bool
    (** Structural equality of the two edges *)

  val hash : t -> int
    (** Hash, used for hashtables *)

  val id : t -> int
    (** Same as {!hash}, but guarantees that the int is actually unique. *)

  val cmp : t -> t -> int
    (** Arbitrary total order *)

  val data : t -> data option
    (** Data contained in this edge, if any *)

  val const : t -> const
    (** Constant that annotates this hyperedge. *)

  val arity : t -> int
    (** Number of sub-elements *)

  val nth : t -> int -> t
    (** [nth x i] accesses the [i]-th sub-node of [x].
        @raise Invalid_argument if [i >= arity x]. *)

  val sub : t -> t array
    (** Access the sub-nodes as an array. This array {b MUST NOT} be modified
        by the caller. *)

  val make : ?data:data -> const -> t array -> t
    (** Create a new hyperedge from a constant that annotates it, and
        an ordered tuple of sub-edges.
        @param data optional data to decorate the edge. *)

  val make_l : ?data:data -> const -> t list -> t
    (** From a list, same as {!make} otherwise *)

  val const : ?data:data -> const -> t
    (** Constant node *)

  val update : ?data:data -> t -> t array -> t
    (** [update e sub] creates an hyperedge equivalent to [e] in all ways,
        but with the given sub-nodes as sub-edges. The array's ownership
        is lost by the caller.

        @param data optional data that annotates the new edge.
    *)

  val pp : Buffer.t -> t -> unit
    (** Print itself (non-recursively) on the buffer *)
end

module type PARAM = sig
  type const
  type data

  val eq : const -> const -> bool
  val hash : const -> int
  val to_string : const -> string  (* for printing *)
end

module Make(P : PARAM) : S with type const = P.const and type data = P.data

(** {2 Useful default} *)

module DefaultParam : sig
  type const =
    | S of string
    | I of int

  type data = unit

  include PARAM with type const := const and type data := data

  val i : int -> const
  val s : string -> const
end

module Default : S with type const = DefaultParam.const and type data = DefaultParam.data
