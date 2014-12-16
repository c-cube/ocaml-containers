
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

CCGeneralized Hypergraphs. Objects are either constants, or hyperedges that
connect [n] other objets together (a [n]-tuple).

Hashconsing is used to ensure that structural equality implies physical
equality. This makes this module non thread safe.
*)

module type S = sig
  type const
    (** Constants. Those are what can annotate hyperedges or make single,
        leaf, nodes. *)

  type t
    (** An hypergraph. It stores a set of edges, and possibly inherits from
        another graph. *)

  type edge
    (** A single edge of the hypergraph. *)

  val self : t -> edge
    (** The edge that represents (reifies) the hypergraph itself *)

  val eq : edge -> edge -> bool
    (** Equality of the two edges. *)

  val arity : edge -> int
    (** Number of sub-elements of the edge (how many other edges it connects
        together) *)

  val nth : edge -> int -> edge
    (** [nth x i] accesses the [i]-th sub-node of [x].
        @raise Invalid_argument if [i >= arity x]. *)

  val make_graph : ?parent:t -> unit -> t
    (** New graph, possibly inheriting from another graph. *)

  val make_edge : t -> edge array -> edge
    (** Create a new hyperedge from an ordered tuple of sub-edges.
        The edge belongs to the given graph.
        The array must not be used afterwards and must not be empty.
        @raise Invalid_argument if the array is empty *)

  val make_const : t -> const -> edge
    (** Constant edge, without sub-edges *)

  val fresh : t -> edge
    (** Fresh edge, without constant. It is equal to no other edge. *)

  module EdgeTbl : Hashtbl.S with type key = edge

  val pp : ?printed:unit EdgeTbl.t ->
           Buffer.t -> edge -> unit
    (** Print the edge on the buffer. @param printed: sub-edges already
        printed. *)

  val fmt : Format.formatter -> edge -> unit
  val to_string : edge -> string
end

module type PARAM = sig
  type const

  val eq : const -> const -> bool
  val hash : const -> int
  val to_string : const -> string  (* for printing *)
end

module Make(P : PARAM) : S with type const = P.const

(** {2 Useful default} *)

module DefaultParam : sig
  type const =
    | S of string
    | I of int

  include PARAM with type const := const

  val i : int -> const
  val s : string -> const
end

module Default : sig
  include S with type const = DefaultParam.const

  module Lexbuf : sig
    type t

    val of_string : string -> t

    val of_fun : (unit -> string option) -> t

    val of_chan : in_channel -> t
  end

  val parse_edge : t -> Lexbuf.t -> [ `Ok of edge | `Error of string ]

  val edge_of_string : t -> string -> [ `Ok of edge | `Error of string ]
end
