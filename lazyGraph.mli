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

(** {1 Lazy graph polymorphic data structure} *)

(** This module serves to represent directed graphs in a lazy fashion. Such
    a graph is always accessed from a given initial node (so only connected
    components can be represented by a single value of type ('v,'e) t).
    
    The default equality considered here is [(=)], and the default hash
    function is {! Hashtbl.hash}. *)

(** {2 Type definitions} *)

type ('id, 'v, 'e) t = {
  eq : 'id -> 'id -> bool;
  hash : 'id -> int;
  force : 'id -> ('id, 'v, 'e) node;
} (** Lazy graph structure. Vertices, that have unique identifiers of type 'id,
      are annotated with values of type 'v, and edges are annotated by type 'e.
      A graph is a function that maps each identifier to a label and some edges to
      other vertices, or to Empty if the identifier is not part of the graph. *)
and ('id, 'v, 'e) node =
  | Empty
  | Node of 'id * 'v * ('e * 'id) Enum.t
  (** A single node of the graph, with outgoing edges *)
and ('id, 'e) path = ('id * 'e * 'id) list
  (** A reverse path (from the last element of the path to the first). *)

(** {2 Basic constructors} *)

(** It is difficult to provide generic combinators to build graphs. The problem
    is that if one wants to "update" a node, it's still very hard to update
    how other nodes re-generate the current node at the same time.
    The best way to do it is to build one function that maps the
    underlying structure of the type vertex to a graph (for instance,
    a concrete data structure, or an URL...). *)

val empty : ('id, 'v, 'e) t
  (** Empty graph *)

val singleton : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
                'id -> 'v -> ('id, 'v, 'e) t
  (** Trivial graph, composed of one node *)

val make : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
           ('id -> ('id,'v,'e) node) -> ('id,'v,'e) t
  (** Build a graph from the [force] function *)

val from_enum : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
                vertices:('id * 'v) Enum.t ->
                edges:('id * 'e * 'id) Enum.t ->
                ('id, 'v, 'e) t
  (** Concrete (eager) representation of a Graph (XXX not implemented)*)

val from_fun : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
               ('id -> ('v * ('e * 'id) list) option) -> ('id, 'v, 'e) t
  (** Convenient semi-lazy implementation of graphs *)

(** {2 Polymorphic utils} *)

(** A set of vertices *)
type 'id set =
  <
    mem : 'id -> bool;
    add : 'id -> unit;
    remove : 'id -> unit;
    iter : ('id -> unit) -> unit;
  >

val mk_hset : ?eq:('id -> 'id -> bool) -> hash:('id -> int) -> 'id set
  (** Make a set based on hashtables *)

val mk_tset : cmp:('id -> 'id -> int) -> 'id set
  (** Make a set based on balanced trees *)

type ('id,'a) map =
  <
    mem : 'id -> bool;
    get : 'id -> 'a;   (* or Not_found *)
    add : 'id -> 'a -> unit;
    remove : 'id -> unit;
    iter : ('id -> 'a -> unit) -> unit;
  >

val mk_hmap : ?eq:('id -> 'id -> bool) -> hash:('id -> int) -> ('id,'a) map

val mk_tmap : cmp:('id -> 'id -> int) -> ('id,'a) map

(** {2 Mutable concrete implementation} *)

type ('id, 'v, 'e) graph = ('id, 'v, 'e) t (* alias *)

module Mutable : sig
  type ('id, 'v, 'e) t
    (** Mutable graph *)

  val create : ?eq:('id -> 'id -> bool) -> hash:('id -> int) ->
    ('id, 'v, 'e) t * ('id, 'v, 'e) graph
    (** Create a new graph from the given equality and hash function, plus
        a view of it as an abstract graph *)

  val add_vertex : ('id, 'v, 'e) t -> 'id -> 'v -> unit
    (** Add a vertex to the graph *)

  val add_edge : ('id, 'v, 'e) t -> 'id -> 'e -> 'id -> unit
    (** Add an edge; the two vertices must already exist *)
end

(** {2 Traversals} *)

(** {3 Full interface to traversals} *)
module Full : sig
  type ('id, 'v, 'e) traverse_event =
    | EnterVertex of 'id * 'v * int * ('id, 'e) path (* unique ID, trail *)
    | ExitVertex of 'id (* trail *)
    | MeetEdge of 'id * 'e * 'id * edge_type (* edge *)
  and edge_type =
    | EdgeForward     (* toward non explored vertex *)
    | EdgeBackward    (* toward the current trail *)
    | EdgeTransverse  (* toward a totally explored part of the graph *)

  val bfs_full : ?id:int -> ?explored:(unit -> 'id set) ->
                  ('id, 'v, 'e) t -> 'id Enum.t ->
                  ('id, 'v, 'e) traverse_event Enum.t
    (** Lazy traversal in breadth first from a finite set of vertices *)

  val dfs_full : ?id:int -> ?explored:(unit -> 'id set) ->
                 ('id, 'v, 'e) t -> 'id Enum.t ->
                 ('id, 'v, 'e) traverse_event Enum.t
    (** Lazy traversal in depth first from a finite set of vertices *)
end

(** The traversal functions assign a unique ID to every traversed node *)

val bfs : ?id:int -> ?explored:(unit -> 'id set) ->
          ('id, 'v, 'e) t -> 'id -> ('id * 'v * int) Enum.t
  (** Lazy traversal in breadth first *)

val dfs : ?id:int -> ?explored:(unit -> 'id set) ->
          ('id, 'v, 'e) t -> 'id -> ('id * 'v * int) Enum.t
  (** Lazy traversal in depth first *)

val enum : ('id, 'v, 'e) t -> 'id -> ('id * 'v) Enum.t * ('id * 'e * 'id) Enum.t
  (** Convert to an enumeration. The traversal order is undefined. *)

val depth : ('id, _, 'e) t -> 'id -> ('id, int, 'e) t
  (** Map vertices to their depth, ie their distance from the initial point *)

val min_path : ?distance:('id -> 'e -> 'id -> int) ->
               ?explored:(unit -> ('id, int * ('id,'e) path) map) ->
               ('id, 'v, 'e) t -> 'id -> 'id ->
               int * ('id, 'e) path
  (** Minimal path from the given Graph from the first vertex to
      the second. It returns both the distance and the path *)

(** {2 Lazy transformations} *)

val union : ?combine:('v -> 'v -> 'v) ->
            ('id, 'v, 'e) t -> ('id, 'v, 'e) t -> ('id, 'v, 'e) t
  (** Lazy union of the two graphs. If they have common vertices,
      [combine] is used to combine the labels. By default, the second
      label is dropped and only the first is kept *)

val map : vertices:('v -> 'v2) -> edges:('e -> 'e2) ->
          ('id, 'v, 'e) t -> ('id, 'v2, 'e2) t
  (** Map vertice and edge labels *)

val filter : ?vertices:('id -> 'v -> bool) ->
             ?edges:('id -> 'e -> 'id -> bool) ->
             ('id, 'v, 'e) t -> ('id, 'v, 'e) t
  (** Filter out vertices and edges that do not satisfy the given
      predicates. The default predicates always return true. *)

val product : ('id1, 'v1, 'e1) t -> ('id2, 'v2, 'e2) t ->
              ('id1 * 'id2, 'v1 * 'v2, 'e1 * 'e2) t
  (** Cartesian product of the two graphs *)

val limit_depth : max:int -> ('id, 'v, 'e) t -> ('id, 'v, 'e) t
  (** Return the same graph, but with a bounded depth. Vertices whose
      depth is too high will be replaced by Empty *)

module Infix : sig
  val (++) : ('id, 'v, 'e) t -> ('id, 'v, 'e) t -> ('id, 'v, 'e) t
    (** Union of graphs (alias for {! union}) *)
end

(** {2 Pretty printing in the DOT (graphviz) format *)
module Dot : sig
  type attribute = [
  | `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  val pp_enum : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
                name:string -> Format.formatter ->
                ('id,attribute list,attribute list) Full.traverse_event Enum.t ->
                unit

  val pp : name:string -> ('id, attribute list, attribute list) t ->
           Format.formatter ->
           'id Enum.t -> unit
    (** Pretty print the given graph (starting from the given set of vertices)
        to the channel in DOT format *)
end

(** {2 Example of graphs} *)

val divisors_graph : (int, int, unit) t

val collatz_graph : (int, int, unit) t
