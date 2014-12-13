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

type 'a sequence = ('a -> unit) -> unit

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
  | Node of 'id * 'v * ('e * 'id) sequence
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
                vertices:('id * 'v) sequence ->
                edges:('id * 'e * 'id) sequence ->
                ('id, 'v, 'e) t
  (** Concrete (eager) representation of a Graph *)

val from_list : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
                ('id * 'e * 'id) list ->
                ('id, 'id, 'e) t
  (** Simple way to generate a graph, from a list of edges *)

val from_fun : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) ->
               ('id -> ('v * ('e * 'id) list) option) -> ('id, 'v, 'e) t
  (** Convenient semi-lazy implementation of graphs *)

(** {2 Mutable concrete implementation} *)

type ('id, 'v, 'e) graph = ('id, 'v, 'e) t (* alias *)

module Mutable : sig
  type ('id, 'v, 'e) t
    (** Mutable graph *)

  val create : ?eq:('id -> 'id -> bool) -> ?hash:('id -> int) -> unit ->
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

  val bfs_full : ('id, 'v, 'e) t -> 'id sequence ->
                  ('id, 'v, 'e) traverse_event sequence
    (** Lazy traversal in breadth first from a finite set of vertices *)

  val dfs_full : ('id, 'v, 'e) t -> 'id sequence ->
                 ('id, 'v, 'e) traverse_event sequence
    (** Lazy traversal in depth first from a finite set of vertices *)
end

(** The traversal functions assign a unique ID to every traversed node *)

val bfs : ('id, 'v, 'e) t -> 'id -> ('id * 'v * int) sequence
  (** Lazy traversal in breadth first *)

val dfs : ('id, 'v, 'e) t -> 'id -> ('id * 'v * int) sequence
  (** Lazy traversal in depth first *)

module Heap : sig
  type 'a t
  val empty : cmp:('a -> 'a -> int) -> 'a t
  val is_empty : _ t -> bool
  val insert : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end

val a_star : ('id, 'v, 'e) t ->
             ?on_explore:('id -> unit) ->
             ?ignore:('id -> bool) ->
             ?heuristic:('id -> float) ->
             ?distance:('id -> 'e -> 'id -> float) ->
             goal:('id -> bool) ->
             'id ->
             (float * ('id, 'e) path) sequence
  (** Shortest path from the first node to nodes that satisfy [goal], according
      to the given (positive!) distance function. The distance is also returned.
      [ignore] allows one to ignore some vertices during exploration.
      [heuristic] indicates the estimated distance to some goal, and must be
      - admissible (ie, it never overestimates the actual distance);
      - consistent (ie, h(X) <= dist(X,Y) + h(Y)).
      Both the distance and the heuristic must always
      be positive or null. *)

val dijkstra : ('id, 'v, 'e) t ->
               ?on_explore:('id -> unit) ->
               ?ignore:('id -> bool) ->
               ?distance:('id -> 'e -> 'id -> float) ->
               'id -> 'id ->
               float * ('id, 'e) path
  (** Shortest path from the first node to the second one, according
      to the given (positive!) distance function. 
      [ignore] allows one to ignore some vertices during exploration. 
      This raises Not_found if no path could be found. *)

val is_dag : ('id, _, _) t -> 'id -> bool
  (** Is the subgraph explorable from the given vertex, a Directed
      Acyclic Graph? *)

val is_dag_full : ('id, _, _) t -> 'id sequence -> bool
  (** Is the Graph reachable from the given vertices, a DAG? See {! is_dag} *)

val find_cycle : ('id, _, 'e) t -> 'id -> ('id, 'e) path
  (** Find a cycle in the given graph.
      @raise Not_found if the graph is acyclic *)

val rev_path : ('id, 'e) path -> ('id, 'e) path
  (** Reverse the path *)

(** {2 Lazy transformations} *)

val union : ?combine:('v -> 'v -> 'v) ->
            ('id, 'v, 'e) t -> ('id, 'v, 'e) t -> ('id, 'v, 'e) t
  (** Lazy union of the two graphs. If they have common vertices,
      [combine] is used to combine the labels. By default, the second
      label is dropped and only the first is kept *)

val map : vertices:('v -> 'v2) -> edges:('e -> 'e2) ->
          ('id, 'v, 'e) t -> ('id, 'v2, 'e2) t
  (** Map vertice and edge labels *)

val flatMap : ('id -> 'id sequence) ->
              ('id, 'v, 'e) t ->
              ('id, 'v, 'e) t
  (** Replace each vertex by some vertices. By mapping [v'] to [f v'=v1,...,vn],
      whenever [v] ---e---> [v'], then [v --e--> vi] for i=1,...,n. Optional
      functions can be used to transform labels for edges and vertices. *)

val filter : ?vertices:('id -> 'v -> bool) ->
             ?edges:('id -> 'e -> 'id -> bool) ->
             ('id, 'v, 'e) t -> ('id, 'v, 'e) t
  (** Filter out vertices and edges that do not satisfy the given
      predicates. The default predicates always return true. *)

val product : ('id1, 'v1, 'e1) t -> ('id2, 'v2, 'e2) t ->
              ('id1 * 'id2, 'v1 * 'v2, 'e1 * 'e2) t
  (** Cartesian product of the two graphs *)

module Infix : sig
  val (++) : ('id, 'v, 'e) t -> ('id, 'v, 'e) t -> ('id, 'v, 'e) t
    (** Union of graphs (alias for {! union}) *)
end

(** {2 Pretty printing in the DOT (graphviz) format} *)
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
                ('id,attribute list,attribute list) Full.traverse_event sequence ->
                unit

  val pp : name:string -> ('id, attribute list, attribute list) t ->
           Format.formatter ->
           'id sequence -> unit
    (** Pretty print the given graph (starting from the given set of vertices)
        to the channel in DOT format *)
end

(** {2 Example of graphs} *)

val divisors_graph : (int, int, unit) t

val collatz_graph : (int, int, unit) t
  (** If [n] is even, [n] points to [n/2], otherwise to [3n+1] *)

val collatz_graph_bis : (int, int, bool) t
  (** Same as {! collatz_graph}, but also with reverse edges (n -> n*2,
      and n -> (n-1)/3 if n mod 3 = 1. Direct edges annotated with [true],
      reverse edges with [false] *)

val heap_graph : (int, int, unit) t
  (** maps an integer i to 2*i and 2*i+1 *)
