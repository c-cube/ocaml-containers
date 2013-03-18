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

(** {1 Lazy graph data structure} *)

module type S = sig
  (** This module serves to represent directed graphs in a lazy fashion. Such
      a graph is always accessed from a given initial node (so only connected
      components can be represented by a single value of type ('v,'e) t). *)

  (** {2 Type definitions} *)

  type vertex
    (** The concrete type of a vertex. Vertices are considered unique within
        the graph. *)

  type ('v, 'e) t = vertex -> ('v, 'e) node
    (** Lazy graph structure. Vertices are annotated with values of type 'v,
        and edges are of type 'e. A graph is a function that maps vertices
        to a label and some edges to other vertices. *)
  and ('v, 'e) node =
    | Empty
    | Node of vertex * 'v * ('e * vertex) Enum.t
    (** A single node of the graph, with outgoing edges *)

  (** {2 Basic constructors} *)

  (** It is difficult to provide generic combinators to build graphs. The problem
      is that if one wants to "update" a node, it's still very hard to update
      how other nodes re-generate the current node at the same time. *)

  val empty : ('v, 'e) t
    (** Empty graph *)

  val singleton : vertex -> 'v -> ('v, 'e) t
    (** Trivial graph, composed of one node *)

  val from_enum : vertices:(vertex * 'v) Enum.t ->
                 edges:(vertex * 'e * vertex) Enum.t ->
                 ('v, 'e) t
    (** Concrete (eager) representation of a Graph *)

  val from_fun : (vertex -> ('v * ('e * vertex) list) option) -> ('v, 'e) t
    (** Convenient semi-lazy implementation of graphs *)

  (** {2 Traversals} *)

  (** {3 Full interface to traversals} *)
  module Full : sig
    type ('v, 'e) traverse_event =
      | EnterVertex of vertex * 'v * int * vertex list (* unique ID, trail *)
      | ExitVertex of vertex (* trail *)
      | MeetEdge of vertex * 'e * vertex * edge_type (* edge *)
    and edge_type =
      | EdgeForward     (* toward non explored vertex *)
      | EdgeBackward    (* toward the current trail *)
      | EdgeTransverse  (* toward a totally explored part of the graph *)

    val bfs_full : ?id:int -> ('v, 'e) t -> vertex -> ('v, 'e) traverse_event Enum.t

    val dfs_full : ?id:int -> ('v, 'e) t -> vertex -> ('v, 'e) traverse_event Enum.t
      (** Lazy traversal in depth first *)
  end

  (** The traversal functions assign a unique ID to every traversed node *)

  val bfs : ?id:int -> ('v, 'e) t -> vertex -> (vertex * 'v * int) Enum.t
    (** Lazy traversal in breadth first *)

  val dfs : ?id:int -> ('v, 'e) t -> vertex -> (vertex * 'v * int) Enum.t
    (** Lazy traversal in depth first *)

  val enum : ('v, 'e) t -> vertex -> (vertex * 'v) Enum.t * (vertex * 'e * vertex) Enum.t
    (** Convert to an enumeration. The traversal order is undefined. *)

  val depth : (_, 'e) t -> vertex -> (int, 'e) t
    (** Map vertices to their depth, ie their distance from the initial point *)

  type 'e path = (vertex * 'e * vertex) list

  val min_path : ?distance:(vertex -> 'e -> vertex -> int) ->
                 ('v, 'e) t -> vertex -> vertex ->
                 int * 'e path
    (** Minimal path from the given Graph from the first vertex to
        the second. It returns both the distance and the path *)

  (** {2 Lazy transformations} *)

  val union : ?combine:('v -> 'v -> 'v) -> ('v, 'e) t -> ('v, 'e) t -> ('v, 'e) t
    (** Lazy union of the two graphs. If they have common vertices,
        [combine] is used to combine the labels. By default, the second
        label is dropped and only the first is kept *)

  val map : vertices:('v -> 'v2) -> edges:('e -> 'e2) ->
            ('v, 'e) t -> ('v2, 'e2) t
    (** Map vertice and edge labels *)

  val filter : ?vertices:(vertex -> 'v -> bool) ->
               ?edges:(vertex -> 'e -> vertex -> bool) ->
               ('v, 'e) t -> ('v, 'e) t
    (** Filter out vertices and edges that do not satisfy the given
        predicates. The default predicates always return true. *)

  val limit_depth : max:int -> ('v, 'e) t -> ('v, 'e) t
    (** Return the same graph, but with a bounded depth. Vertices whose
        depth is too high will be replaced by Empty *)

  module Infix : sig
    val (++) : ('v, 'e) t -> ('v, 'e) t -> ('v, 'e) t
      (** Union of graphs (alias for {! union}) *)
  end

  (** {2 Pretty printing in the DOT (graphviz) format *)
  module Dot : sig
    type graph
      (** A DOT graph *)

    val empty : string -> graph
      (** Create an empty graph with the given name *)

    type attribute = [
    | `Color of string
    | `Shape of string
    | `Weight of int
    | `Style of string
    | `Label of string
    | `Other of string * string
    ] (** Dot attribute *)

    val add : print_edge:(vertex -> 'e -> vertex -> attribute list) ->
              print_vertex:(vertex -> 'v -> attribute list) ->
              graph ->
              ('v,'e) t -> vertex Enum.t ->
              graph
      (** Add the given vertices of the graph to the DOT graph *)

    val pp : Format.formatter -> graph -> unit
      (** Pretty print the graph in DOT, on the given formatter. *)

    val to_string : graph -> string
      (** Pretty print the graph in a string *)
  end
end

(** {2 Module type for hashable types} *)
module type HASHABLE = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Implementation of HASHABLE with physical equality and hash} *)
module PhysicalHash(X : sig type t end) : HASHABLE with type t = X.t
  = struct
    type t = X.t
    let equal a b = a == b
    let hash a = Hashtbl.hash a
  end

(** {2 Build a graph} *)
module Make(X : HASHABLE) : S with type vertex = X.t = struct
  (** {2 Type definitions} *)

  type vertex = X.t
    (** The concrete type of a vertex. Vertices are considered unique within
        the graph. *)

  type ('v, 'e) t = vertex -> ('v, 'e) node
    (** Lazy graph structure. Vertices are annotated with values of type 'v,
        and edges are of type 'e. A graph is a function that maps vertices
        to a label and some edges to other vertices. *)
  and ('v, 'e) node =
    | Empty
    | Node of vertex * 'v * ('e * vertex) Enum.t
    (** A single node of the graph, with outgoing edges *)

  (** {2 Basic constructors} *)

  let empty =
    fun _ -> Empty

  let singleton v label =
    fun v' ->
      if X.equal v v' then Node (v, label, Enum.empty) else Empty

  let from_enum ~vertices ~edges = failwith "from_enum: not implemented"

  let from_fun f =
    fun v ->
      match f v with
      | None -> Empty
      | Some (l, edges) -> Node (v, l, Enum.of_list edges)

  (** {2 Traversals} *)

  (** {3 Full interface to traversals} *)
  module Full = struct
    type ('v, 'e) traverse_event =
      | EnterVertex of vertex * 'v * int * vertex list (* unique ID, trail *)
      | ExitVertex of vertex (* trail *)
      | MeetEdge of vertex * 'e * vertex * edge_type (* edge *)
    and edge_type =
      | EdgeForward     (* toward non explored vertex *)
      | EdgeBackward    (* toward the current trail *)
      | EdgeTransverse  (* toward a totally explored part of the graph *)

    let bfs_full ?(id=0) graph v = Enum.empty  (* TODO *)

    let dfs_full ?(id=0) graph v = Enum.empty  (* TODO *)
  end

  let bfs ?id graph v = Enum.empty (* TODO *)

  let dfs ?id graph v = Enum.empty (* TODO *)

  let enum graph v = (Enum.empty, Enum.empty)  (* TODO *)

  let depth graph v = failwith "not implemented"

  type 'e path = (vertex * 'e * vertex) list

  (** Minimal path from the given Graph from the first vertex to
      the second. It returns both the distance and the path *)
  let min_path ?(distance=fun v1 e v2 -> 1) graph v1 v2 = failwith "not implemented"

  (** {2 Lazy transformations} *)

  let union ?(combine=fun x y -> x) g1 g2 =
    fun v ->
      match g1 v, g2 v with
      | Empty, Empty -> Empty
      | ((Node _) as n), Empty -> n
      | Empty, ((Node _) as n) -> n
      | Node (_, l1, e1), Node (_, l2, e2) ->
        Node (v, combine l1 l2, Enum.append e1 e2)

  let map ~vertices ~edges g = failwith "not implemented"

  let filter ?(vertices=fun v l -> true) ?(edges=fun v1 e v2 -> true) g =
    failwith "not implemented"

  let limit_depth ~max g = failwith "not implemented"

  module Infix = struct
    let (++) g1 g2 = union ?combine:None g1 g2
  end

  module Dot = struct
    type graph = Graph of string (* TODO *)

    let empty name = Graph name

    type attribute = [
    | `Color of string
    | `Shape of string
    | `Weight of int
    | `Style of string
    | `Label of string
    | `Other of string * string
    ] (** Dot attribute *)

    let add ~print_edge ~print_vertex graph g vertices = graph (* TODO *)

    let pp formatter graph = failwith "not implemented"

    let to_string graph =
      let b = Buffer.create 64 in
      Format.bprintf b "%a@?" pp graph;
      Buffer.contents b
  end
end

(** {2 Build a graph based on physical equality} *)
module PhysicalMake(X : sig type t end) : S with type vertex = X.t
  = Make(PhysicalHash(X))
