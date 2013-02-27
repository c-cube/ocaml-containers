(** {1 A simple persistent directed graph.} *)

module type S = sig
  (** {2 Basics} *)

  type vertex

  module M : Map.S with type key = vertex
  module S : Set.S with type elt = vertex

  type 'e t
    (** Graph parametrized by a type for edges *)

  val empty : 'e t
    (** Create an empty graph. *)

  val is_empty : 'e t -> bool
    (** Is the graph empty? *)

  val length : 'e t -> int
    (** Number of vertices *)

  val add : 'e t -> vertex -> 'e -> vertex -> 'e t
    (** Add an edge between two vertices *)

  val add_seq : 'e t -> (vertex * 'e * vertex) Sequence.t -> 'e t
    (** Add the vertices to the graph *)

  val next : 'e t -> vertex -> ('e * vertex) Sequence.t
    (** Outgoing edges *)

  val prev : 'e t -> vertex -> ('e * vertex) Sequence.t
    (** Incoming edges *)

  val between : 'e t -> vertex -> vertex -> 'e Sequence.t

  val iter_vertices : 'e t -> (vertex -> unit) -> unit
  val vertices : 'e t -> vertex Sequence.t
      (** Iterate on vertices *)

  val iter : 'e t -> (vertex * 'e * vertex -> unit) -> unit 
  val to_seq : 'e t -> (vertex * 'e * vertex) Sequence.t
    (** Dump the graph as a sequence of vertices *)

  (** {2 Global operations} *)

  val roots : 'e t -> vertex Sequence.t
    (** Roots, ie vertices with no incoming edges *)

  val leaves : 'e t -> vertex Sequence.t
    (** Leaves, ie vertices with no outgoing edges *)

  val choose : 'e t -> vertex
    (** Pick a vertex, or raise Not_found *)

  val rev_edge : (vertex * 'e * vertex) -> (vertex * 'e * vertex)
  val rev : 'e t -> 'e t
    (** Reverse all edges *)

  (** {2 Traversals} *)

  val bfs : 'e t -> vertex -> (vertex -> unit) -> unit
  val bfs_seq : 'e t -> vertex -> vertex Sequence.t
    (** Breadth-first search, from given vertex *)

  val dfs_full : 'e t ->
                 ?labels:int M.t ref ->
                 ?enter:((vertex * int) list -> unit) ->
                 ?exit:((vertex * int) list -> unit) ->
                 ?tree_edge:((vertex * 'e * vertex) -> unit) ->
                 ?fwd_edge:((vertex * 'e * vertex) -> unit) ->
                 ?back_edge:((vertex * 'e * vertex) -> unit) ->
                 vertex -> 
                 unit
    (** DFS, with callbacks called on each encountered node and edge *)

  val dfs : 'e t -> vertex -> ((vertex * int) -> unit) -> unit
    (** Depth-first search, from given vertex. Each vertex is labelled
        with its index in the traversal order. *)

  val is_dag : 'e t -> bool
    (** Is the graph acyclic? *)

  (** {2 Path operations} *)

  type 'e path = (vertex * 'e * vertex) list

  val rev_path : 'e path -> 'e path
    (** Reverse the path *)

  val min_path_full : 'e t ->
                 ?cost:(vertex -> 'e -> vertex -> int) ->
                 ?ignore:(vertex -> bool) ->
                 goal:(vertex -> 'e path -> bool) ->
                 vertex ->
                 vertex * int * 'e path
    (** Find the minimal path, from the given [vertex], that does not contain
        any vertex satisfying [ignore], and that reaches a vertex
        that satisfies [goal]. It raises Not_found if no reachable node
        satisfies [goal]. *)

  val min_path : 'e t -> cost:('e -> int) -> vertex -> vertex -> 'e path
    (** Minimal path from first vertex to second, given the cost function,
        or raises Not_found *)

  val diameter : 'e t -> vertex -> int
    (** Maximal distance between the given vertex, and any other vertex
        in the graph that is reachable from it. *)

  (** {2 Print to DOT} *)

  type attribute = [
  | `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  type 'e dot_printer
    (** Helper to print a graph to DOT *)

  val mk_dot_printer : 
     print_edge:(vertex -> 'e -> vertex -> attribute list) ->
     print_vertex:(vertex -> attribute list) ->
     'e dot_printer
    (** Create a Dot graph printer. Functions to convert edges and vertices
        to Dot attributes must be provided. *)

  val pp : 'e dot_printer -> ?vertices:S.t -> name:string ->
            Format.formatter ->
            (vertex * 'e * vertex) Sequence.t -> unit
    (** Pretty print the graph in DOT, on given formatter. Using a sequence
        allows to easily select which edges are important,
        or to combine several graphs with [Sequence.append].
        An optional set of additional vertices to print can be given. *)
end

module Make(V : Map.OrderedType) : S with type vertex = V.t
