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

(** {1 A simple polymorphic directed graph.} *)

type 'a sequence = ('a -> unit) -> unit

(** {2 Basics} *)

type ('v, 'e) t
  (** Graph parametrized by a type for vertices, and a type for edges *)

val empty : ?hash:('v -> int) -> ?eq:('v -> 'v -> bool) -> int -> ('v, 'e) t
  (** Create an empty graph. The int argument specifies the initial size *)

val mk_v_set : ?size:int -> ('v, _) t -> 'v Hashset.t
  (** Create an empty set of vertices *)

val mk_v_table : ?size:int -> ('v, _) t -> ('v, 'a) PHashtbl.t
  (** Create an empty hashtable of vertices *)

val copy : ('v, 'e) t -> ('v, 'e) t
  (** Copy the graph *)

val is_empty : (_, _) t -> bool
  (** Is the graph empty? *)

val length : (_, _) t -> int
  (** Number of vertices *)

val add : ('v,'e) t -> 'v -> 'e -> 'v -> unit
  (** Add an edge between two vertices *)

val add_seq : ('v,'e) t -> ('v * 'e * 'v) sequence -> unit
  (** Add the vertices to the graph *)

val next : ('v, 'e) t -> 'v -> ('e * 'v) sequence
  (** Outgoing edges *)

val prev : ('v, 'e) t -> 'v -> ('e * 'v) sequence
  (** Incoming edges *)

val between : ('v, 'e) t -> 'v -> 'v -> 'e sequence

val iter_vertices : ('v, 'e) t -> ('v -> unit) -> unit
val vertices : ('v, 'e) t -> 'v sequence
    (** Iterate on vertices *)

val iter : ('v, 'e) t -> ('v * 'e * 'v -> unit) -> unit 
val to_seq : ('v, 'e) t -> ('v * 'e * 'v) sequence
  (** Dump the graph as a sequence of vertices *)

(** {2 Global operations} *)

val roots : ('v, 'e) t -> 'v sequence
  (** Roots, ie vertices with no incoming edges *)

val leaves : ('v, 'e) t -> 'v sequence
  (** Leaves, ie vertices with no outgoing edges *)

val choose : ('v, 'e) t -> 'v
  (** Pick a 'v, or raise Not_found *)

val rev_edge : ('v * 'e * 'v) -> ('v * 'e * 'v)
  (** Reverse one edge *)

val rev : ('v, 'e) t -> unit
  (** Reverse all edges in the graph, in place *)

(** {2 Traversals} *)

val bfs : ('v, 'e) t -> 'v -> ('v -> unit) -> unit
  (** Breadth-first search, from given 'v *)

val bfs_seq : ('v, 'e) t -> 'v -> 'v sequence
  (** Sequence of vertices traversed during breadth-first search *)

val dfs_full : ('v, 'e) t ->
               ?labels:('v, int) PHashtbl.t ->
               ?enter:(('v * int) list -> unit) ->
               ?exit:(('v * int) list -> unit) ->
               ?tree_edge:(('v * 'e * 'v) -> unit) ->
               ?fwd_edge:(('v * 'e * 'v) -> unit) ->
               ?back_edge:(('v * 'e * 'v) -> unit) ->
               'v -> 
               unit
  (** DFS, with callbacks called on each encountered node and edge *)

val dfs : ('v, 'e) t -> 'v -> (('v * int) -> unit) -> unit
  (** Depth-first search, from given 'v. Each 'v is labelled
      with its index in the traversal order. *)

val is_dag : ('v, 'e) t -> bool
  (** Is the graph acyclic? *)

(** {2 Path operations} *)

type ('v, 'e) path = ('v * 'e * 'v) list
  (** A path is a list of edges connected by vertices. *)

val rev_path : ('v, 'e) path -> ('v, 'e) path
  (** Reverse the path *)

val min_path_full : ('v, 'e) t ->
               ?cost:('v -> 'e -> 'v -> int) ->
               ?ignore:('v -> bool) ->
               goal:('v -> ('v, 'e) path -> bool) ->
               'v ->
               'v * int * ('v, 'e) path
  (** Find the minimal path, from the given ['v], that does not contain
      any 'v satisfying [ignore], and that reaches a 'v
      that satisfies [goal]. It raises Not_found if no reachable node
      satisfies [goal]. The path is reversed. *)

val min_path : ('v, 'e) t -> cost:('e -> int) -> 'v -> 'v -> ('v,'e) path
  (** Minimal path from first 'v to second, given the cost function,
      or raises Not_found. The path is reversed. *)

val diameter : ('v, 'e) t -> 'v -> int
  (** Maximal distance between the given 'v, and any other 'v
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

val pp : name:string -> ?vertices:('v,int) PHashtbl.t ->
         print_edge:('v -> 'e -> 'v -> attribute list) ->
         print_vertex:('v -> attribute list) ->
          Format.formatter ->
          ('v, 'e) t -> unit
  (** Pretty print the graph in DOT, on given formatter. *)
