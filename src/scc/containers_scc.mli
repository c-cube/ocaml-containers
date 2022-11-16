type 'a iter = ('a -> unit) -> unit

module type ARG = sig
  type t
  type node

  val children : t -> node -> node iter

  module Node_tbl : Hashtbl.S with type key = node
end

module type S = sig
  module A : ARG

  val scc : A.t -> A.node list -> A.node list list
end

module Make (A : ARG) : S with module A = A

val scc :
  tbl:(module Hashtbl.S with type key = 'node) ->
  graph:'graph ->
  children:('graph -> 'node -> 'node iter) ->
  nodes:'node list ->
  unit ->
  'node list list
(** Compute the strongly connected components of the given [graph],
      reachable from [nodes].

    @param graph the graph state
    @param children maps a node to its direct descendants (children)
    @param nodes initial nodes.
    @param tbl a hashtable implementation that takes nodes as keys
*)
