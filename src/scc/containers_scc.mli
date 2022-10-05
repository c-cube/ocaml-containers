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
