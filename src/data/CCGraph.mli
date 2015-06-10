
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

(** {1 Simple Graph Interface} *)

type 'a sequence = ('a -> unit) -> unit
(** A sequence of items of type ['a], possibly infinite *)

type 'a sequence_once = 'a sequence
(** Sequence that should be used only once *)

exception Sequence_once
(** raised when a sequence meant to be used once is used several times *)

module Seq : sig
  type 'a t = 'a sequence
  val return : 'a -> 'a sequence
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

(** {2 Interfaces for graphs} *)

(** Directed graph with vertices of type ['v] and edges of type [e'] *)
type ('v, 'e) t = {
  children: 'v -> 'e sequence;
  origin: 'e -> 'v;
  dest: 'e -> 'v;
}

(** Mutable bitset for values of type ['v] *)
type 'v tag_set = {
  get_tag: 'v -> bool;
  set_tag: 'v -> unit; (** Set tag to [true] for the given element *)
}

(** Mutable table with keys ['k] and values ['a] *)
type ('k, 'a) table = {
  mem: 'k -> bool;
  find: 'k -> 'a;  (** @raise Not_found *)
  add: 'k -> 'a -> unit; (** Erases previous binding *)
  size: unit -> int;
}

(** Mutable set *)
type 'a set = ('a, unit) table

(** Default implementation for {!table}: a {!Hashtbl.t} *)
val mk_table: ?eq:('k -> 'k -> bool) -> ?hash:('k -> int) -> int -> ('k, 'a) table

(** {2 Traversals} *)

(** Bag of elements of type ['a] *)
type 'a bag = {
  push: 'a -> unit;
  is_empty: unit -> bool;
  pop: unit -> 'a;  (** raises some exception is empty *)
}

val mk_queue: unit -> 'a bag
val mk_stack: unit -> 'a bag

val mk_heap: leq:('a -> 'a -> bool) -> 'a bag
(** [mk_heap ~leq] makes a priority queue where [leq x y = true] means that
    [x] is smaller than [y] and should be prioritary *)

val traverse: ?tbl:(int -> 'v set) ->
              bag:(unit -> 'v bag) ->
              graph:('v, 'e) t ->
              'v sequence -> 'v sequence
(** Traversal of the given graph, starting from a sequence
    of vertices, using the given bag to choose the next vertex to
    explore. Each vertex is visited at most once. *)

val traverse_tag: tags:'v tag_set ->
                  bag:'v bag ->
                  graph:('v, 'e) t ->
                  'v sequence ->
                  'v sequence_once
(** One-shot traversal of the graph using a tag set and the given bag *)

val bfs: ?tbl:(int -> 'v set) -> graph:('v, 'e) t -> 'v sequence -> 'v sequence

val bfs_tag: tags:'v tag_set -> graph:('v, 'e) t -> 'v sequence -> 'v sequence_once

val dfs: ?tbl:(int -> 'v set) -> graph:('v, 'e) t -> 'v sequence -> 'v sequence

val dfs_tag: tags:'v tag_set -> graph:('v, 'e) t -> 'v sequence -> 'v sequence_once

val dijkstra : ?tbl:(int -> 'v set) ->
                ?dist:('e -> int) ->
                graph:('v, 'e) t ->
                'v sequence ->
                ('v * int) sequence
(** Dijkstra algorithm, traverses a graph in increasing distance order.
    Yields each vertex paired with its distance to the set of initial vertices
    (the smallest distance needed to reach the node from the initial vertices)
    @param dist distance from origin of the edge to destination,
      must be strictly positive. Default is 1 for every edge *)

val dijkstra_tag : ?dist:('e -> int) ->
                    tags:'v tag_set ->
                    graph:('v, 'e) t ->
                    'v sequence ->
                    ('v * int) sequence_once

