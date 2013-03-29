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

(** This module serves to represent directed graphs in a lazy fashion. Such
    a graph is always accessed from a given initial node (so only connected
    components can be represented by a single value of type ('v,'e) t). *)

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
  | Node of 'id * 'v * ('e * 'id) Gen.t
  (** A single node of the graph, with outgoing edges *)
and ('id, 'e) path = ('id * 'e * 'id) list
  (** A reverse path (from the last element of the path to the first). *)

(** {2 Basic constructors} *)

let empty =
  { eq=(==);
    hash=Hashtbl.hash;
    force = (fun _ -> Empty);
  }

let singleton ?(eq=(=)) ?(hash=Hashtbl.hash) v label =
  let force v' =
    if eq v v' then Node (v, label, Gen.empty) else Empty in
  { force; eq; hash; }

let make ?(eq=(=)) ?(hash=Hashtbl.hash) force =
  { eq; hash; force; }

let from_enum ?(eq=(=)) ?(hash=Hashtbl.hash) ~vertices ~edges =
  failwith "from_enum: not implemented"

let from_fun ?(eq=(=)) ?(hash=Hashtbl.hash) f =
  let force v =
    match f v with
    | None -> Empty
    | Some (l, edges) -> Node (v, l, Gen.of_list edges) in
  { eq; hash; force; }

(** {2 Polymorphic map} *)

type ('id, 'a) map = {
  map_is_empty : unit -> bool;
  map_mem : 'id -> bool;
  map_add : 'id -> 'a -> unit;
  map_get : 'id -> 'a;
}

let mk_map (type id) ~eq ~hash =
  let module H = Hashtbl.Make(struct
    type t = id
    let equal = eq
    let hash = hash
  end) in
  let h = H.create 3 in
  { map_is_empty = (fun () -> H.length h = 0);
    map_mem = (fun k -> H.mem h k);
    map_add = (fun k v -> H.replace h k v);
    map_get = (fun k -> H.find h k);
  }

(** {2 Mutable concrete implementation} *)

(** This is a general purpose eager implementation of graphs. It can be
    modified in place *)

type ('id, 'v, 'e) graph = ('id, 'v, 'e) t (* alias *)

module Mutable = struct
  type ('id, 'v, 'e) t = ('id, ('id, 'v, 'e) mut_node) map
  and ('id, 'v, 'e) mut_node = {
    mut_id : 'id;
    mutable mut_v : 'v;
    mutable mut_outgoing : ('e * 'id) list;
  }

  let create ?(eq=(=)) ~hash =
    let map = mk_map ~eq ~hash in
    let force v =
      try let node = map.map_get v in
          Node (v, node.mut_v, Gen.of_list node.mut_outgoing)
      with Not_found -> Empty in
    let graph = { eq; hash; force; } in
    map, graph

  let add_vertex map id v =
    if not (map.map_mem id)
      then
        let node = { mut_id=id; mut_v=v; mut_outgoing=[]; } in
        map.map_add id node

  let add_edge map v1 e v2 =
    let n1 = map.map_get v1 in
    n1.mut_outgoing <- (e, v2) :: n1.mut_outgoing;
    ()
end

(** {2 Traversals} *)

(** {3 Full interface to traversals} *)
module Full = struct
  type ('id, 'v, 'e) traverse_event =
    | EnterVertex of 'id * 'v * int * ('id, 'e) path (* unique ID, trail *)
    | ExitVertex of 'id (* trail *)
    | MeetEdge of 'id * 'e * 'id * edge_type (* edge *)
  and edge_type =
    | EdgeForward     (* toward non explored vertex *)
    | EdgeBackward    (* toward the current trail *)
    | EdgeTransverse  (* toward a totally explored part of the graph *)

  (* helper type *)
  type ('id,'e) todo_item =
    | FullEnter of 'id * ('id, 'e) path
    | FullExit of 'id
    | FullFollowEdge of ('id, 'e) path

  (** Is [v] part of the [path]? *)
  let rec mem_path ~eq path v =
    match path with
    | (v',_,v'')::path' ->
      (eq v v') || (eq v v'') || (mem_path ~eq path' v)
    | [] -> false

  let bfs_full graph vertices =
    fun () ->
      let explored = mk_map ~eq:graph.eq ~hash:graph.hash in
      let id = ref 0 in
      let q = Queue.create () in (* queue of nodes to explore *)
      Gen.iter (fun v -> Queue.push (FullEnter (v,[])) q) vertices;
      let rec next () =
        if Queue.is_empty q then raise Gen.EOG else
          match Queue.pop q with
          | FullEnter (v', path) ->
            if explored.map_mem v' then next ()
              else begin match graph.force v' with
              | Empty -> next ()
              | Node (_, label, edges) ->
                explored.map_add v' ();
                (* explore neighbors *)
                Gen.iter
                  (fun (e,v'') ->
                    let path' = (v'',e,v') :: path in
                    Queue.push (FullFollowEdge path') q)
                  edges;
                (* exit node afterward *)
                Queue.push (FullExit v') q;
                (* return this vertex *)
                let i = !id in
                incr id;
                EnterVertex (v', label, i, path)
              end
          | FullExit v' -> ExitVertex v'
          | FullFollowEdge [] -> assert false
          | FullFollowEdge (((v'', e, v') :: path) as path') ->
            (* edge path .... v' --e--> v'' *)
            if explored.map_mem v''
              then if mem_path ~eq:graph.eq path v''
                then MeetEdge (v'', e, v', EdgeBackward)
                else MeetEdge (v'', e, v', EdgeTransverse)
              else begin
                (* explore this edge *)
                Queue.push (FullEnter (v'', path')) q;
                MeetEdge (v'', e, v', EdgeForward)
              end
      in next

  let dfs_full graph vertices =
    fun () -> 
      let explored = mk_map ~eq:graph.eq ~hash:graph.hash in
      let id = ref 0 in
      let s = Stack.create () in (* stack of nodes to explore *)
      Gen.iter (fun v -> Stack.push (FullEnter (v,[])) s) vertices;
      let rec next () =
        if Stack.is_empty s then raise Gen.EOG else
          match Stack.pop s with
          | FullExit v' -> ExitVertex v'
          | FullEnter (v', path) ->
            if explored.map_mem v' then next ()
              (* explore the node now *)
              else begin match graph.force v' with
              | Empty -> next ()
              | Node (_, label, edges) ->
                explored.map_add v' ();
                (* prepare to exit later *)
                Stack.push (FullExit v') s;
                (* explore neighbors *)
                Gen.iter
                  (fun (e,v'') ->
                    Stack.push (FullFollowEdge ((v'', e, v') :: path)) s)
                  edges;
                (* return this vertex *)
                let i = !id in
                incr id;
                EnterVertex (v', label, i, path)
              end
          | FullFollowEdge [] -> assert false
          | FullFollowEdge (((v'', e, v') :: path) as path') ->
            (* edge path .... v' --e--> v'' *)
            if explored.map_mem v''
              then if mem_path ~eq:graph.eq path v''
                then MeetEdge (v'', e, v', EdgeBackward)
                else MeetEdge (v'', e, v', EdgeTransverse)
              else begin
                (* explore this edge *)
                Stack.push (FullEnter (v'', path')) s;
                MeetEdge (v'', e, v', EdgeForward)
              end
      in next
end

let bfs graph v =
  Gen.filterMap
    (function
      | Full.EnterVertex (v, l, i, _) -> Some (v, l, i)
      | _ -> None)
    (Full.bfs_full graph (Gen.singleton v))

let dfs graph v =
  Gen.filterMap
    (function
      | Full.EnterVertex (v, l, i, _) -> Some (v, l, i)
      | _ -> None)
    (Full.dfs_full graph (Gen.singleton v))

(** {3 Mutable heap (taken from heap.ml to avoid dependencies)} *)
module Heap = struct
  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A splay tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of ('a tree * 'a * 'a tree)
    (** A splay tree containing values of type 'a *)

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  (** Partition the tree into (elements <= pivot, elements > pivot) *)
  let rec partition ~cmp pivot tree =
    match tree with
    | Empty -> Empty, Empty
    | Node (a, x, b) ->
      if cmp x pivot <= 0
        then begin
          match b with
          | Empty -> (tree, Empty)
          | Node (b1, y, b2) ->
            if cmp y pivot <= 0
              then
                let small, big = partition ~cmp pivot b2 in
                Node (Node (a, x, b1), y, small), big
              else
                let small, big = partition ~cmp pivot b1 in
                Node (a, x, small), Node (big, y, b2)
        end else begin
          match a with
          | Empty -> (Empty, tree)
          | Node (a1, y, a2) ->
            if cmp y pivot <= 0
              then
                let small, big = partition ~cmp pivot a2 in
                Node (a1, y, small), Node (big, x, b)
              else
                let small, big = partition ~cmp pivot a1 in
                small, Node (big, y, Node (a2, x, b))
        end

  (** Insert the element in the tree *)
  let insert h x =
    let small, big = partition ~cmp:h.cmp x h.tree in
    let tree' = Node (small, x, big) in
    h.tree <- tree'

  (** Get minimum value and remove it from the tree *)
  let pop h =
    let rec delete_min tree = match tree with
    | Empty -> raise Not_found
    | Node (Empty, x, b) -> x, b
    | Node (Node (Empty, x, b), y, c) ->
      x, Node (b, y, c)  (* rebalance *)
    | Node (Node (a, x, b), y, c) ->
      let m, a' = delete_min a in
      m, Node (a', x, Node (b, y, c))
    in
    let m, tree' = delete_min h.tree in
    h.tree <- tree';
    m
end

(** Shortest path from the first node to the second one, according
    to the given (positive!) distance function. The path is reversed,
    ie, from the destination to the source. The int is the distance. *)
let disjktra graph ?(distance=fun v1 e v2 -> 1) v1 v2 =
  failwith "not implemented"

(** {2 Lazy transformations} *)

let union ?(combine=fun x y -> x) g1 g2 =
  let force v =
    match g1.force v, g2.force v with
    | Empty, Empty -> Empty
    | ((Node _) as n), Empty -> n
    | Empty, ((Node _) as n) -> n
    | Node (_, l1, e1), Node (_, l2, e2) ->
      Node (v, combine l1 l2, Gen.append e1 e2)
  in { eq=g1.eq; hash=g1.hash; force; }

let map ~vertices ~edges g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) ->
      let edges_enum' = Gen.map (fun (e,v') -> (edges e), v') edges_enum in
      Node (v, vertices l, edges_enum')
  in { eq=g.eq; hash=g.hash; force; }

let filter ?(vertices=(fun v l -> true)) ?(edges=fun v1 e v2 -> true) g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) when vertices v l ->
      (* filter out edges *)
      let edges_enum' = Gen.filter (fun (e,v') -> edges v e v') edges_enum in
      Node (v, l, edges_enum')
    | Node _ -> Empty  (* filter out this vertex *)
  in { eq=g.eq; hash=g.hash; force; }

let product g1 g2 =
  let force (v1,v2) =
    match g1.force v1, g2.force v2 with
    | Empty, _
    | _, Empty -> Empty
    | Node (_, l1, edges1), Node (_, l2, edges2) ->
      (* product of edges *)
      let edges = Gen.product edges1 edges2 in
      let edges = Gen.map (fun ((e1,v1'),(e2,v2')) -> ((e1,e2),(v1',v2'))) edges in
      Node ((v1,v2), (l1,l2), edges)
  and eq (v1,v2) (v1',v2') =
    g1.eq v1 v1' && g2.eq v2 v2'
  and hash (v1,v2) = ((g1.hash v1) * 65599) + g2.hash v2 
  in
  { eq; hash; force; }

module Infix = struct
  let (++) g1 g2 = union ?combine:None g1 g2
end

module Dot = struct
  type attribute = [
  | `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  (** Print an enum of Full.traverse_event *)
  let pp_enum ?(eq=(=)) ?(hash=Hashtbl.hash) ~name formatter events =
    (* print an attribute *)
    let print_attribute formatter attr =
      match attr with
      | `Color c -> Format.fprintf formatter "color=%s" c
      | `Shape s -> Format.fprintf formatter "shape=%s" s
      | `Weight w -> Format.fprintf formatter "weight=%d" w
      | `Style s -> Format.fprintf formatter "style=%s" s
      | `Label l -> Format.fprintf formatter "label=\"%s\"" l
      | `Other (name, value) -> Format.fprintf formatter "%s=\"%s\"" name value
    (* map from vertices to integers *)
    and get_id =
      let count = ref 0 in
      let m = mk_map ~eq ~hash in
      fun vertex ->
        try m.map_get vertex
        with Not_found ->
          let n = !count in
          incr count;
          m.map_add vertex n;
          n
    in
    (* the unique name of a vertex *)
    let pp_vertex formatter v =
      Format.fprintf formatter "vertex_%d" (get_id v) in
    (* print preamble *)
    Format.fprintf formatter "@[<v2>digraph %s {@;" name;
    (* traverse *)
    Gen.iter
      (function
        | Full.EnterVertex (v, attrs, _, _) ->
          Format.fprintf formatter "  @[<h>%a [%a];@]@." pp_vertex v
            (Gen.pp ~sep:"," print_attribute) (Gen.of_list attrs)
        | Full.ExitVertex _ -> ()
        | Full.MeetEdge (v2, attrs, v1, _) ->
          Format.fprintf formatter "  @[<h>%a -> %a [%a];@]@."
            pp_vertex v1 pp_vertex v2
            (Gen.pp ~sep:"," print_attribute)
            (Gen.of_list attrs))
      events;
    (* close *)
    Format.fprintf formatter "}@]@;@?";
    ()

  let pp ~name graph formatter vertices =
    let enum = Full.bfs_full graph vertices in
    pp_enum ~eq:graph.eq ~hash:graph.hash ~name formatter enum
end

(** {2 Example of graphs} *)

let divisors_graph =
  let rec divisors acc j i =
    if j = i then acc
    else
      let acc' = if (i mod j = 0) then j :: acc else acc in
      divisors acc' (j+1) i
  in
  let force i =
    if i > 2
      then
        let l = divisors [] 2 i in
        let edges = Gen.map (fun i -> (), i) (Gen.of_list l) in
        Node (i, i, edges)
      else
        Node (i, i, Gen.empty)
  in make force

let collatz_graph =
  let force i =
    if i mod 2 = 0
      then Node (i, i, Gen.singleton ((), i / 2))
      else Node (i, i, Gen.singleton ((), i * 3 + 1))
  in make force
