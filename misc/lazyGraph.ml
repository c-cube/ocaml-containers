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

type 'a sequence = ('a -> unit) -> unit

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
  | Node of 'id * 'v * ('e * 'id) sequence
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
    if eq v v' then Node (v, label, fun _ -> ()) else Empty in
  { force; eq; hash; }

let make ?(eq=(=)) ?(hash=Hashtbl.hash) force =
  { eq; hash; force; }

let from_fun ?(eq=(=)) ?(hash=Hashtbl.hash) f =
  let force v =
    match f v with
    | None -> Empty
    | Some (l, edges) -> Node (v, l, fun k -> List.iter k edges) in
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

  let create ?(eq=(=)) ?(hash=Hashtbl.hash) () =
    let map = mk_map ~eq ~hash in
    let force v =
      try let node = map.map_get v in
          Node (v, node.mut_v, fun k -> List.iter k node.mut_outgoing)
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

let from_enum ?(eq=(=)) ?(hash=Hashtbl.hash) ~vertices ~edges =
  let g, lazy_g = Mutable.create ~eq ~hash () in
  vertices
    (fun (v,label_v) -> Mutable.add_vertex g v label_v;);
  edges
    (fun (v1, e, v2) -> Mutable.add_edge g v1 e v2);
  lazy_g

let from_list ?(eq=(=)) ?(hash=Hashtbl.hash) l =
  let g, lazy_g = Mutable.create ~eq ~hash () in
  List.iter
    (fun (v1, e, v2) ->
      Mutable.add_vertex g v1 v1;
      Mutable.add_vertex g v2 v2;
      Mutable.add_edge g v1 e v2)
    l;
  lazy_g

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
    fun k ->
      let explored = mk_map ~eq:graph.eq ~hash:graph.hash in
      let id = ref 0 in
      let q = Queue.create () in (* queue of nodes to explore *)
      vertices (fun v -> Queue.push (FullEnter (v,[])) q);
      while not (Queue.is_empty q) do
        match Queue.pop q with
        | FullEnter (v', path) ->
          if not (explored.map_mem v')
            then begin match graph.force v' with
            | Empty -> ()
            | Node (_, label, edges) ->
              explored.map_add v' ();
              (* explore neighbors *)
              edges
                (fun (e,v'') ->
                  let path' = (v'',e,v') :: path in
                  Queue.push (FullFollowEdge path') q
                );
              (* exit node afterward *)
              Queue.push (FullExit v') q;
              (* return this vertex *)
              let i = !id in
              incr id;
              k (EnterVertex (v', label, i, path))
            end
        | FullExit v' -> k (ExitVertex v')
        | FullFollowEdge [] -> assert false
        | FullFollowEdge (((v'', e, v') :: path) as path') ->
          (* edge path .... v' --e--> v'' *)
          if explored.map_mem v''
            then if mem_path ~eq:graph.eq path v''
              then k (MeetEdge (v'', e, v', EdgeBackward))
              else k (MeetEdge (v'', e, v', EdgeTransverse))
            else begin
              (* explore this edge *)
              Queue.push (FullEnter (v'', path')) q;
              k (MeetEdge (v'', e, v', EdgeForward))
            end
      done

  (* TODO: use a set of nodes currently being explored, rather than
    checking whether the node is in the path (should be faster) *)

  let dfs_full graph vertices =
    fun k ->
      let explored = mk_map ~eq:graph.eq ~hash:graph.hash in
      let id = ref 0 in
      let s = Stack.create () in (* stack of nodes to explore *)
      vertices (fun v -> Stack.push (FullEnter (v,[])) s);
      while not (Stack.is_empty s) do
        match Stack.pop s with
        | FullExit v' -> k (ExitVertex v')
        | FullEnter (v', path) ->
          if not (explored.map_mem v')
            (* explore the node now *)
            then begin match graph.force v' with
            | Empty ->()
            | Node (_, label, edges) ->
              explored.map_add v' ();
              (* prepare to exit later *)
              Stack.push (FullExit v') s;
              (* explore neighbors *)
              edges
                (fun (e,v'') ->
                  Stack.push (FullFollowEdge ((v'', e, v') :: path)) s
                );
              (* return this vertex *)
              let i = !id in
              incr id;
              k (EnterVertex (v', label, i, path))
            end
        | FullFollowEdge [] -> assert false
        | FullFollowEdge (((v'', e, v') :: path) as path') ->
          (* edge path .... v' --e--> v'' *)
          if explored.map_mem v''
            then if mem_path ~eq:graph.eq path v''
              then k (MeetEdge (v'', e, v', EdgeBackward))
              else k (MeetEdge (v'', e, v', EdgeTransverse))
            else begin
              (* explore this edge *)
              Stack.push (FullEnter (v'', path')) s;
              k (MeetEdge (v'', e, v', EdgeForward))
            end
      done
end

let seq_filter_map f seq k =
  seq (fun x -> match f x with
      | None -> ()
      | Some y -> k y
      )

let bfs graph v =
  seq_filter_map
    (function
      | Full.EnterVertex (v, l, i, _) -> Some (v, l, i)
      | _ -> None)
    (Full.bfs_full graph (fun k -> k v))

let dfs graph v =
  seq_filter_map
    (function
      | Full.EnterVertex (v, l, i, _) -> Some (v, l, i)
      | _ -> None)
    (Full.dfs_full graph (fun k -> k v))

(** {3 Mutable heap} *)
module Heap = struct
  (** Implementation from http://en.wikipedia.org/wiki/Skew_heap *)

  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A pairing tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  let rec union ~cmp t1 t2 = match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (x1, l1, r1), Node (x2, l2, r2) ->
    if cmp x1 x2 <= 0
      then Node (x1, union ~cmp t2 r1, l1)
      else Node (x2, union ~cmp t1 r2, l2)

  let insert h x =
    h.tree <- union ~cmp:h.cmp (Node (x, Empty, Empty)) h.tree

  let pop h = match h.tree with
    | Empty -> raise Not_found
    | Node (x, l, r) ->
      h.tree <- union ~cmp:h.cmp l r;
      x
end

(** Node used to rebuild a path in A* algorithm *)
type ('id,'e) came_from = {
  mutable cf_explored : bool;     (* vertex explored? *)
  cf_node : 'id;                  (* ID of the vertex *)
  mutable cf_cost : float;        (* cost from start *)
  mutable cf_prev : ('id, 'e) came_from_edge;  (* path to origin *)
}
and ('id, 'e) came_from_edge =
  | CFStart
  | CFEdge of 'e * ('id, 'e) came_from

(** Shortest path from the first node to nodes that satisfy [goal], according
    to the given (positive!) distance function. The path is reversed,
    ie, from the destination to the source. The distance is also returned.
    [ignore] allows one to ignore some vertices during exploration.
    [heuristic] indicates the estimated distance to some goal, and must be
    - admissible (ie, it never overestimates the actual distance);
    - consistent (ie, h(X) <= dist(X,Y) + h(Y)).
    Both the distance and the heuristic must always
    be positive or null. *)
let a_star graph
  ?(on_explore=fun v -> ())
  ?(ignore=fun v -> false)
  ?(heuristic=(fun v -> 0.))
  ?(distance=(fun v1 e v2 -> 1.))
  ~goal
  start =
  fun k ->
    (* map node -> 'came_from' cell *)
    let nodes = mk_map ~eq:graph.eq ~hash:graph.hash in
    (* priority queue for nodes to explore *)
    let h = Heap.empty ~cmp:(fun (i,_) (j, _) -> compare i j) in
    (* initial node *)
    Heap.insert h (0., start);
    let start_cell =
      {cf_explored=false; cf_cost=0.; cf_node=start; cf_prev=CFStart; } in
    nodes.map_add start start_cell;
    (* re_build the path from [v] to [start] *)
    let rec mk_path nodes path v =
      let node = nodes.map_get v in
      match node.cf_prev with
      | CFStart -> path
      | CFEdge (e, node') ->
        let v' = node'.cf_node in
        let path' = (v', e, v) :: path in
        mk_path nodes path' v'
    in
    (* explore nodes in the heap order *)
    while not (Heap.is_empty h) do
      (* next vertex *)
      let dist, v' = Heap.pop h in
      (* data for this vertex *)
      let cell = nodes.map_get v' in
      if not (cell.cf_explored || ignore v') then begin
        (* 'explore' the node *)
        on_explore v';
        cell.cf_explored <- true;
        match graph.force v' with
        | Empty -> ()
        | Node (_, label, edges) ->
          (* explore neighbors *)
          edges
            (fun (e,v'') ->
              let cost = dist +. distance v' e v'' +. heuristic v'' in
              let cell' =
                try nodes.map_get v''
                with Not_found ->
                  (* first time we meet this node *)
                  let cell' = {cf_cost=cost; cf_explored=false;
                               cf_node=v''; cf_prev=CFEdge (e, cell); } in
                  nodes.map_add v'' cell';
                  cell'
              in
              if not cell'.cf_explored
                then Heap.insert h (cost, v'')  (* new node *)
              else if cost < cell'.cf_cost
                then begin  (* put the node in [h] with a better cost *)
                  Heap.insert h (cost, v'');
                  cell'.cf_cost <- cost; (* update best cost/path *)
                  cell'.cf_prev <- CFEdge (e, cell);
                end);
          (* check whether the node we just explored is a goal node *)
          if goal v'
            (* found a goal node! yield it *)
            then k (dist, mk_path nodes [] v')
        end
    done

exception ExitHead
let seq_head seq =
  let r = ref None in
  try
    seq (fun x -> r := Some x; raise ExitHead); None
  with ExitHead -> !r
    
(** Shortest path from the first node to the second one, according
    to the given (positive!) distance function. The path is reversed,
    ie, from the destination to the source. The int is the distance. *)
let dijkstra graph ?on_explore ?(ignore=fun v -> false)
  ?(distance=fun v1 e v2 -> 1.) v1 v2 =
  let paths =
    a_star graph ?on_explore ~ignore ~distance ~heuristic:(fun _ -> 0.)
       ~goal:(fun v -> graph.eq v v2) v1
  in
  match seq_head paths with
  | None -> raise Not_found
  | Some x -> x

exception ExitForall
let seq_for_all p seq =
  try
    seq (fun x -> if not (p x) then raise ExitForall);
    true
  with ExitForall -> false


(** Is the subgraph explorable from the given vertex, a Directed
    Acyclic Graph? *)
let is_dag graph v =
  seq_for_all
    (function
      | Full.MeetEdge (_, _, _, Full.EdgeBackward) -> false
      | _ -> true)
    (Full.dfs_full graph (fun k -> k v))

let is_dag_full graph vs =
  seq_for_all
    (function
      | Full.MeetEdge (_, _, _, Full.EdgeBackward) -> false
      | _ -> true)
    (Full.dfs_full graph vs)

let rec _cut_path ~eq v path = match path with
  | [] -> []
  | (v'', e, v') :: _ when eq v v' -> [v'', e, v']  (* cut *)
  | (v'', e, v') :: path' -> (v'', e, v') :: _cut_path ~eq v path'

let find_cycle graph v =
  let cycle = ref [] in
  try
    let path_stack = Stack.create () in
    let seq = Full.dfs_full graph (fun k -> k v) in
    seq (function
      | Full.EnterVertex (_, _, _, path) ->
        Stack.push path path_stack
      | Full.ExitVertex _ ->
        ignore (Stack.pop path_stack)
      | Full.MeetEdge(v1, e, v2, Full.EdgeBackward) ->
        (* found a cycle! cut the non-cyclic part and add v1->v2 at the beginning *)
        let path = _cut_path ~eq:graph.eq v1 (Stack.top path_stack) in
        let path = (v1, e, v2) :: path in
        cycle := path;
        raise Exit
      | Full.MeetEdge _ -> ()
    );
    raise Not_found
  with Exit ->
    !cycle

(** Reverse the path *)
let rev_path p =
  let rec rev acc p = match p with
  | [] -> acc
  | (v,e,v')::p' -> rev ((v',e,v)::acc) p'
  in rev [] p

(** {2 Lazy transformations} *)

let seq_map f seq k = seq (fun x -> k (f x))
let seq_append s1 s2 k = s1 k; s2 k

let union ?(combine=fun x y -> x) g1 g2 =
  let force v =
    match g1.force v, g2.force v with
    | Empty, Empty -> Empty
    | ((Node _) as n), Empty -> n
    | Empty, ((Node _) as n) -> n
    | Node (_, l1, e1), Node (_, l2, e2) ->
      Node (v, combine l1 l2, seq_append e1 e2)
  in { eq=g1.eq; hash=g1.hash; force; }

let map ~vertices ~edges g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) ->
      let edges_enum' = seq_map (fun (e,v') -> (edges e), v') edges_enum in
      Node (v, vertices l, edges_enum')
  in { eq=g.eq; hash=g.hash; force; }

let seq_flat_map f seq k = seq (fun x -> f x k)

(** Replace each vertex by some vertices. By mapping [v'] to [f v'=v1,...,vn],
    whenever [v] ---e---> [v'], then [v --e--> vi] for i=1,...,n. *)
let flatMap f g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) ->
      let edges_enum' = seq_flat_map
        (fun (e, v') ->
          seq_map (fun v'' -> e, v'') (f v'))
        edges_enum in
      Node (v, l, edges_enum')
  in { eq=g.eq; hash=g.hash; force; }

let seq_filter p seq k = seq (fun x -> if p x then k x)

let filter ?(vertices=(fun v l -> true)) ?(edges=fun v1 e v2 -> true) g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) when vertices v l ->
      (* filter out edges *)
      let edges_enum' = seq_filter (fun (e,v') -> edges v e v') edges_enum in
      Node (v, l, edges_enum')
    | Node _ -> Empty  (* filter out this vertex *)
  in { eq=g.eq; hash=g.hash; force; }

let seq_product s1 s2 k =
  s1 (fun x -> s2 (fun y -> k(x,y)))

let product g1 g2 =
  let force (v1,v2) =
    match g1.force v1, g2.force v2 with
    | Empty, _
    | _, Empty -> Empty
    | Node (_, l1, edges1), Node (_, l2, edges2) ->
      (* product of edges *)
      let edges = seq_product edges1 edges2 in
      let edges = seq_map (fun ((e1,v1'),(e2,v2')) -> ((e1,e2),(v1',v2'))) edges in
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
    events
      (function
        | Full.EnterVertex (v, attrs, _, _) ->
          Format.fprintf formatter "  @[<h>%a %a;@]@." pp_vertex v
            (CCList.print ~start:"[" ~stop:"]" ~sep:"," print_attribute) attrs
        | Full.ExitVertex _ -> ()
        | Full.MeetEdge (v2, attrs, v1, _) ->
          Format.fprintf formatter "  @[<h>%a -> %a %a;@]@."
            pp_vertex v1 pp_vertex v2
            (CCList.print ~start:"[" ~stop:"]" ~sep:"," print_attribute)
            attrs
      );
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
        let edges = seq_map (fun i -> (), i) (fun k -> List.iter k l) in
        Node (i, i, edges)
      else
        Node (i, i, fun _ -> ())
  in make force

let collatz_graph =
  let force i =
    if i mod 2 = 0
      then Node (i, i, fun k -> k ((), i / 2))
      else Node (i, i, fun k -> k ((), i * 3 + 1))
  in make force

let collatz_graph_bis =
  let force i =
    let l =
      [ true, if i mod 2 = 0 then i/2 else i*3+1
      ; false, i * 2 ] @
      if i mod 3 = 1 then [false, (i-1)/3] else []
    in
    Node (i, i, fun k -> List.iter k l)
  in make force

let heap_graph =
  let force i =
    Node (i, i, fun k -> List.iter k [(), 2*i; (), 2*i+1])
  in make force
