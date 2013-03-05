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

(** {1 A simple mutable directed graph.} *)

type ('v, 'e) t = ('v, ('v, 'e) node) PHashtbl.t
  (** Graph parametrized by a type for vertices, and one for edges *)
and ('v, 'e) node = {
  n_vertex : 'v;
  mutable n_next : ('e * 'v) list;
  mutable n_prev : ('e * 'v) list;
} (** A node of the graph *)

(** Create an empty graph. The int argument specifies the initial size *)
let empty ?hash ?eq size =
  PHashtbl.create ?hash ?eq size

let mk_v_set ?(size=10) graph =
  let open PHashtbl in
  empty ~hash:graph.hash ~eq:graph.eq size

let mk_v_table ?(size=10) graph =
  let open PHashtbl in
  create ~hash:graph.hash ~eq:graph.eq size

let is_empty graph =
  PHashtbl.length graph = 0

let length graph =
  PHashtbl.length graph

(** Create an empty node for this vertex *)
let empty_node v = {
  n_vertex = v;
  n_next = [];
  n_prev = [];
}

(** Copy of the graph *)
let copy graph =
  PHashtbl.map
    (fun v node ->
      let node' = empty_node v in
      node'.n_prev <- node.n_prev;
      node'.n_next <- node.n_next;
      node')
    graph

let get_node t v =
  try PHashtbl.find t v
  with Not_found ->
    let n = empty_node v in
    PHashtbl.replace t v n;
    n

let add t v1 e v2 =
  let n1 = get_node t v1
  and n2 = get_node t v2 in
  n1.n_next <- (e,v2) :: n1.n_next;
  n2.n_prev <- (e,v1) :: n2.n_prev;
  ()

let add_seq t seq =
  Sequence.iter (fun (v1,e,v2) -> add t v1 e v2) seq

let next t v =
  Sequence.of_list (PHashtbl.find t v).n_next

let prev t v =
  Sequence.of_list (PHashtbl.find t v).n_prev

let between t v1 v2 =
  let edges = Sequence.of_list (PHashtbl.find t v1).n_next in
  let edges = Sequence.filter (fun (e, v2') -> (PHashtbl.get_eq t) v2 v2') edges in
  Sequence.map fst edges

(** Call [k] on every vertex *)
let iter_vertices t k =
  PHashtbl.iter (fun v _ -> k v) t

let vertices t =
  Sequence.from_iter (iter_vertices t)

(** Call [k] on every edge *)
let iter t k =
  PHashtbl.iter
    (fun v1 node -> List.iter (fun (e, v2) -> k (v1, e, v2)) node.n_next)
    t

let to_seq t =
  Sequence.from_iter (iter t)

(** {2 Global operations} *)

(** Roots, ie vertices with no incoming edges *)
let roots g =
  let vertices = vertices g in
  Sequence.filter (fun v -> Sequence.is_empty (prev g v)) vertices

(** Leaves, ie vertices with no outgoing edges *)
let leaves g =
  let vertices = vertices g in
  Sequence.filter (fun v -> Sequence.is_empty (next g v)) vertices

(** Pick a vertex, or raise Not_found *)
let choose g =
  match Sequence.to_list (Sequence.take 1 (vertices g)) with
  | [x] -> x
  | [] -> raise Not_found
  | _ -> assert false

let rev_edge (v,e,v') = (v',e,v)

(** Reverse all edges in the graph, in place *)
let rev g =
  PHashtbl.iter
    (fun _ node -> (* reverse the incoming and outgoing edges *)
      let next = node.n_next in
      node.n_next <- node.n_prev;
      node.n_prev <- next)
    g

(** {2 Traversals} *)

(** Breadth-first search *)
let bfs graph first k =
  let q = Queue.create ()
  and explored = mk_v_set graph in
  Hashset.add explored first;
  Queue.push first q;
  while not (Queue.is_empty q) do
    let v = Queue.pop q in
    (* yield current node *)
    k v;
    (* explore children *)
    Sequence.iter
      (fun (e, v') -> if not (Hashset.mem explored v')
        then (Hashset.add explored v'; Queue.push v' q))
      (next graph v)
  done

let bfs_seq graph first =
  Sequence.from_iter (fun k -> bfs graph first k)

(** DFS, with callbacks called on each encountered node and edge *)
let dfs_full graph ?(labels=mk_v_table graph)
?(enter=fun _ -> ()) ?(exit=fun _ -> ())
?(tree_edge=fun _ -> ()) ?(fwd_edge=fun _ -> ()) ?(back_edge=fun _ -> ())
first
=
  (* next free number for traversal *)
  let count = ref (-1) in
  PHashtbl.iter (fun _ i -> count := max i !count) labels;
  (* explore the vertex. trail is the reverse path from v to first *)
  let rec explore trail v =
    if PHashtbl.mem labels v then () else begin
      (* first time we explore this node! give it an index, put it in trail *)
      let n = (incr count; !count) in
      PHashtbl.replace labels v n;
      let trail' = (v, n) :: trail in
      (* enter the node *)
      enter trail';
      (* explore edges *)
      Sequence.iter
        (fun (e, v') ->
          try let n' = PHashtbl.find labels v' in
              if n' < n && List.exists (fun (_,n'') -> n' = n'') trail'
                then back_edge (v,e,v')  (* back edge, cycle *)
              else
                fwd_edge (v,e,v')   (* forward or cross edge *)
          with Not_found ->
            tree_edge (v,e,v'); (* tree edge *)
            explore trail' v')  (* explore the subnode *)
        (next graph v);
      (* exit the node *)
      exit trail'
    end
  in
  explore [] first

(** Depth-first search, from given vertex. Each vertex is labelled
    with its index in the traversal order. *)
let dfs graph first k =
  (* callback upon entering node *)
  let enter = function
  | [] -> assert false
  | (v,n)::_ -> k (v,n)
  in
  dfs_full graph ~enter first

(** Is the graph acyclic? *)
let is_dag g =
  if is_empty g then true
  else if Sequence.is_empty (roots g) then false  (* DAGs have roots *)
  else try
    let labels = mk_v_table g in
    (* do a DFS from each root; any back edge indicates a cycle *)
    Sequence.iter
      (fun v ->
        dfs_full g ~labels ~back_edge:(fun _ -> raise Exit) v)
      (roots g);
    true   (* complete traversal without back edge *)
  with Exit ->
    false  (* back edge detected! *)

(** {2 Path operations} *)

type ('v, 'e) path = ('v * 'e * 'v) list

(** Reverse the path *)
let rev_path p =
  let rec rev acc p = match p with
  | [] -> acc
  | (v,e,v')::p' -> rev ((v',e,v)::acc) p'
  in rev [] p

exception ExitBfs

(** Find the minimal path, from the given [vertex], that does not contain
    any vertex satisfying [ignore], and that reaches a vertex
    that satisfies [goal]. It raises Not_found if no reachable node
    satisfies [goal]. *)
let min_path_full (type v) (type e) graph
?(cost=fun _ _ _ -> 1) ?(ignore=fun _ -> false) ~goal v =
  (* priority queue *)
  let cmp (_,i,_) (_,j,_) = i - j in
  let q = Heap.empty ~cmp in
  let explored = mk_v_set graph in
  Heap.insert q (v, 0, []);
  let best_path = ref (v,0,[]) in
  try
    while not (Heap.is_empty q) do
      let (v, cost_v, path) = Heap.pop q in
      if Hashset.mem explored v then ()  (* a shorter path is known *)
      else if ignore v then ()      (* ignore the node. *)
      else if goal v path           (* shortest path to goal node! *)
        then (best_path := v, cost_v, path; raise ExitBfs)
      else begin
        Hashset.add explored v;
        (* explore successors *)
        Sequence.iter
          (fun (e, v') ->
            if Hashset.mem explored v' || ignore v' then ()
            else
              let cost_v' = (cost v e v') + cost_v in
              let path' = (v',e,v) :: path in
              Heap.insert q (v', cost_v', path'))
          (next graph v)
      end
    done;
    (* if a satisfying path was found, Exit would have been raised *)
    raise Not_found
  with ExitBfs -> (* found shortest satisfying path *)
    !best_path

(** Minimal path from first vertex to second, given the cost function *)
let min_path graph ~cost v1 v2 =
  let cost _ e _ = cost e in
  let goal v' _ = (PHashtbl.get_eq graph) v' v2 in
  let _,_,path = min_path_full graph ~cost ~goal v1 in
  path

(** Maximal distance between the given vertex, and any other vertex
    in the graph that is reachable from it. *)
let diameter graph v =
  let diameter = ref 0 in
  (* no path is a goal, but we can use its length to update diameter *)
  let goal _ path =
    diameter := max !diameter (List.length path);
    false
  in
  try ignore (min_path_full graph ~goal v); assert false
  with Not_found ->
    !diameter  (* explored every shortest path *)

(** {2 Print to DOT} *)

type attribute = [
| `Color of string
| `Shape of string
| `Weight of int
| `Style of string
| `Label of string
| `Other of string * string
] (** Dot attribute *)

(** Pretty print the graph in DOT, on given formatter. Using a sequence
    allows to easily select which edges are important,
    or to combine several graphs with [Sequence.append]. *)
let pp ~name ?vertices
~(print_edge : 'v -> 'e -> 'v -> attribute list)
~(print_vertex : 'v -> attribute list) formatter (graph : ('v, 'e) t) =
  (* map vertex -> unique int *)
  let vertices = match vertices with
  | Some v -> v
  | None -> mk_v_table graph in
  (* map from vertices to integers *)
  let get_id =
    let count = ref 0 in
    fun vertex ->
      try PHashtbl.find vertices vertex
      with Not_found ->
        let n = !count in
        incr count;
        PHashtbl.replace vertices vertex n;
        n
  (* print an attribute *)
  and print_attribute formatter attr =
    match attr with
    | `Color c -> Format.fprintf formatter "color=%s" c
    | `Shape s -> Format.fprintf formatter "shape=%s" s
    | `Weight w -> Format.fprintf formatter "weight=%d" w
    | `Style s -> Format.fprintf formatter "style=%s" s
    | `Label l -> Format.fprintf formatter "label=\"%s\"" l
    | `Other (name, value) -> Format.fprintf formatter "%s=\"%s\"" name value
  in
  (* the unique name of a vertex *)
  let pp_vertex formatter v =
    Format.fprintf formatter "vertex_%d" (get_id v) in
  (* print preamble *)
  Format.fprintf formatter "@[<v2>digraph %s {@;" name;
  (* print edges *)
  Sequence.iter
    (fun (v1, e, v2) ->
      let attributes = print_edge v1 e v2 in
      Format.fprintf formatter "  @[<h>%a -> %a [%a];@]@."
        pp_vertex v1 pp_vertex v2
        (Sequence.pp_seq ~sep:"," print_attribute)
        (Sequence.of_list attributes))
    (to_seq graph);
  (* print vertices *)
  PHashtbl.iter
    (fun v _ ->
      let attributes = print_vertex v in
      Format.fprintf formatter "  @[<h>%a [%a];@]@." pp_vertex v
        (Sequence.pp_seq ~sep:"," print_attribute) (Sequence.of_list attributes))
    vertices;
  (* close *)
  Format.fprintf formatter "}@]@;";
  ()
