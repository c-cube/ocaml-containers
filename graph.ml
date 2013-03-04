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

module Make(V : Map.OrderedType) = struct
  module M = Map.Make(V)
  module S = Set.Make(V)

  type vertex = V.t

  type 'e t = 'e node M.t
    (** Graph parametrized by a type for edges *)
  and 'e node = {
    n_vertex : vertex;
    n_next : ('e * vertex) list;
    n_prev : ('e * vertex) list;
  } (** A node of the graph *)

  let empty = M.empty

  let is_empty graph = M.is_empty graph

  let length graph = M.cardinal graph

  let empty_node v = {
    n_vertex = v;
    n_next = [];
    n_prev = [];
  }

  let add t v1 e v2 =
    let n1 = try M.find v1 t with Not_found -> empty_node v1
    and n2 = try M.find v2 t with Not_found -> empty_node v2 in
    let n1 = { n1 with n_next = (e,v2) :: n1.n_next; }
    and n2 = { n2 with n_prev = (e,v1) :: n2.n_prev; } in
    M.add v1 n1 (M.add v2 n2 t)

  let add_seq t seq = Sequence.fold (fun t (v1,e,v2) -> add t v1 e v2) t seq

  let next t v = Sequence.of_list (M.find v t).n_next

  let prev t v = Sequence.of_list (M.find v t).n_prev

  let between t v1 v2 =
    let edges = Sequence.of_list (M.find v1 t).n_prev in
    let edges = Sequence.filter (fun (e, v2') -> V.compare v2 v2' = 0) edges in
    Sequence.map fst edges

  (** Call [k] on every vertex *)
  let iter_vertices t k = M.iter (fun v _ -> k v) t

  let vertices t = Sequence.from_iter (iter_vertices t)

  (** Call [k] on every edge *)
  let iter t k =
    M.iter
      (fun v1 node -> List.iter (fun (e, v2) -> k (v1, e, v2)) node.n_next)
      t

  let to_seq t = Sequence.from_iter (iter t)

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
  let choose g = fst (M.choose g)

  let rev_edge (v,e,v') = (v',e,v)

  (** Reverse all edges *)
  let rev g =
    M.map
      (fun node -> {node with n_prev=node.n_next; n_next=node.n_prev})
      g

  (** {2 Traversals} *)

  (** Breadth-first search *)
  let bfs graph first k =
    let q = Queue.create ()
    and explored = ref (S.singleton first) in
    Queue.push first q;
    while not (Queue.is_empty q) do
      let v = Queue.pop q in
      (* yield current node *)
      k v;
      (* explore children *)
      Sequence.iter
        (fun (e, v') -> if not (S.mem v' !explored)
          then (explored := S.add v' !explored; Queue.push v' q))
        (next graph v)
    done

  let bfs_seq graph first = Sequence.from_iter (fun k -> bfs graph first k)

  (** DFS, with callbacks called on each encountered node and edge *)
  let dfs_full graph ?(labels=ref M.empty)
  ?(enter=fun _ -> ()) ?(exit=fun _ -> ())
  ?(tree_edge=fun _ -> ()) ?(fwd_edge=fun _ -> ()) ?(back_edge=fun _ -> ())
  first
  =
    (* next free number for traversal *)
    let count = ref (-1) in
    M.iter (fun _ i -> count := max i !count) !labels;
    (* explore the vertex. trail is the reverse path from v to first *)
    let rec explore trail v =
      if M.mem v !labels then () else begin
        (* first time we explore this node! give it an index, put it in trail *)
        let n = (incr count; !count) in
        labels := M.add v n !labels;
        let trail' = (v, n) :: trail in
        (* enter the node *)
        enter trail';
        (* explore edges *)
        Sequence.iter
          (fun (e, v') ->
            try let n' = M.find v' !labels in
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
      let labels = ref M.empty in
      (* do a DFS from each root; any back edge indicates a cycle *)
      Sequence.iter
        (fun v ->
          dfs_full g ~labels ~back_edge:(fun _ -> raise Exit) v)
        (roots g);
      true   (* complete traversal without back edge *)
    with Exit ->
      false  (* back edge detected! *)

  (** {2 Path operations} *)

  type 'e path = (vertex * 'e * vertex) list

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
  let min_path_full (type e) graph ?(cost=fun _ _ _ -> 1) ?(ignore=fun _ -> false) ~goal v =
    let module HQ = Leftistheap.Make(struct
      type t = vertex * int * e path
      let le (_,i,_) (_,j,_) = i <= j
    end) in
    let q = ref HQ.empty in
    let explored = ref S.empty in
    q := HQ.insert (v, 0, []) !q;
    let best_path = ref (v,0,[]) in
    try
      while not (HQ.is_empty !q) do
        let (v, cost_v, path), q' = HQ.extract_min !q in
        q := q';
        if S.mem v !explored then ()  (* a shorter path is known *)
        else if ignore v then ()      (* ignore the node. *)
        else if goal v path           (* shortest path to goal node! *)
          then (best_path := v, cost_v, path; raise ExitBfs)
        else begin
          explored := S.add v !explored;
          (* explore successors *)
          Sequence.iter
            (fun (e, v') ->
              if S.mem v' !explored || ignore v' then ()
              else
                let cost_v' = (cost v e v') + cost_v in
                let path' = (v',e,v) :: path in
                q := HQ.insert (v', cost_v', path') !q)
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
    let goal v' _ = V.compare v' v2 = 0 in
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

  type 'e dot_printer = {
    print_edge : vertex -> 'e -> vertex -> attribute list;
    print_vertex : vertex -> attribute list;
  } (** Dot printer for graphs of type ['e G.t] *)

  (** Create a Dot graph printer. Functions to convert edges and vertices
      to Dot attributes must be provided. *)
  let mk_dot_printer ~print_edge ~print_vertex = {
    print_vertex;
    print_edge;
  }

  (** Pretty print the graph in DOT, on given formatter. Using a sequence
      allows to easily select which edges are important,
      or to combine several graphs with [Sequence.append]. *)
  let pp printer ?(vertices=S.empty) ~name formatter edges =
    (* map from vertices to integers *)
    let get_id =
      let count_map = ref M.empty
      and count = ref 0 in
      fun vertex ->
        try M.find vertex !count_map
        with Not_found ->
          let n = !count in
          incr count;
          count_map := M.add vertex n !count_map;
          n
    (* accumulate vertices *)
    and vertices = ref vertices
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
    (* the name of a vertex *)
    let pp_vertex formatter v = Format.fprintf formatter "vertex_%d" (get_id v) in
    (* print preamble *)
    Format.fprintf formatter "@[<v2>digraph %s {@;" name;
    (* print edges *)
    Sequence.iter
      (fun (v1, e, v2) ->
        (* add v1 and v2 to set of vertices *)
        vertices := S.add v1 (S.add v2 !vertices);
        let attributes = printer.print_edge v1 e v2 in
        Format.fprintf formatter "  @[<h>%a -> %a [%a];@]@."
          pp_vertex v1 pp_vertex v2
          (Sequence.pp_seq ~sep:"," print_attribute) (Sequence.of_list attributes))
      edges;
    (* print vertices *)
    S.iter
      (fun v ->
        let attributes = printer.print_vertex v in
        Format.fprintf formatter "  @[<h>%a [%a];@]@." pp_vertex v
          (Sequence.pp_seq ~sep:"," print_attribute) (Sequence.of_list attributes))
      !vertices;
    (* close *)
    Format.fprintf formatter "}@]@;";
    ()
end
