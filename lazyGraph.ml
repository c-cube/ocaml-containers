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
  | Node of 'id * 'v * ('e * 'id) Enum.t
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
    if eq v v' then Node (v, label, Enum.empty) else Empty in
  { force; eq; hash; }

let make ?(eq=(=)) ?(hash=Hashtbl.hash) force =
  { eq; hash; force; }

let from_enum ?(eq=(=)) ?(hash=Hashtbl.hash) ~vertices ~edges =
  failwith "from_enum: not implemented"

let from_fun ?(eq=(=)) ?(hash=Hashtbl.hash) f =
  let force v =
    match f v with
    | None -> Empty
    | Some (l, edges) -> Node (v, l, Enum.of_list edges) in
  { eq; hash; force; }

(** {2 Polymorphic utils} *)

(** A set of vertices *)
type 'id set =
  <
    mem : 'id -> bool;
    add : 'id -> unit;
    iter : ('id -> unit) -> unit;
  >

(** Make a set based on hashtables *)
let mk_hset (type id) ?(eq=(=)) ~hash =
  let module H = Hashtbl.Make(struct type t = id let equal = eq let hash = hash end) in
  let set = H.create 5 in
  object
    method mem x = H.mem set x
    method add x = H.replace set x ()
    method iter f = H.iter (fun x () -> f x) set
  end

(** Make a set based on balanced trees *)
let mk_tset (type id) ~cmp =
  let module S = Set.Make(struct type t = id let compare = cmp end) in
  let set = ref S.empty in
  object
    method mem x = S.mem x !set
    method add x = set := S.add x !set
    method iter f = S.iter f !set
  end

type ('id,'a) map =
  <
    mem : 'id -> bool;
    get : 'id -> 'a;   (* or Not_found *)
    add : 'id -> 'a -> unit;
    iter : ('id -> 'a -> unit) -> unit;
  >

(** Make a map based on hashtables *)
let mk_hmap (type id) ?(eq=(=)) ~hash =
  let module H = Hashtbl.Make(struct type t = id let equal = eq let hash = hash end) in
  let m = H.create 5 in
  object
    method mem k = H.mem m k
    method add k v = H.replace m k v
    method get k = H.find m k
    method iter f = H.iter f m
  end

(** Make a map based on balanced trees *)
let mk_tmap (type id) ~cmp =
  let module M = Map.Make(struct type t = id let compare = cmp end) in
  let m = ref M.empty in
  object
    method mem k = M.mem k !m
    method add k v = m := M.add k v !m
    method get k = M.find k !m
    method iter f = M.iter f !m
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

  let bfs_full ?(id=0) ?explored graph vertices =
    let explored = match explored with
      | Some e -> e 
      | None -> fun () -> mk_hset ~eq:graph.eq ~hash:graph.hash in
    fun () ->
      let explored = explored () in
      let id = ref id in
      let q = Queue.create () in (* queue of nodes to explore *)
      Enum.iter (fun v -> Queue.push (FullEnter (v,[])) q) vertices;
      let rec next () =
        if Queue.is_empty q then raise Enum.EOG else
          match Queue.pop q with
          | FullEnter (v', path) ->
            if explored#mem v' then next ()
              else begin match graph.force v' with
              | Empty -> next ()
              | Node (_, label, edges) ->
                explored#add v';
                (* explore neighbors *)
                Enum.iter
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
            if explored#mem v''
              then if mem_path ~eq:graph.eq path v''
                then MeetEdge (v'', e, v', EdgeBackward)
                else MeetEdge (v'', e, v', EdgeTransverse)
              else begin
                (* explore this edge *)
                Queue.push (FullEnter (v'', path')) q;
                MeetEdge (v'', e, v', EdgeForward)
              end
      in next

  let dfs_full ?(id=0) ?explored graph vertices =
    let explored = match explored with
      | Some e -> e 
      | None -> (fun () -> mk_hset ~eq:graph.eq ~hash:graph.hash) in
    fun () -> 
      let explored = explored () in
      let id = ref id in
      let s = Stack.create () in (* stack of nodes to explore *)
      Enum.iter (fun v -> Stack.push (FullEnter (v,[])) s) vertices;
      let rec next () =
        if Stack.is_empty s then raise Enum.EOG else
          match Stack.pop s with
          | FullExit v' -> ExitVertex v'
          | FullEnter (v', path) ->
            if explored#mem v' then next ()
              (* explore the node now *)
              else begin match graph.force v' with
              | Empty -> next ()
              | Node (_, label, edges) ->
                explored#add v';
                (* prepare to exit later *)
                Stack.push (FullExit v') s;
                (* explore neighbors *)
                Enum.iter
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
            if explored#mem v''
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

let bfs ?id ?explored graph v =
  Enum.filterMap
    (function
      | Full.EnterVertex (v, l, i, _) -> Some (v, l, i)
      | _ -> None)
    (Full.bfs_full ?id ?explored graph (Enum.singleton v))

let dfs ?id ?explored graph v =
  Enum.filterMap
    (function
      | Full.EnterVertex (v, l, i, _) -> Some (v, l, i)
      | _ -> None)
    (Full.dfs_full ?id ?explored graph (Enum.singleton v))

let enum graph v = (Enum.empty, Enum.empty)  (* TODO *)

let depth graph v =
  failwith "not implemented" (* TODO *)

(** Minimal path from the given Graph from the first vertex to
    the second. It returns both the distance and the path *)
let min_path ?(distance=fun v1 e v2 -> 1) ?explored graph v1 v2 =
  failwith "not implemented"

(** {2 Lazy transformations} *)

let union ?(combine=fun x y -> x) g1 g2 =
  let force v =
    match g1.force v, g2.force v with
    | Empty, Empty -> Empty
    | ((Node _) as n), Empty -> n
    | Empty, ((Node _) as n) -> n
    | Node (_, l1, e1), Node (_, l2, e2) ->
      Node (v, combine l1 l2, Enum.append e1 e2)
  in { eq=g1.eq; hash=g1.hash; force; }

let map ~vertices ~edges g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) ->
      let edges_enum' = Enum.map (fun (e,v') -> (edges e), v') edges_enum in
      Node (v, vertices l, edges_enum')
  in { eq=g.eq; hash=g.hash; force; }

let filter ?(vertices=(fun v l -> true)) ?(edges=fun v1 e v2 -> true) g =
  let force v =
    match g.force v with
    | Empty -> Empty
    | Node (_, l, edges_enum) when vertices v l ->
      (* filter out edges *)
      let edges_enum' = Enum.filter (fun (e,v') -> edges v e v') edges_enum in
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
      let edges = Enum.product edges1 edges2 in
      let edges = Enum.map (fun ((e1,v1'),(e2,v2')) -> ((e1,e2),(v1',v2'))) edges in
      Node ((v1,v2), (l1,l2), edges)
  and eq (v1,v2) (v1',v2') =
    g1.eq v1 v1' && g2.eq v2 v2'
  and hash (v1,v2) = ((g1.hash v1) * 65599) + g2.hash v2 
  in
  { eq; hash; force; }

let limit_depth ~max g =
  (* TODO; this should be eager (compute depth by BFS) *)
  failwith "not implemented"

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
      let m = mk_hmap ~eq ~hash in
      fun vertex ->
        try m#get vertex
        with Not_found ->
          let n = !count in
          incr count;
          m#add vertex n;
          n
    in
    (* the unique name of a vertex *)
    let pp_vertex formatter v =
      Format.fprintf formatter "vertex_%d" (get_id v) in
    (* print preamble *)
    Format.fprintf formatter "@[<v2>digraph %s {@;" name;
    (* traverse *)
    Enum.iter
      (function
        | Full.EnterVertex (v, attrs, _, _) ->
          Format.fprintf formatter "  @[<h>%a [%a];@]@." pp_vertex v
            (Enum.pp ~sep:"," print_attribute) (Enum.of_list attrs)
        | Full.ExitVertex _ -> ()
        | Full.MeetEdge (v2, attrs, v1, _) ->
          Format.fprintf formatter "  @[<h>%a -> %a [%a];@]@."
            pp_vertex v1 pp_vertex v2
            (Enum.pp ~sep:"," print_attribute)
            (Enum.of_list attrs))
      events;
    (* close *)
    Format.fprintf formatter "}@]@;@?";
    ()

  let pp ~name graph formatter vertices =
    let enum = Full.bfs_full graph vertices in
    pp_enum ~eq:graph.eq ~hash:graph.hash ~name formatter enum
end
