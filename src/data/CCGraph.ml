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

type 'a sequence = ('a -> unit) -> unit

type 'a sequence_once = 'a sequence

exception Sequence_once

let (|>) x f = f x

module Seq = struct
  type 'a t = 'a sequence
  let return x k = k x
  let (>>=) a f k = a (fun x -> f x k)
  let map f a k = a (fun x -> k (f x))
  let filter_map f a k = a (fun x -> match f x with None -> () | Some y -> k y)
  let iter f a = a f
  let fold f acc a =
    let acc = ref acc in
    a (fun x -> acc := f !acc x);
    !acc
  let to_list seq = fold (fun acc x->x::acc) [] seq |> List.rev
  exception Exit_
  let exists_ f seq =
    try seq (fun x -> if f x then raise Exit_); false
    with Exit_ -> true
end

(** {2 Interfaces for graphs} *)

(** Directed graph with vertices of type ['v] and edges of type [e'] *)
type ('v, 'e) t = {
  children: 'v -> 'e sequence;
  origin: 'e -> 'v;
  dest: 'e -> 'v;
}

type ('v, 'e) graph = ('v, 'e) t

let make ~origin ~dest f = {origin; dest; children=f; }

let make_labelled_tuple f =
  make ~origin:(fun (x,_,_) -> x) ~dest:(fun (_,_,x) -> x)
    (fun v yield -> f v (fun (l,v') -> yield (v,l,v')))

let make_tuple f =
  make ~origin:fst ~dest:snd
    (fun v yield -> f v (fun v' -> yield (v,v')))

(** Mutable bitset for values of type ['v] *)
type 'v tag_set = {
  get_tag: 'v -> bool;
  set_tag: 'v -> unit; (** Set tag for the given element *)
}

(** Mutable table with keys ['k] and values ['a] *)
type ('k, 'a) table = {
  mem: 'k -> bool;
  find: 'k -> 'a;  (** @raise Not_found *)
  add: 'k -> 'a -> unit; (** Erases previous binding *)
}

(** Mutable set *)
type 'a set = ('a, unit) table

let mk_table (type k) ?(eq=(=)) ?(hash=Hashtbl.hash) size =
  let module H = Hashtbl.Make(struct
    type t = k
    let equal = eq
    let hash = hash
  end) in
  let tbl = H.create size in
  { mem=(fun k -> H.mem tbl k)
  ; find=(fun k -> H.find tbl k)
  ; add=(fun k v -> H.replace tbl k v)
  }

let mk_map (type k) ?(cmp=Pervasives.compare) () =
  let module M = Map.Make(struct
    type t = k
    let compare = cmp
  end) in
  let tbl = ref M.empty in
  { mem=(fun k -> M.mem k !tbl)
  ; find=(fun k -> M.find k !tbl)
  ; add=(fun k v -> tbl := M.add k v !tbl)
  }

(** {2 Bags} *)

type 'a bag = {
  push: 'a -> unit;
  is_empty: unit -> bool;
  pop: unit -> 'a;  (** raises some exception is empty *)
}

let mk_queue () =
  let q = Queue.create() in
  { push=(fun x -> Queue.push x q)
  ; is_empty=(fun () -> Queue.is_empty q)
  ; pop=(fun () -> Queue.pop q);
  }

let mk_stack() =
  let s = Stack.create() in
  { push=(fun x -> Stack.push x s)
  ; is_empty=(fun () -> Stack.is_empty s)
  ; pop=(fun () -> Stack.pop s);
  }

(** Implementation from http://en.wikipedia.org/wiki/Skew_heap *)
module Heap = struct
  type 'a t =
    | E
    | N of 'a * 'a t * 'a t

  let is_empty = function
    | E -> true
    | N _ -> false

  let rec union ~leq t1 t2 = match t1, t2 with
  | E, _ -> t2
  | _, E -> t1
  | N (x1, l1, r1), N (x2, l2, r2) ->
    if leq x1 x2
      then N (x1, union ~leq t2 r1, l1)
      else N (x2, union ~leq t1 r2, l2)

  let insert ~leq h x = union ~leq (N (x, E, E)) h

  let pop ~leq h = match h with
    | E -> raise Not_found
    | N (x, l, r) ->
      x, union ~leq l r
end

let mk_heap ~leq =
  let t = ref Heap.E in
  { push=(fun x -> t := Heap.insert ~leq !t x)
  ; is_empty=(fun () -> Heap.is_empty !t)
  ; pop=(fun () ->
        let x, h = Heap.pop ~leq !t in
        t := h;
        x
    )
  }

(** {2 Traversals} *)

module Traverse = struct
  type 'e path = 'e list

  let generic_tag ~tags ~bag ~graph seq =
    let first = ref true in
    fun k ->
      (* ensure linearity *)
      if !first then first := false else raise Sequence_once;
      Seq.iter bag.push seq;
      while not (bag.is_empty ()) do
        let x = bag.pop () in
        if not (tags.get_tag x) then (
          k x;
          tags.set_tag x;
          Seq.iter
            (fun e -> bag.push (graph.dest e))
            (graph.children x)
        )
      done

  let generic ?(tbl=mk_table 128) ~bag ~graph seq =
    let tags = {
      get_tag=tbl.mem;
      set_tag=(fun v -> tbl.add v ());
    } in
    generic_tag ~tags ~bag ~graph seq

  let bfs ?tbl ~graph seq =
    generic ?tbl ~bag:(mk_queue ()) ~graph seq

  let bfs_tag ~tags ~graph seq =
    generic_tag ~tags ~bag:(mk_queue()) ~graph seq

  let dijkstra_tag ?(dist=fun _ -> 1) ~tags ~graph seq =
    let tags' = {
      get_tag=(fun (v,_,_) -> tags.get_tag v);
      set_tag=(fun (v,_,_) -> tags.set_tag v);
    }
    and seq' = Seq.map (fun v -> v, 0, []) seq
    and graph' = {
      children=(fun (v,d,p) -> Seq.map (fun e -> e, d, p) (graph.children v));
      origin=(fun (e, d, p) -> graph.origin e, d, p);
      dest=(fun (e, d, p) -> graph.dest e, d + dist e, e :: p);
    } in
    let bag = mk_heap ~leq:(fun (_,d1,_) (_,d2,_) -> d1 <= d2) in
    generic_tag ~tags:tags' ~bag ~graph:graph' seq'

  let dijkstra ?(tbl=mk_table 128) ?dist ~graph seq =
    let tags = {
      get_tag=tbl.mem;
      set_tag=(fun v -> tbl.add v ());
    } in
    dijkstra_tag ~tags ?dist ~graph seq

  let dfs ?tbl ~graph seq =
    generic ?tbl ~bag:(mk_stack ()) ~graph seq

  let dfs_tag ~tags ~graph seq =
    generic_tag ~tags ~bag:(mk_stack()) ~graph seq

  module Event = struct
    type edge_kind = [`Forward | `Back | `Cross ]

    (** A traversal is a sequence of such events *)
    type ('v,'e) t =
      [ `Enter of 'v * int * 'e path  (* unique index in traversal, path from start *)
      | `Exit of 'v
      | `Edge of 'e * edge_kind
      ]

    let get_vertex = function
      | `Enter (v, _, _) -> Some (v, `Enter)
      | `Exit v -> Some (v, `Exit)
      | `Edge _ -> None

    let get_enter = function
      | `Enter (v, _, _) -> Some v
      | `Exit _
      | `Edge _ -> None

    let get_exit = function
      | `Exit v -> Some v
      | `Enter _
      | `Edge _ -> None

    let get_edge = function
      | `Edge (e, _) -> Some e
      | `Enter _
      | `Exit _ -> None

    let get_edge_kind = function
      | `Edge (e, k) -> Some (e, k)
      | `Enter _
      | `Exit _ -> None

    (* is [v] the origin of some edge in [path]? *)
    let rec list_mem_ ~eq ~graph v path = match path with
      | [] -> false
      | e :: path' ->
        eq v (graph.origin e) || list_mem_ ~eq ~graph v path'

    let dfs_tag ?(eq=(=)) ~tags ~graph seq =
      let first = ref true in
      fun k ->
        if !first then first := false else raise Sequence_once;
        let bag = mk_stack() in
        let n = ref 0 in
        Seq.iter
          (fun v ->
             (* start DFS from this vertex *)
             bag.push (`Enter (v, []));
             while not (bag.is_empty ()) do
               match bag.pop () with
               | `Enter (x, path) ->
                 if not (tags.get_tag x) then (
                   let num = !n in
                   incr n;
                   tags.set_tag x;
                   k (`Enter (x, num, path));
                   bag.push (`Exit x);
                   Seq.iter
                     (fun e -> bag.push (`Edge (e, e :: path)))
                     (graph.children x);
                 )
               | `Exit x -> k (`Exit x)
               | `Edge (e, path) ->
                 let v = graph.dest e in
                 let edge_kind =
                   if tags.get_tag v
                   then if list_mem_ ~eq ~graph v path
                     then `Back
                     else `Cross
                   else (
                     bag.push (`Enter (v, path));
                     `Forward
                   ) in
                 k (`Edge (e, edge_kind))
             done
          ) seq

    let dfs ?(tbl=mk_table 128) ?eq ~graph seq =
      let tags = {
        set_tag=(fun v -> tbl.add v ());
        get_tag=tbl.mem;
      } in
      dfs_tag ?eq ~tags ~graph seq
  end
end

(** {2 Cycles} *)

let is_dag ?(tbl=mk_table 128) ~graph vs =
  Traverse.Event.dfs ~tbl ~graph vs
  |> Seq.exists_
    (function
      | `Edge (_, `Back) -> true
      | _ -> false)

(** {2 Topological Sort} *)

exception Has_cycle

let topo_sort_tag ?(eq=(=)) ?(rev=false) ~tags ~graph seq =
  (* use DFS *)
  let l =
    Traverse.Event.dfs_tag ~eq ~tags ~graph seq
    |> Seq.filter_map
        (function
          | `Exit v -> Some v
          | `Edge (_, `Back) -> raise Has_cycle
          | `Enter _
          | `Edge _ -> None
        )
    |> Seq.fold (fun acc x -> x::acc) []
  in
  if rev then List.rev l else l

let topo_sort ?eq ?rev ?(tbl=mk_table 128) ~graph seq =
  let tags = {
    get_tag=tbl.mem;
    set_tag=(fun v -> tbl.add v ());
  } in
  topo_sort_tag ?eq ?rev ~tags ~graph seq

(*$T
  let l = topo_sort ~graph:divisors_graph (Seq.return 42) in \
  List.for_all (fun (i,j) -> \
    let idx_i = CCList.find_idx ((=)i) l |> CCOpt.get_exn |> fst in \
    let idx_j = CCList.find_idx ((=)j) l |> CCOpt.get_exn |> fst in \
    idx_i < idx_j) \
    [ 42, 21; 14, 2; 3, 1; 21, 7; 42, 3]
  let l = topo_sort ~rev:true ~graph:divisors_graph (Seq.return 42) in \
  List.for_all (fun (i,j) -> \
    let idx_i = CCList.find_idx ((=)i) l |> CCOpt.get_exn |> fst in \
    let idx_j = CCList.find_idx ((=)j) l |> CCOpt.get_exn |> fst in \
    idx_i > idx_j) \
    [ 42, 21; 14, 2; 3, 1; 21, 7; 42, 3]
*)

(** {2 Lazy Spanning Tree} *)

module LazyTree = struct
  type ('v, 'e) t =
    | Vertex of 'v * ('e * ('v, 'e) t) list Lazy.t

  let rec map_v f (Vertex (v, l)) =
    let l' = lazy (List.map (fun (e, child) -> e, map_v f child) (Lazy.force l)) in
    Vertex (f v, l')

  let rec fold_v f acc t = match t with
    | Vertex (v, l) ->
      let acc = f acc v in
      List.fold_left
        (fun acc (_, t') -> fold_v f acc t')
        acc
        (Lazy.force l)
end

let spanning_tree_tag ~tags ~graph v =
  let rec mk_node v =
    let children = lazy (
      Seq.fold
        (fun acc e ->
           let v' = graph.dest e in
           if tags.get_tag v'
           then acc
           else (
             tags.set_tag v';
             (e, mk_node v') :: acc
           )
        ) [] (graph.children v)
      )
    in
    LazyTree.Vertex (v, children)
  in
  mk_node v

let spanning_tree ?(tbl=mk_table 128) ~graph v =
  let tags = {
    get_tag=tbl.mem;
    set_tag=(fun v -> tbl.add v ());
  } in
  spanning_tree_tag ~tags ~graph v

(** {2 Strongly Connected Components} *)

module SCC = struct
  type 'v state = {
    mutable min_id: int; (* min ID of the vertex' scc *)
    id: int;  (* ID of the vertex *)
    mutable on_stack: bool;
    mutable vertex: 'v;
  }

  let mk_cell v n = {
    min_id=n;
    id=n;
    on_stack=false;
    vertex=v;
  }

  (* pop elements of [stack] until we reach node with given [id] *)
  let rec pop_down_to ~id acc stack =
    assert (not(Stack.is_empty stack));
    let cell = Stack.pop stack in
    cell.on_stack <- false;
    if cell.id = id then (
      assert (cell.id = cell.min_id);
      cell.vertex :: acc (* return SCC *)
    ) else pop_down_to ~id (cell.vertex::acc) stack

  let explore ~tbl ~graph seq =
    let first = ref true in
    fun k ->
      if !first then first := false else raise Sequence_once;
      (* stack of nodes being explored, for the DFS *)
      let to_explore = Stack.create() in
      (* stack for Tarjan's algorithm itself *)
      let stack = Stack.create () in
      (* unique ID *)
      let n = ref 0 in
      (* exploration *)
      Seq.iter
        (fun v ->
           Stack.push (`Enter v) to_explore;
           while not (Stack.is_empty to_explore) do
             match Stack.pop to_explore with
             | `Enter v ->
               if not (tbl.mem v) then (
                 (* remember unique ID for [v] *)
                 let id = !n in
                 incr n;
                 let cell = mk_cell v id in
                 cell.on_stack <- true;
                 tbl.add v cell;
                 Stack.push cell stack;
                 Stack.push (`Exit (v, cell)) to_explore;
                 (* explore children *)
                 Seq.iter
                   (fun e -> Stack.push (`Enter (graph.dest e)) to_explore)
                   (graph.children v)
               )
             | `Exit (v, cell) ->
               (* update [min_id] *)
               assert cell.on_stack;
               Seq.iter
                 (fun e ->
                    let dest = graph.dest e in
                    (* must not fail, [dest] already explored *)
                    let dest_cell = tbl.find dest in
                    (* same SCC? yes if [dest] points to [cell.v] *)
                    if dest_cell.on_stack
                      then cell.min_id <- min cell.min_id dest_cell.min_id
                 ) (graph.children v);
               (* pop from stack if SCC found *)
               if cell.id = cell.min_id then (
                 let scc = pop_down_to ~id:cell.id [] stack in
                 k scc
               )
           done
        ) seq;
      assert (Stack.is_empty stack);
      ()
end

type 'v scc_state = 'v SCC.state

let scc ?(tbl=mk_table 128) ~graph seq = SCC.explore ~tbl ~graph seq

(* example from https://en.wikipedia.org/wiki/Strongly_connected_component *)
(*$R
  let set_eq ?(eq=(=)) l1 l2 = CCList.Set.subset ~eq l1 l2 && CCList.Set.subset ~eq l2 l1 in
  let graph = of_list
    [ "a", "b"
    ; "b", "e"
    ; "e", "a"
    ; "b", "f"
    ; "e", "f"
    ; "f", "g"
    ; "g", "f"
    ; "b", "c"
    ; "c", "g"
    ; "c", "d"
    ; "d", "c"
    ; "d", "h"
    ; "h", "d"
    ; "h", "g"
  ] in
  let res = scc ~graph (Seq.return "a") |> Seq.to_list in
  assert_bool "scc"
    (set_eq ~eq:(set_eq ?eq:None) res
      [ [ "a"; "b"; "e" ]
      ; [ "f"; "g" ]
      ; [ "c"; "d"; "h" ]
      ]
    )
*)

(** {2 Pretty printing in the DOT (graphviz) format} *)

module Dot = struct
  type attribute = [
  | `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  let pp_list pp_x out l =
    Format.pp_print_string out "[";
    List.iteri
      (fun i x ->
        if i > 0 then Format.fprintf out ",@;";
        pp_x out x)
      l;
    Format.pp_print_string out "]"

  type vertex_state = {
    mutable explored : bool;
    id : int;
  }

  (** Print an enum of Full.traverse_event *)
  let pp_seq
      ?(tbl=mk_table 128)
      ?(eq=(=))
      ?(attrs_v=fun _ -> [])
      ?(attrs_e=fun _ -> [])
      ?(name="graph")
      ~graph out seq =
    (* print an attribute *)
    let pp_attr out attr = match attr with
      | `Color c -> Format.fprintf out "color=%s" c
      | `Shape s -> Format.fprintf out "shape=%s" s
      | `Weight w -> Format.fprintf out "weight=%d" w
      | `Style s -> Format.fprintf out "style=%s" s
      | `Label l -> Format.fprintf out "label=\"%s\"" l
      | `Other (name, value) -> Format.fprintf out "%s=\"%s\"" name value
    (* map from vertices to integers *)
    and get_node =
      let count = ref 0 in
      fun v ->
        try tbl.find v
        with Not_found ->
          let node = {id= !count; explored=false} in
          incr count;
          tbl.add v node;
          node
    and vertex_explored v =
      try (tbl.find v).explored
      with Not_found -> false
    in
    let set_explored v = (get_node v).explored <- true
    and get_id v = (get_node v).id in
    (* the unique name of a vertex *)
    let pp_vertex out v = Format.fprintf out "vertex_%d" (get_id v) in
    (* print preamble *)
    Format.fprintf out "@[<v2>digraph \"%s\" {@;" name;
    (* traverse *)
    let tags = {
      get_tag=vertex_explored;
      set_tag=set_explored; (* allocate new ID *)
    } in
    let events = Traverse.Event.dfs_tag ~eq ~tags ~graph seq in
    Seq.iter
      (function
        | `Enter (v, _n, _path) ->
          let attrs = attrs_v v in
          Format.fprintf out "@[<h>%a %a;@]@," pp_vertex v (pp_list pp_attr) attrs
        | `Exit _ -> ()
        | `Edge (e, _) ->
          let v1 = graph.origin e in
          let v2 = graph.dest e in
          let attrs = attrs_e e in
          Format.fprintf out "@[<h>%a -> %a %a;@]@,"
            pp_vertex v1 pp_vertex v2
            (pp_list pp_attr)
            attrs
      ) events;
    (* close *)
    Format.fprintf out "}@]@;@?";
    ()

  let pp ?tbl ?eq ?attrs_v ?attrs_e ?name ~graph fmt v =
    pp_seq ?tbl ?eq ?attrs_v ?attrs_e ?name ~graph fmt (Seq.return v)

  let with_out filename f =
    let oc = open_out filename in
    try
      let fmt = Format.formatter_of_out_channel oc in
      let x = f fmt in
      Format.pp_print_flush fmt ();
      close_out oc;
      x
    with e ->
      close_out oc;
      raise e
end

(** {2 Mutable Graph} *)

type ('v, 'e) mut_graph = <
  graph: ('v, 'e) t;
  add_edge: 'e -> unit;
  remove : 'v -> unit;
>

let mk_mut_tbl (type k) ?(eq=(=)) ?(hash=Hashtbl.hash) size =
  let module Tbl = Hashtbl.Make(struct
    type t = k
    let hash = hash
    let equal = eq
  end) in
  let tbl = Tbl.create size in
  object
    method graph = {
      origin=(fun (x,_,_) -> x);
      dest=(fun (_,_,x) -> x);
      children=(fun v k ->
          try List.iter k (Tbl.find tbl v)
         with Not_found -> ()
        );
    }
    method add_edge (v1,e,v2) =
      let l = try Tbl.find tbl v1 with Not_found -> [] in
      Tbl.replace tbl v1 ((v1,e,v2)::l)
    method remove v = Tbl.remove tbl v
  end

(** {2 Immutable Graph} *)

module type MAP = sig
  type vertex
  type t

  val as_graph : t -> (vertex, (vertex * vertex)) graph
  (** Graph view of the map *)

  val empty : t

  val add_edge : vertex -> vertex -> t -> t

  val remove_edge : vertex -> vertex -> t -> t

  val add : vertex -> t -> t
  (** Add a vertex, possibly with no outgoing edge *)

  val remove : vertex -> t -> t
  (** Remove the vertex and all its outgoing edges.
      Edges that point to the vertex are {b NOT} removed, they must be
      manually removed with {!remove_edge} *)

  val union : t -> t -> t

  val vertices : t -> vertex sequence

  val vertices_l : t -> vertex list

  val of_list : (vertex * vertex) list -> t

  val add_list : (vertex * vertex) list -> t -> t

  val to_list : t -> (vertex * vertex) list

  val of_seq : (vertex * vertex) sequence -> t

  val add_seq : (vertex * vertex) sequence -> t -> t

  val to_seq : t -> (vertex * vertex) sequence
end

module Map(O : Map.OrderedType) = struct
  module M = Map.Make(O)
  module S = Set.Make(O)

  type vertex = O.t
  type t = {
    edges: S.t M.t;
    vertices: S.t;
  }

  let as_graph m = {
    origin=fst;
    dest=snd;
    children=(fun v yield ->
      try
        let set = M.find v m.edges in
        S.iter (fun v' -> yield (v, v')) set
      with Not_found -> ()
      );
  }

  let empty = {edges=M.empty; vertices=S.empty}

  let add_edge v1 v2 m =
    let set = try M.find v1 m.edges with Not_found -> S.empty in
    let edges = M.add v1 (S.add v2 set) m.edges in
    let vertices = S.add v1 (S.add v2 m.vertices) in
    { edges; vertices; }

  let remove_edge v1 v2 m =
    try
      let set = S.remove v2 (M.find v1 m.edges) in
      if S.is_empty set
      then {m with edges=M.remove v1 m.edges}
      else {m with edges=M.add v1 set m.edges}
    with Not_found -> m

  let add v m = { m with vertices=S.add v m.vertices }

  let remove v m =
    { edges=M.remove v m.edges; vertices=S.remove v m.vertices }

  let union m1 m2 =
    {edges=M.merge
      (fun _ s1 s2 -> match s1, s2 with
         | Some s, None
         | None, Some s -> Some s
         | None, None -> assert false
         | Some s1, Some s2 -> Some (S.union s1 s2)
      ) m1.edges m2.edges;
     vertices=S.union m1.vertices m2.vertices
    }

  let vertices m yield = S.iter yield m.vertices

  let vertices_l m = S.fold (fun v acc -> v::acc) m.vertices []

  let add_list l m = List.fold_left (fun m (v1,v2) -> add_edge v1 v2 m) m l

  let of_list l = add_list l empty

  let to_list m =
    M.fold
      (fun v set acc -> S.fold (fun v' acc -> (v,v')::acc) set acc)
      m.edges []

  let add_seq seq m = Seq.fold (fun m (v1,v2) -> add_edge v1 v2 m) m seq

  let of_seq seq = add_seq seq empty

  let to_seq m k = M.iter (fun v set -> S.iter (fun v' -> k(v,v')) set) m.edges
end

(** {2 Misc} *)

let of_list ?(eq=(=)) l = {
  origin=fst;
  dest=snd;
  children=(fun v yield -> List.iter (fun (a,b) -> if eq a v then yield (a,b)) l)
}

let of_fun f = {
  origin=fst;
  dest=snd;
  children=(fun v yield ->
    let l = f v in
    List.iter (fun v' -> yield (v,v')) l
  );
}

let of_hashtbl tbl = {
  origin=fst;
  dest=snd;
  children=(fun v yield ->
    try List.iter (fun b -> yield (v, b)) (Hashtbl.find tbl v)
    with Not_found -> ()
  )
}

let divisors_graph = {
  origin=fst;
  dest=snd;
  children=(fun i ->
    (* divisors of [i] that are [>= j] *)
    let rec divisors j i yield =
      if j < i
      then (
        if (i mod j = 0) then yield (i,j);
        divisors (j+1) i yield
      )
    in
    divisors 1 i
  );
}
