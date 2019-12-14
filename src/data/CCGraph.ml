
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Simple Graph Interface} *)

(** {2 Iter Helpers} *)

type 'a iter = ('a -> unit) -> unit
(** A sequence of items of type ['a], possibly infinite
    @since 2.8 *)

type 'a iter_once = 'a iter
(** Iter that should be used only once
    @since 2.8 *)

type 'a sequence = ('a -> unit) -> unit
(** A sequence of items of type ['a], possibly infinite
    @deprecate see {!iter} instead *)
[@@ocaml.deprecated "see iter"]

type 'a sequence_once = 'a iter
(** Iter that should be used only once
    @deprecate see {!iter_once} instead *)
[@@ocaml.deprecated "see iter_once"]

exception Iter_once

let (|>) x f = f x

module Iter = struct
  type 'a t = 'a iter
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

module Seq = Iter

(** {2 Interfaces for graphs} *)

(** Directed graph with vertices of type ['v] and edges labeled with [e'] *)
type ('v, 'e) t = ('v -> ('e * 'v) iter)

type ('v, 'e) graph = ('v, 'e) t

let make (f:'v->('e*'v) iter): ('v, 'e) t = f

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

let mk_table (type k) ~eq ?(hash=Hashtbl.hash) size =
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

let mk_map (type k) ~cmp () =
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
  type ('v, 'e) path = ('v * 'e * 'v) list

  let generic_tag ~tags ~bag ~graph iter =
    let first = ref true in
    fun k ->
      (* ensure linearity *)
      if !first then first := false else raise Iter_once;
      Iter.iter bag.push iter;
      while not (bag.is_empty ()) do
        let x = bag.pop () in
        if not (tags.get_tag x) then (
          k x;
          tags.set_tag x;
          Iter.iter
            (fun (_,dest) -> bag.push dest)
            (graph x)
        )
      done

  let generic ~tbl ~bag ~graph iter =
    let tags = {
      get_tag=tbl.mem;
      set_tag=(fun v -> tbl.add v ());
    } in
    generic_tag ~tags ~bag ~graph iter

  let bfs ~tbl ~graph iter =
    generic ~tbl ~bag:(mk_queue ()) ~graph iter

  let bfs_tag ~tags ~graph iter =
    generic_tag ~tags ~bag:(mk_queue()) ~graph iter

  let dijkstra_tag ?(dist=fun _ -> 1) ~tags ~graph iter =
    let tags' = {
      get_tag=(fun (v,_,_) -> tags.get_tag v);
      set_tag=(fun (v,_,_) -> tags.set_tag v);
    }
    and iter' = Iter.map (fun v -> v, 0, []) iter
    and graph' (v,d,p) =
      graph v
      |> Iter.map (fun (e,v') -> e, (v',d+dist e, (v,e,v')::p))
    in
    let bag = mk_heap ~leq:(fun (_,d1,_) (_,d2,_) -> d1 <= d2) in
    generic_tag ~tags:tags' ~bag ~graph:graph' iter'

  let dijkstra ~tbl ?dist ~graph iter =
    let tags = {
      get_tag=tbl.mem;
      set_tag=(fun v -> tbl.add v ());
    } in
    dijkstra_tag ~tags ?dist ~graph iter

  let dfs ~tbl ~graph iter =
    generic ~tbl ~bag:(mk_stack ()) ~graph iter

  let dfs_tag ~tags ~graph iter =
    generic_tag ~tags ~bag:(mk_stack()) ~graph iter

  module Event = struct
    type edge_kind = [`Forward | `Back | `Cross ]

    (** A traversal is a iteruence of such events *)
    type ('v,'e) t =
      [ `Enter of 'v * int * ('v,'e) path  (* unique index in traversal, path from start *)
      | `Exit of 'v
      | `Edge of 'v * 'e * 'v * edge_kind
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
      | `Edge (v1,e,v2,_) -> Some (v1,e,v2)
      | `Enter _
      | `Exit _ -> None

    let get_edge_kind = function
      | `Edge (v,e,v',k) -> Some (v,e,v',k)
      | `Enter _
      | `Exit _ -> None

    (* is [v] the origin of some edge in [path]? *)
    let rec list_mem_ ~eq ~graph v path = match path with
      | [] -> false
      | (v1,_,_) :: path' ->
        eq v v1 || list_mem_ ~eq ~graph v path'

    let dfs_tag ~eq ~tags ~graph iter =
      let first = ref true in
      fun k ->
        if !first then first := false else raise Iter_once;
        let bag = mk_stack() in
        let n = ref 0 in
        Iter.iter
          (fun v ->
             (* start DFS from this vertex *)
             bag.push (`Enter (v, []));
             while not (bag.is_empty ()) do
               match bag.pop () with
                 | `Enter (v, path) ->
                   if not (tags.get_tag v) then (
                     let num = !n in
                     incr n;
                     tags.set_tag v;
                     k (`Enter (v, num, path));
                     bag.push (`Exit v);
                     Iter.iter
                       (fun (e,v') -> bag.push (`Edge (v,e,v',(v,e,v') :: path)))
                       (graph v);
                   )
                 | `Exit x -> k (`Exit x)
                 | `Edge (v,e,v', path) ->
                   let edge_kind =
                     if tags.get_tag v'
                     then if list_mem_ ~eq ~graph v' path
                       then `Back
                       else `Cross
                     else (
                       bag.push (`Enter (v', path));
                       `Forward
                     )
                   in
                   k (`Edge (v,e,v', edge_kind))
             done
          ) iter

    let dfs ~tbl ~eq ~graph iter =
      let tags = {
        set_tag=(fun v -> tbl.add v ());
        get_tag=tbl.mem;
      } in
      dfs_tag ~eq ~tags ~graph iter
  end

  (*$R
    let l =
      let tbl = mk_table ~eq:CCInt.equal 128 in
      Traverse.Event.dfs ~tbl ~eq:CCInt.equal ~graph:divisors_graph (Iter.return 345614)
      |> Iter.to_list in
    let expected =
    [`Enter (345614, 0, []); `Edge (345614, (), 172807, `Forward);
     `Enter (172807, 1, [(345614, (), 172807)]); `Edge (172807, (), 1, `Forward);
     `Enter (1, 2, [(172807, (), 1); (345614, (), 172807)]); `Exit 1; `Exit 172807;
     `Edge (345614, (), 2, `Forward); `Enter (2, 3, [(345614, (), 2)]);
     `Edge (2, (), 1, `Cross); `Exit 2; `Edge (345614, (), 1, `Cross);
     `Exit 345614]
    in
    assert_equal expected l
  *)
end

(** {2 Cycles} *)

let is_dag ~tbl ~eq ~graph vs =
  Traverse.Event.dfs ~tbl ~eq ~graph vs
  |> Iter.exists_
    (function
      | `Edge (_, _, _, `Back) -> true
      | _ -> false)

(** {2 Topological Sort} *)

exception Has_cycle

let topo_sort_tag ~eq ?(rev=false) ~tags ~graph iter =
  (* use DFS *)
  let l =
    Traverse.Event.dfs_tag ~eq ~tags ~graph iter
    |> Iter.filter_map
      (function
        | `Exit v -> Some v
        | `Edge (_, _, _, `Back) -> raise Has_cycle
        | `Enter _
        | `Edge _ -> None
      )
    |> Iter.fold (fun acc x -> x::acc) []
  in
  if rev then List.rev l else l

let topo_sort ~eq ?rev ~tbl ~graph iter =
  let tags = {
    get_tag=tbl.mem;
    set_tag=(fun v -> tbl.add v ());
  } in
  topo_sort_tag ~eq ?rev ~tags ~graph iter

(*$T
  let tbl = mk_table ~eq:CCInt.equal 128 in \
  let l = topo_sort ~eq:CCInt.equal ~tbl ~graph:divisors_graph (Iter.return 42) in \
  List.for_all (fun (i,j) -> \
    let idx_i = CCList.find_idx ((=)i) l |> CCOpt.get_exn |> fst in \
    let idx_j = CCList.find_idx ((=)j) l |> CCOpt.get_exn |> fst in \
    idx_i < idx_j) \
    [ 42, 21; 14, 2; 3, 1; 21, 7; 42, 3]
  let tbl = mk_table ~eq:CCInt.equal 128 in \
  let l = topo_sort ~eq:CCInt.equal ~rev:true ~tbl ~graph:divisors_graph (Iter.return 42) in \
  List.for_all (fun (i,j) -> \
    let idx_i = CCList.find_idx ((=)i) l |> CCOpt.get_exn |> fst in \
    let idx_j = CCList.find_idx ((=)j) l |> CCOpt.get_exn |> fst in \
    idx_i > idx_j) \
    [ 42, 21; 14, 2; 3, 1; 21, 7; 42, 3]
*)

(** {2 Lazy Spanning Tree} *)

module Lazy_tree = struct
  type ('v, 'e) t = {
    vertex: 'v;
    children: ('e * ('v, 'e) t) list Lazy.t;
  }

  let make_ vertex children = {vertex; children}

  let rec map_v f {vertex=v; children=l} =
    let l' = lazy (List.map (fun (e, child) -> e, map_v f child) (Lazy.force l)) in
    make_ (f v) l'

  let rec fold_v f acc {vertex=v; children=l} =
    let acc = f acc v in
    List.fold_left
      (fun acc (_, t') -> fold_v f acc t')
      acc
      (Lazy.force l)
end

let spanning_tree_tag ~tags ~graph v =
  let rec mk_node v =
    let children = lazy (
      Iter.fold
        (fun acc (e,v') ->
           if tags.get_tag v'
           then acc
           else (
             tags.set_tag v';
             (e, mk_node v') :: acc
           )
        ) [] (graph v)
    )
    in
    Lazy_tree.make_ v children
  in
  mk_node v

let spanning_tree ~tbl ~graph v =
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

  let explore ~tbl ~graph iter =
    let first = ref true in
    fun k ->
      if !first then first := false else raise Iter_once;
      (* stack of nodes being explored, for the DFS *)
      let to_explore = Stack.create() in
      (* stack for Tarjan's algorithm itself *)
      let stack = Stack.create () in
      (* unique ID *)
      let n = ref 0 in
      (* exploration *)
      Iter.iter
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
                   Iter.iter
                     (fun (_,v') -> Stack.push (`Enter v') to_explore)
                     (graph v)
                 )
               | `Exit (v, cell) ->
                 (* update [min_id] *)
                 assert cell.on_stack;
                 Iter.iter
                   (fun (_,dest) ->
                      (* must not fail, [dest] already explored *)
                      let dest_cell = tbl.find dest in
                      (* same SCC? yes if [dest] points to [cell.v] *)
                      if dest_cell.on_stack
                      then cell.min_id <- min cell.min_id dest_cell.min_id
                   ) (graph v);
                 (* pop from stack if SCC found *)
                 if cell.id = cell.min_id then (
                   let scc = pop_down_to ~id:cell.id [] stack in
                   k scc
                 )
           done
        ) iter;
      assert (Stack.is_empty stack);
      ()
end

type 'v scc_state = 'v SCC.state

let scc ~tbl ~graph iter = SCC.explore ~tbl ~graph iter

(* example from https://en.wikipedia.org/wiki/Strongly_connected_component *)
(*$R
  let set_eq ?(eq=(=)) l1 l2 = CCList.subset ~eq l1 l2 && CCList.subset ~eq l2 l1 in
  let graph = of_list ~eq:CCString.equal
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
  let tbl = mk_table ~eq:CCString.equal 128 in
  let res = scc ~tbl ~graph (Iter.return "a") |> Iter.to_list in
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
  let pp_all
      ~tbl
      ~eq
      ?(attrs_v=fun _ -> [])
      ?(attrs_e=fun _ -> [])
      ?(name="graph")
      ~graph out iter =
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
    let events = Traverse.Event.dfs_tag ~eq ~tags ~graph iter in
    Iter.iter
      (function
        | `Enter (v, _n, _path) ->
          let attrs = attrs_v v in
          Format.fprintf out "@[<h>%a %a;@]@," pp_vertex v (pp_list pp_attr) attrs
        | `Exit _ -> ()
        | `Edge (v1,e,v2,_) ->
          let attrs = attrs_e e in
          Format.fprintf out "@[<h>%a -> %a %a;@]@,"
            pp_vertex v1 pp_vertex v2
            (pp_list pp_attr)
            attrs
      ) events;
    (* close *)
    Format.fprintf out "}@]@;@?";
    ()

  let pp_seq = pp_all

  let pp ~tbl ~eq ?attrs_v ?attrs_e ?name ~graph fmt v =
    pp_all ~tbl ~eq ?attrs_v ?attrs_e ?name ~graph fmt (Iter.return v)

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

type ('v, 'e) mut_graph = {
  graph: ('v, 'e) t;
  add_edge: 'v -> 'e -> 'v -> unit;
  remove : 'v -> unit;
}

let mk_mut_tbl (type k) ~eq ?(hash=Hashtbl.hash) size =
  let module Tbl = Hashtbl.Make(struct
      type t = k
      let hash = hash
      let equal = eq
    end) in
  let tbl = Tbl.create size in
  {
    graph=(fun v yield ->
      try List.iter yield (Tbl.find tbl v)
      with Not_found -> ()
    );
    add_edge=(fun v1 e v2 ->
      let l = try Tbl.find tbl v1 with Not_found -> [] in
      Tbl.replace tbl v1 ((e,v2)::l)
    );
    remove = (fun v -> Tbl.remove tbl v);
  }

(** {2 Immutable Graph} *)

module type MAP = sig
  type vertex
  type 'a t

  val as_graph : 'a t -> (vertex, 'a) graph
  (** Graph view of the map. *)

  val empty : 'a t

  val add_edge : vertex -> 'a -> vertex -> 'a t -> 'a t

  val remove_edge : vertex -> vertex -> 'a t -> 'a t

  val add : vertex -> 'a t -> 'a t
  (** Add a vertex, possibly with no outgoing edge. *)

  val remove : vertex -> 'a t -> 'a t
  (** Remove the vertex and all its outgoing edges.
      Edges that point to the vertex are {b NOT} removed, they must be
      manually removed with {!remove_edge}. *)

  val union : 'a t -> 'a t -> 'a t

  val vertices : _ t -> vertex iter

  val vertices_l : _ t -> vertex list

  val of_list : (vertex * 'a * vertex) list -> 'a t

  val add_list : (vertex * 'a * vertex) list -> 'a t -> 'a t

  val to_list : 'a t -> (vertex * 'a * vertex) list

  val of_iter : (vertex * 'a * vertex) iter -> 'a t
  (** @since 2.8 *)

  val add_iter : (vertex * 'a * vertex) iter -> 'a t -> 'a t
  (** @since 2.8 *)

  val to_iter : 'a t -> (vertex * 'a * vertex) iter
  (** @since 2.8 *)

  val of_seq : (vertex * 'a * vertex) iter -> 'a t
  (** @deprecated use {!of_iter} instead *)

  val add_seq : (vertex * 'a * vertex) iter -> 'a t -> 'a t
  (** @deprecated use {!add_iter} instead *)

  val to_seq : 'a t -> (vertex * 'a * vertex) iter
  (** @deprecated use {!to_iter} instead *)
end

module Map(O : Map.OrderedType) : MAP with type vertex = O.t = struct
  module M = Map.Make(O)

  type vertex = O.t
  type 'a t = 'a M.t M.t
  (* vertex -> set of (vertex * label) *)

  let as_graph m =
    (fun v yield ->
       try
         let sub = M.find v m in
         M.iter (fun v' e -> yield (e, v')) sub
       with Not_found -> ()
    )

  let empty = M.empty

  let add_edge v1 e v2 m =
    let sub = try M.find v1 m with Not_found -> M.empty in
    M.add v1 (M.add v2 e sub) m

  let remove_edge v1 v2 m =
    try
      let map = M.remove v2 (M.find v1 m) in
      if M.is_empty map
      then M.remove v1 m
      else M.add v1 map m
    with Not_found -> m

  let add v m =
    if M.mem v m then m
    else M.add v M.empty m

  let remove v m = M.remove v m

  let union m1 m2 =
    M.merge
      (fun _ s1 s2 -> match s1, s2 with
         | Some s, None
         | None, Some s -> Some s
         | None, None -> assert false
         | Some s1, Some s2 ->
           let s =
             M.merge
               (fun _ e1 e2 -> match e1, e2 with
                  | Some _, _ -> e1
                  | None, _ -> e2)
               s1 s2
           in
           Some s)
      m1 m2

  let vertices m yield = M.iter (fun v _ -> yield v) m

  let vertices_l m = M.fold (fun v _ acc -> v::acc) m []

  let add_list l m = List.fold_left (fun m (v1,e,v2) -> add_edge v1 e v2 m) m l

  let of_list l = add_list l empty

  let to_list m =
    M.fold
      (fun v map acc -> M.fold (fun v' e acc -> (v,e,v')::acc) map acc)
      m []

  let add_iter iter m = Iter.fold (fun m (v1,e,v2) -> add_edge v1 e v2 m) m iter

  let of_iter iter = add_iter iter empty

  let to_iter m k = M.iter (fun v map -> M.iter (fun v' e -> k(v,e,v')) map) m

  let add_seq = add_iter
  let of_seq = of_iter
  let to_seq = to_iter
end

(** {2 Misc} *)

let of_list ~eq l =
  (fun v yield -> List.iter (fun (a,b) -> if eq a v then yield ((),b)) l)

let of_fun f =
  (fun v yield ->
     let l = f v in
     List.iter (fun v' -> yield ((),v')) l
  )

let of_hashtbl tbl =
  (fun v yield ->
     try List.iter (fun b -> yield ((), b)) (Hashtbl.find tbl v)
     with Not_found -> ()
  )

let divisors_graph =
  (fun i ->
     (* divisors of [i] that are [>= j] *)
     let rec divisors j i yield =
       if j < i
       then (
         if (i mod j = 0) then yield ((),j);
         divisors (j+1) i yield
       )
     in
     divisors 1 i
  )
