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

module Make (A : ARG) = struct
  module A = A

  type state = {
    mutable min_id: int; (* min ID of the vertex' scc *)
    id: int; (* ID of the vertex *)
    mutable on_stack: bool;
    vertex: A.node;
  }

  let mk_cell v n = { min_id = n; id = n; on_stack = false; vertex = v }

  (* pop elements of [stack] until we reach node with given [id] *)
  let rec pop_down_to ~id acc stack =
    assert (not (Stack.is_empty stack));
    let cell = Stack.pop stack in
    cell.on_stack <- false;
    if cell.id = id then (
      assert (cell.id = cell.min_id);
      cell.vertex :: acc (* return SCC *)
    ) else
      pop_down_to ~id (cell.vertex :: acc) stack

  let scc (graph : A.t) (nodes : A.node list) : _ list list =
    let res = ref [] in
    let tbl = A.Node_tbl.create 16 in

    (* stack of nodes being explored, for the DFS *)
    let to_explore = Stack.create () in
    (* stack for Tarjan's algorithm itself *)
    let stack = Stack.create () in
    (* unique ID for new nodes *)
    let n = ref 0 in

    (* exploration starting from [v] *)
    let explore_from (v : A.node) : unit =
      Stack.push (`Enter v) to_explore;
      while not (Stack.is_empty to_explore) do
        match Stack.pop to_explore with
        | `Enter v ->
          if not (A.Node_tbl.mem tbl v) then (
            (* remember unique ID for [v] *)
            let id = !n in
            incr n;
            let cell = mk_cell v id in
            cell.on_stack <- true;
            A.Node_tbl.add tbl v cell;
            Stack.push cell stack;
            Stack.push (`Exit (v, cell)) to_explore;
            (* explore children *)
            let children = A.children graph v in
            children (fun v' -> Stack.push (`Enter v') to_explore)
          )
        | `Exit (v, cell) ->
          (* update [min_id] *)
          assert cell.on_stack;
          let children = A.children graph v in
          children (fun dest ->
              (* must not fail, [dest] already explored *)
              let dest_cell = A.Node_tbl.find tbl dest in
              (* same SCC? yes if [dest] points to [cell.v] *)
              if dest_cell.on_stack then
                cell.min_id <- min cell.min_id dest_cell.min_id);
          (* pop from stack if SCC found *)
          if cell.id = cell.min_id then (
            let scc = pop_down_to ~id:cell.id [] stack in
            res := scc :: !res
          )
      done
    in

    List.iter explore_from nodes;
    assert (Stack.is_empty stack);
    !res
end

let scc (type graph node) ~(tbl : (module Hashtbl.S with type key = node))
    ~graph ~children ~nodes () : _ list =
  let module S = Make (struct
    type t = graph
    type nonrec node = node

    let children = children

    module Node_tbl = (val tbl)
  end) in
  S.scc graph nodes
