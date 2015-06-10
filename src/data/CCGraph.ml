
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

module Seq = struct
  type 'a t = 'a sequence
  let return x k = k x
  let (>>=) a f k = a (fun x -> f x k)
  let map f a k = a (fun x -> k (f x))
  let iter f a = a f
  let fold f acc a =
    let acc = ref acc in
    a (fun x -> acc := f !acc x);
    !acc
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
  ; size=(fun () -> H.length tbl)
  }

(** {2 Traversals} *)

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

let traverse ?tbl:(mk_tbl=mk_table ?eq:None ?hash:None) ~bag:mk_bag ~graph seq =
  fun k ->
    let bag = mk_bag() in
    Seq.iter bag.push seq;
    let tbl = mk_tbl 128 in
    let bag = mk_bag () in
    while not (bag.is_empty ()) do
      let x = bag.pop () in
      if not (tbl.mem x) then (
        k x;
        tbl.add x ();
        Seq.iter
          (fun e -> bag.push (graph.dest e))
          (graph.children x)
      )
    done

let traverse_tag ~tags ~bag ~graph seq =
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

let bfs ?tbl ~graph seq =
  traverse ?tbl ~bag:mk_queue ~graph seq

let bfs_tag ~tags ~graph seq =
  traverse_tag ~tags ~bag:(mk_queue()) ~graph seq

let dfs ?tbl ~graph seq =
  traverse ?tbl ~bag:mk_stack ~graph seq

let dfs_tag ~tags ~graph seq =
  traverse_tag ~tags ~bag:(mk_stack()) ~graph seq

let dijkstra ?(tbl=mk_table ?eq:None ?hash:None) ?(dist=fun _ -> 1) ~graph seq =
  (* a table [('v * int) -> 'a] built from a ['v -> 'a] table *)
  let mk_tbl' size =
    let vertex_tbl = tbl size in
    { mem=(fun (v, _) -> vertex_tbl.mem v)
    ; find=(fun (v, _) -> vertex_tbl.find v)
    ; add=(fun (v, _) -> vertex_tbl.add v)
    ; size=vertex_tbl.size
    }
  and seq' = Seq.map (fun v -> v, 0) seq
  and graph' = {
    children=(fun (v,d) -> Seq.map (fun e -> e, d) (graph.children v));
    origin=(fun (e, d) -> graph.origin e, d);
    dest=(fun (e, d) -> graph.dest e, d + dist e);
  } in
  let mk_bag () = mk_heap ~leq:(fun (_, d1) (_, d2) -> d1 <= d2) in
  traverse ~tbl:mk_tbl' ~bag:mk_bag ~graph:graph' seq'

let dijkstra_tag ?(dist=fun _ -> 1) ~tags ~graph seq = assert false (* TODO *)





