
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

(** {2 Hypergraph Representation}

CCGeneralized Hypergraphs. Objects are either constants, or hyperedges that
connect [n] other objets together (a [n]-tuple). Each hyperedge can contain
additional data.
*)

module type S = sig
  type const
    (** Constants. Those are what can annotate hyperedges or make single,
        leaf, nodes. *)

  type t
    (** An hypergraph. It stores a set of edges, and possibly inherits from
        another graph. *)

  type edge
    (** A single edge of the hypergraph. *)

  val self : t -> edge
    (** The edge that represents (reifies) the hypergraph itself *)

  val eq : edge -> edge -> bool
    (** Equality of the two edges. *)

  val arity : edge -> int
    (** Number of sub-elements of the edge (how many other edges it connects
        together) *)

  val nth : edge -> int -> edge
    (** [nth x i] accesses the [i]-th sub-node of [x].
        @raise Invalid_argument if [i >= arity x]. *)

  val make_graph : ?parent:t -> unit -> t
    (** New graph, possibly inheriting from another graph. *)

  val make_edge : t -> edge array -> edge
    (** Create a new hyperedge from an ordered tuple of sub-edges.
        The edge belongs to the given graph.
        The array must not be used afterwards and must not be empty.
        @raise Invalid_argument if the array is empty *)

  val make_const : t -> const -> edge
    (** Constant edge, without sub-edges *)

  val fresh : t -> edge
    (** Fresh edge, without constant. It is equal to no other edge. *)

  module EdgeTbl : Hashtbl.S with type key = edge

  val pp : ?printed:unit EdgeTbl.t ->
           Buffer.t -> edge -> unit
    (** Print the edge on the buffer. @param printed: sub-edges already
        printed. *)

  val fmt : Format.formatter -> edge -> unit
  val to_string : edge -> string
end

module type PARAM = sig
  type const

  val eq : const -> const -> bool
  val hash : const -> int
  val to_string : const -> string  (* for printing *)
end

module Make(P : PARAM) = struct
  type const = P.const

  type edge =
    | Fresh of int
    | Const of const
    | Edge of edge array

  let rec eq e1 e2 = match e1, e2 with
    | Fresh _, Fresh _ -> e1 == e2
    | Const c1, Const c2 -> P.eq c1 c2
    | Edge a1, Edge a2 ->
      Array.length a1 = Array.length a2 &&
      begin try
        for i = 0 to Array.length a1 - 1 do
          if not (eq (Array.unsafe_get a1 i) (Array.unsafe_get a2 i))
            then raise Exit;
        done; true
      with Exit -> false
      end
    | _ -> false

  let rec hash e = match e with
    | Fresh i -> i
    | Const c -> P.hash c
    | Edge a ->
      let h = ref 0 in
      for i = 0 to Array.length a - 1 do
        h := max_int land (!h * 65599 + (hash (Array.unsafe_get a i)))
      done;
      !h

  (* hashtable on edges *)
  module EdgeTbl = Hashtbl.Make(struct
    type t = edge
    let equal = eq
    let hash = hash
  end)

  (* hashtable on edges * int *)
  module BackTbl = Hashtbl.Make(struct
    type t = edge * int
    let equal (e1, i1) (e2, i2) = i1 = i2 && eq e1 e2
    let hash (e, i) = i * 65599 + hash e
  end)

  (** Hypergraph: set of edges. We map each edge to other edges that point
      to it (knowing which ones it points to is trivial) *)
  type t = {
    edges : unit EdgeTbl.t;
    backref : edge BackTbl.t; 
    parent : t option;
    mutable count : int;  (* used for Fresh nodes *)
    self : edge;
  }

  let arity e = match e with
    | Fresh _
    | Const _ -> 0
    | Edge a -> Array.length a

  let nth e i = match e with
    | Fresh _
    | Const _ -> raise (Invalid_argument"HGraph.nth")
    | Edge a -> a.(i)

  let self g = g.self

  let make_graph ?parent () =
    let g = { 
      parent;
      edges = EdgeTbl.create 15;
      backref = BackTbl.create 15;
      count = 1;
      self = Fresh 0;
    } in
    g

  (* add a backref from [e]'s sub-edges to [e] *)
  let _add_backrefs g e = match e with
    | Fresh _
    | Const _ -> assert false
    | Edge a ->
      for i = 0 to Array.length a - 1 do
        BackTbl.add g.backref (Array.unsafe_get a i, i) e
      done

  let make_edge g sub =
    if Array.length sub = 0 then raise (Invalid_argument "HGraph.make_edge");
    let e = Edge sub in
    (* add edge if not already present *)
    if not (EdgeTbl.mem g.edges e) then begin
      EdgeTbl.add g.edges e ();
      _add_backrefs g e
    end;
    e

  let make_const g c =
    let e = Const c in
    if not (EdgeTbl.mem g.edges e) then
      EdgeTbl.add g.edges e ();
    e

  let fresh g =
    let e = Fresh g.count in
    g.count <- g.count + 1;
    (* always new! *)
    EdgeTbl.add g.edges e ();
    e

  let pp ?(printed=EdgeTbl.create 7) buf e =
    let rec pp buf e = match e with
    | Fresh i -> Printf.bprintf buf "_e%d" i
    | Const c -> Buffer.add_string buf (P.to_string c)
    | Edge a ->
      if not (EdgeTbl.mem printed e) then begin
        EdgeTbl.add printed e ();
        Buffer.add_char buf '[';
        for i = 0 to Array.length a - 1 do
          if i > 0 then Buffer.add_char buf ' ';
          pp buf a.(i)
        done;
        Buffer.add_char buf ']'
      end
    in
    pp buf e

  let to_string e =
    let buf = Buffer.create 15 in
    pp buf e;
    Buffer.contents buf

  let fmt fmt e =
    Format.pp_print_string fmt (to_string e)
end

(** {2 Useful default} *)

module DefaultParam = struct
  type const =
    | S of string
    | I of int

  type data = unit

  let eq c1 c2 = match c1, c2 with
    | S s1, S s2 -> s1 = s2
    | I i1, I i2 -> i1 = i2
    | _ -> false

  let hash = function
    | S s -> Hashtbl.hash s
    | I i -> i

  let to_string = function
    | S s -> s
    | I i -> string_of_int i

  let i i = I i
  let s s = S s
end

module Default = struct
  include Make(DefaultParam)

  exception EOI
  exception Error of string

  module Lexbuf = struct
    type t = {
      mutable s : string;
      mutable i : int;
      get : (unit -> string option);
    }

    let of_string s = { s; i=0; get = (fun () -> None); }

    let of_fun get = { s=""; i = 0; get; }

    let of_chan c =
      let s = String.make 64 ' ' in
      let get () =
        try
          let n = input c s 0 64 in
          Some (String.sub s 0 n)
        with End_of_file -> None
      in
      { s = ""; i = 0; get; }
  end

  let rec _get_rec lb =
    if lb.Lexbuf.i >= String.length lb.Lexbuf.s
      then match lb.Lexbuf.get () with
      | None -> raise EOI
      | Some s' ->
        lb.Lexbuf.s <- s';
        lb.Lexbuf.i <- 0;
        _get_rec lb
      else lb.Lexbuf.s.[lb.Lexbuf.i]

  let _get lb =
    if lb.Lexbuf.i >= String.length lb.Lexbuf.s
      then _get_rec lb
      else lb.Lexbuf.s.[lb.Lexbuf.i]

  let _skip lb = lb.Lexbuf.i <- lb.Lexbuf.i + 1

  (* skip whitespace *)
  let rec _white lb =
    match _get lb with
    | ' ' | '\t' | '\n' -> _skip lb; _white lb
    | _ -> ()

  (* read lb, expecting the given char *)
  let _expect lb c =
    if _get lb = c
      then _skip lb
      else raise (Error (Printf.sprintf "expected %c" c))

  let rec __parse_edge g lb =
    _white lb;
    match _get lb with
    | '[' ->
      _skip lb;
      let sub = __parse_edges g [] lb in
      let sub = match sub with
        | [] -> raise (Error "parsed an empty list of sub-edges")
        | _ -> Array.of_list sub
      in
      _white lb;
      _expect lb ']';
      make_edge g sub
    | '0' .. '9' ->
      let i = _parse_int 0 lb in
      make_const g (DefaultParam.I i)
    | '_' ->
      _skip lb;
      fresh g
    | _ ->
      let s = _parse_str (Buffer.create 15) lb in
      make_const g (DefaultParam.S s)

  and __parse_edges g acc lb =
    _white lb;
    match _get lb with
    | ']' -> List.rev acc  (* done *)
    | _ ->
      let e = __parse_edge g lb in
      __parse_edges g (e::acc) lb

  and _parse_int i lb =
    match _get lb with
    | ('0' .. '9') as c ->
      let n = Char.code c - Char.code '0' in
      _skip lb;
      _parse_int ((i * 10) + n) lb
    | _ -> i

  and _parse_str buf lb =
    match _get lb with
    | ' ' | '\t' | '\n' | ']' -> Buffer.contents buf  (* done *)
    | '\\' ->
      (* must read next char *)
      _skip lb;
      Buffer.add_char buf (_get lb);
      _skip lb;
      _parse_str buf lb
    | c ->
      Buffer.add_char buf c;
      _skip lb;
      _parse_str buf lb

  (* parse one edge *)
  let parse_edge g lb =
    try `Ok (__parse_edge g lb)
    with
    | EOI -> `Error "unexpected end of input"
    | Error e -> `Error e

  let edge_of_string g s = parse_edge g (Lexbuf.of_string s)
end
