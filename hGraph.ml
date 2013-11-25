
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

Generalized Hypergraphs. Objects are either constants, or hyperedges that
connect [n] other objets together (a [n]-tuple). Each hyperedge can contain
additional data.
*)

module type S = sig
  type const
    (** Constants. Those are what can annotate hyperedges or make single,
        leaf, nodes. *)

  type data
    (** Additional data carried by the hypergraph elements *)

  type t
    (** An element of the hypergraph. It can be parametrized by a ['a option]
        additional data (use 'a = unit if you don't care). *)

  val eq : t -> t -> bool
    (** Structural equality of the two edges *)

  val hash : t -> int
    (** Hash, used for hashtables *)

  val id : t -> int
    (** Same as {!hash}, but guarantees that the int is actually unique. *)

  val cmp : t -> t -> int
    (** Arbitrary total order *)

  val data : t -> data option
    (** Data contained in this edge, if any *)

  val const : t -> const
    (** Constant that annotates this hyperedge. *)

  val arity : t -> int
    (** Number of sub-elements *)

  val nth : t -> int -> t
    (** [nth x i] accesses the [i]-th sub-node of [x].
        @raise Invalid_argument if [i >= arity x]. *)

  val sub : t -> t array
    (** Access the sub-nodes as an array. This array {b MUST NOT} be modified
        by the caller. *)

  val make : ?data:data -> const -> t array -> t
    (** Create a new hyperedge from a constant that annotates it, and
        an ordered tuple of sub-edges.
        @param data optional data to decorate the edge. *)

  val make_l : ?data:data -> const -> t list -> t
    (** From a list, same as {!make} otherwise *)

  val const : ?data:data -> const -> t
    (** Constant node *)

  val update : ?data:data -> t -> t array -> t
    (** [update e sub] creates an hyperedge equivalent to [e] in all ways,
        but with the given sub-nodes as sub-edges. The array's ownership
        is lost by the caller.

        @param data optional data that annotates the new edge.
    *)

  val pp : Buffer.t -> t -> unit
    (** Print itself (non-recursively) on the buffer *)
end

module type PARAM = sig
  type const
  type data

  val eq : const -> const -> bool
  val hash : const -> int
  val to_string : const -> string  (* for printing *)
end

module Make(P : PARAM) = struct
  type const = P.const
  type data = P.data

  type t = {
    head : const;
    data : data option;
    sub : t array;
    mutable id : int;
    mutable backref : t Weak.t;
  }

  type edge = t

  let eq t1 t2 = t1.id = t2.id

  let hash t = t.id

  let id t = t.id

  let cmp t1 t2 = t1.id - t2.id

  let data t = t.data

  let const t = t.head

  let arity t = Array.length t.sub

  let nth t i = t.sub.(i)

  let sub t = t.sub

  (* add a backref from [a] to [b]. *)
  let _add_backref a b =
    let n = Weak.length a.backref in
    let arr = a.backref in
    try
      for i = 0 to n-1 do
        if not (Weak.check arr i)
          then begin  (* insert here *)
            Weak.set arr i (Some b);
            raise Exit;
          end
      done;
      (* no insertion possible: resize *)
      a.backref <- Weak.create (2 * n);
      Weak.blit arr 0 a.backref 0 n;
      Weak.set a.backref n (Some b)
    with Exit -> ()

  (* structural equality on top-level *)
  let _eq_top t1 t2 =
    Array.length t1.sub = Array.length t2.sub &&
    P.eq t1.head t2.head &&
    try
      for i = 0 to Array.length t1.sub - 1 do
        if not (eq (Array.unsafe_get t1.sub i) (Array.unsafe_get t2.sub i)) then raise Exit;
      done; true
    with Exit -> false

  (* top-level hashing *)
  let _hash_top t =
    let h = ref (P.hash t.head) in
    for i = 0 to Array.length t.sub - 1 do
      h := max_int land (!h * 65599 + (hash (Array.unsafe_get t.sub i)))
    done;
    !h

  (* hashconsing weak table *)
  module H = Weak.Make(struct
    type t = edge
    let equal = _eq_top
    let hash = _hash_top
  end)

  let __count = ref 0
  let __table = H.create 2045

  let make ?data head sub =
    let my_t = {
      head;
      data;
      sub;
      id = ~-1;
      backref = Weak.create 0;
    } in
    let t = H.merge __table my_t in
    if t == my_t then begin
      (* hashconsing tag *)
      assert (t.id = ~-1);
      t.id <- !__count;
      incr __count;
      (* make a proper backref array *)
      t.backref <- Weak.create 5;
      (* add oneself to sub-nodes' backref arrays *)
      Array.iter (fun t' -> _add_backref t' t) sub
      end;
    t

  let make_l ?data head sub = make ?data head (Array.of_list sub)

  let const ?data head = make ?data head [| |]

  let update ?data t sub' = make ?data t.head sub'

  let pp buf e =
    Buffer.add_string buf (string_of_int e.id);
    Buffer.add_char buf ':';
    if arity e = 0
      then Buffer.add_string buf (P.to_string e.head)
      else begin
        Buffer.add_char buf '(';
        Buffer.add_string buf (P.to_string e.head);
        Array.iteri
          (fun i sub ->
            if i > 0 then Buffer.add_char buf ' ';
            Buffer.add_string buf (string_of_int sub.id))
          e.sub;
        Buffer.add_char buf ')'
      end
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

module Default = Make(DefaultParam)
