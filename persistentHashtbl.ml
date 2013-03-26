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

(** {1 Persistent hash-table on top of OCaml's hashtables} *)

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Signature of such a hashtable} *)

module type S = sig
  type key
  type 'a t

  val create : int -> 'a t
    (** Create a new hashtable *)

  val is_empty : 'a t -> bool
    (** Is the table empty? *)

  val find : 'a t -> key -> 'a
    (** Find the value for this key, or raise Not_found *)

  val mem : 'a t -> key -> bool
    (** Is the key bound? *)

  val length : 'a t -> int
    (** Number of bindings *)

  val replace : 'a t -> key -> 'a -> 'a t
    (** Add the binding to the table, returning a new table. This erases
        the current binding for [key], if any. *)

  val remove : 'a t -> key -> 'a t
    (** Remove the key *)

  val copy : 'a t -> 'a t
    (** Fresh copy of the table; the underlying structure is not shared
        anymore, so using both tables alternatively will be efficient *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
    (** Iterate over bindings *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold over bindings *)

  val of_gen : ?init:'a t -> (key * 'a) Gen.t -> 'a t
    (** Add (replace) bindings from the generator to the table *)

  val to_gen : 'a t -> (key * 'a) Gen.t
    (** Generator on the bindings of the table *)
end

(** {2 Implementation} *)

module Make(H : HashedType) : S with type key = H.t = struct
  module Table = Hashtbl.Make(H)
    (** Imperative hashtable *)

  type key = H.t
  type 'a t = 'a zipper ref
  and 'a zipper =
    | Table of 'a Table.t         (** Concrete table *)
    | Add of key * 'a * 'a t      (** Add key *)
    | Replace of key * 'a * 'a t  (** Replace key by value *)
    | Remove of key * 'a t        (** As the table, but without given key *)

  let create i =
    ref (Table (Table.create i))

  (** Reroot: modify the zipper so that the current node is a proper
      hashtable, and return the hashtable *)
  let reroot t =
    (* pass continuation to get a tailrec rerooting *)
    let rec reroot t k = match !t with
    | Table tbl -> k tbl  (* done *)
    | Add (key, v, t') ->
      reroot t'
        (fun tbl ->
          t' := Remove (key, t);
          Table.add tbl key v;
          t := Table tbl;
          k tbl)
    | Replace (key, v, t') ->
      reroot t'
        (fun tbl ->
          let v' = Table.find tbl key in
          t' := Replace (key, v', t);
          t := Table tbl;
          Table.replace tbl key v;
          k tbl)
    | Remove (key, t') ->
      reroot t'
        (fun tbl ->
          let v = Table.find tbl key in
          t' := Add (key, v, t);
          t := Table tbl;
          Table.remove tbl key;
          k tbl)
    in
    reroot t (fun x -> x)

  let is_empty t =
    match !t with
    | Table tbl -> Table.length tbl = 0
    | _ -> Table.length (reroot t) = 0

  let find t k =
    match !t with
    | Table tbl -> Table.find tbl k
    | _ -> Table.find (reroot t) k

  let mem t k =
    match !t with
    | Table tbl -> Table.mem tbl k
    | _ -> Table.mem (reroot t) k

  let length t =
    match !t with
    | Table tbl -> Table.length tbl
    | _ -> Table.length (reroot t)

  let replace t k v =
    let tbl = match !t with
    | Table tbl -> tbl
    | _ -> reroot t in
    (* create the new hashtable *)
    let t' = ref (Table tbl) in
    (* update [t] to point to the new hashtable *)
    (try
      let v' = Table.find tbl k in
      t := Replace (k, v', t')
    with Not_found ->
      t := Remove (k, t'));
    (* modify the underlying hashtable *)
    Table.replace tbl k v;
    t'

  let remove t k =
    let tbl = match !t with
    | Table tbl -> tbl
    | _ -> reroot t in
    try
      let v' = Table.find tbl k in
      (* value present, make a new hashtable without this value *)
      let t' = ref (Table tbl) in
      t := Add (k, v', t');
      Table.remove tbl k;
      t'
    with Not_found ->
      (* not member, nothing to do *)
      t

  let copy t =
    let tbl = match !t with
    | Table tbl -> tbl
    | _ -> reroot t in
    (* no one will point to the new [t] *)
    let t = ref (Table (Table.copy tbl)) in
    t
  
  let iter t f =
    let tbl = match !t with
    | Table tbl -> tbl
    | _ -> reroot t in
    Table.iter f tbl

  let fold f acc t =
    let tbl = match !t with
    | Table tbl -> tbl
    | _ -> reroot t in
    Table.fold (fun k v acc -> f acc k v) tbl acc

  let of_gen ?init gen =
    let tbl = match init with
    | None -> Table.create 5
    | Some t -> Table.copy (reroot t) in
    Gen.iter (fun (k,v) -> Table.replace tbl k v) gen;
    ref (Table tbl)

  let to_gen t =
    (* not efficient at the moment... *)
    let tbl = reroot t in
    let bindings = Table.fold (fun k v acc -> (k,v)::acc) tbl [] in
    Gen.of_list bindings
end

