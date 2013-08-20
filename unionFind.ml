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

(** {1 Imperative Union-Find structure} *)

(** We need to be able to hash and compare keys, and values need to form
    a monoid *)
module type PAIR = sig
  type key
  type value

  val hash : key -> int
  val equal : key -> key -> bool

  val merge : value -> value -> value
  val zero : value
end

(** Build a union-find module from a key/value specification *)
module Make(P : PAIR) = struct
  type key = P.key
    (** Elements that can be compared *)

  type value = P.value
    (** Values associated with elements *)

  type node = {
    mutable n_repr : key;     (* representative *)
    mutable n_value : value;  (* value (only up-to-date for representative) *)
  }

  module H = Hashtbl.Make(struct include P type t = P.key end)

  (** The union-find imperative structure itself*)
  type t = node H.t

  let mk_node key = {
    n_repr = key;
    n_value = P.zero;
  }

  (** Elements that can be compared *)
  let create keys =
    let t = H.create 5 in
    (* add k -> zero for each key k *)
    List.iter (fun key -> H.replace t key (mk_node key)) keys;
    t

  let mem t key = H.mem t key

  (** Find representative value for this key. *)
  let rec find_root t key =
    let node = H.find t key in
    (* if key is its own representative, done; otherwise recurse toward key's root *)
    if P.equal key node.n_repr
      then node
      else begin
        (* path compression *)
        let node' = find_root t node.n_repr in
        node.n_repr <- node'.n_repr;
        node'
      end

  let find t key = (find_root t key).n_repr

  (** Get value of the root for this key. *)
  let find_value t key = (find_root t key).n_value

  (** Merge two representatives *)
  let union t k1 k2 =
    let n1, n2 = find_root t k1, find_root t k2 in
    if not (P.equal n1.n_repr n2.n_repr)
      then begin
        (* k2 points to k1, and k1 points to the new value *)
        n1.n_value <- P.merge n1.n_value n2.n_value;
        n2.n_repr <- n1.n_repr;
      end

  (** Add the given value to the key (monoid) *)
  let add t key value =
    try
      let node = find_root t key in
      node.n_value <- P.merge node.n_value value
    with Not_found ->
      let node = mk_node key in
      node.n_value <- value;
      H.add t key node

  (** Iterate on representative and their value *)
  let iter t f =
    H.iter
      (fun key node -> if P.equal key node.n_repr then f key node.n_value)
      t
end
