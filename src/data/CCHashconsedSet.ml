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

(** {1 Hashconsed Sets} *)


(* uses "Fast Mergeable Integer Maps", Okasaki & Gill, as a hash tree.
We use big-endian trees. *)

module type ELT = sig
  type t

  val compare : t -> t -> int
  (** Total order *)

  val hash : t -> int
  (** Deterministic *)
end

module type S = sig
  type elt

  type t
  (** Set of elements *)

  val empty : t

  val singleton : elt -> t

  val doubleton : elt -> elt -> t

  val mem : elt -> t -> bool

  val equal : t -> t -> bool
  (** Fast equality test [O(1)] *)

  val compare : t -> t -> int
  (** Fast (arbitrary) comparison test [O(1)] *)

  val hash : t -> int
  (** Fast (arbitrary, deterministic) hash [O(1)] *)

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val cardinal : t -> int

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on elements, in no particular order *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** fold on elements, in arbitrary order *)

  val choose : t -> elt option

  val choose_exn : t -> elt

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  (** {2 Whole-collection operations} *)

  type 'a sequence = ('a -> unit) -> unit
  type 'a gen = unit -> 'a option

  val add_list : t -> elt list -> t

  val of_list : elt list -> t

  val to_list : t -> elt list

  val add_seq : t -> elt sequence -> t

  val of_seq : elt sequence -> t

  val to_seq : t -> elt sequence
end

module Make(E : ELT) : S with type elt = E.t = struct
  type elt = E.t

  type t = {
    cell: cell;
    id: int;  (* unique hashconsing ID *)
  }
  and cell =
    | E  (* empty *)
    | L of int * elt list  (* leaf: sorted list of elements *)
    | N of int (* common prefix *) * int (* bit switch *) * t * t

  let rec eq_list_ l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | x1 :: tl1, x2 :: tl2 ->
      E.compare x1 x2 = 0 && eq_list_ tl1 tl2

  let hash_pair_ a b = Hashtbl.hash (a,b)
  let hash_quad_ a b c d = Hashtbl.hash (a,b,c,d)

  let rec hash_list_ l = match l with
    | [] -> 0xf00d
    | x :: tl -> hash_pair_ x (hash_list_ tl)

  (* hashconsing table *)
  module Tbl = Weak.Make(struct
    type t_ = t
    type t = t_
    let equal t1 t2 = match t1.cell, t2.cell with
      | E, E -> true
      | L (k1, l1), L (k2, l2) -> k1==k2 && eq_list_ l1 l2
      | N (a1, b1, l1, r1), N (a2, b2, l2, r2) ->
        a1==a2 && b1==b2 && l1.id == l2.id && r1.id == r2.id
      | E, _
      | L _, _
      | N _, _ -> false
    let hash t = match t.cell with
      | E -> 42
      | L (k, l) -> hash_pair_ k (hash_list_ l)
      | N (a, b, l, r) ->
        hash_quad_ a b l.id r.id
  end)

  let table_ = Tbl.create 4096
  let id_ = ref 1

  (* make a node out of a cell, with hashconsing *)
  let hashcons_ cell =
    let n = {cell; id= !id_} in
    let n' = Tbl.merge table_ n in
    if n==n' then incr id_;
    n'

  (* empty tree *)
  let empty = hashcons_ E

  let bit_is_0_ x ~bit = x land bit = 0

  let mask_ x ~mask = (x lor (mask -1)) land (lnot mask)
  (* low endian: let mask_ x ~mask = x land (mask - 1) *)

  let is_prefix_ ~prefix y ~bit = prefix = mask_ y ~mask:bit

  (* loop down until x=lowest_bit_ x *)
  let rec highest_bit_naive x m =
    if m = 0 then 0
    else if x land m = 0 then highest_bit_naive x (m lsr 1)
    else m

  let highest_bit_ =
    (* the highest representable 2^n *)
    let max_log = 1 lsl (Sys.word_size - 2) in
    fun x ->
      if x > 1 lsl 20
      then (* small shortcut: remove least significant 20 bits *)
        let x' = x land (lnot ((1 lsl 20) -1)) in
        highest_bit_naive x' max_log
      else highest_bit_naive x max_log

  let branching_bit_ a b = highest_bit_ (a lxor b)

  let rec list_mem_ x l = match l with
    | [] -> false
    | y :: tl ->
      match E.compare x y with
      | 0 -> true
      | c when c > 0 -> list_mem_ x tl
      | _ -> false (* [x] cannot be in the tail, all elements are larger *)

  let rec mem_rec_ k x t = match t.cell with
    | E -> false
    | L (k', l) when k = k' ->
      list_mem_ x l
    | L _ -> false
    | N (prefix, m, l, r) ->
      if is_prefix_ ~prefix k ~bit:m
      then if bit_is_0_ k ~bit:m
        then mem_rec_ k x l
        else mem_rec_ k x r
      else raise Not_found

  let equal t1 t2 = t1.id = t2.id

  let compare t1 t2 = Pervasives.compare t1.id t2.id

  let hash t = t.id land max_int

  let mem x t = mem_rec_ (E.hash x) x t

  let mk_node_ prefix switch l r = match l.cell, r.cell with
    | E, _ -> r
    | _, E -> l
    | _ -> hashcons_ (N (prefix, switch, l, r))

  let mk_leaf_ hash l = match l with
    | [] -> empty
    | _::_ -> hashcons_ (L (hash, l))

  (* join trees t1 and t2 with prefix p1 and p2 respectively
     (p1 and p2 do not overlap) *)
  let join_ t1 p1 t2 p2 =
    let switch = branching_bit_ p1 p2 in
    let prefix = mask_ p1 ~mask:switch in
    if bit_is_0_ p1 ~bit:switch
    then mk_node_ prefix switch t1 t2
    else (assert (bit_is_0_ p2 ~bit:switch); mk_node_ prefix switch t2 t1)

  let singleton_ k x = hashcons_ (L (k, [x]))

  let singleton x = singleton_ (E.hash x) x

  (* insert [x] in [l], keeping [l] sorted *)
  let rec insert_list_ x l = match l with
    | [] -> [x]
    | y :: tl ->
      match E.compare x y with
      | 0 -> l  (* already in there *)
      | c when c<0 ->
        (* x<y, insert in front *)
        x :: l
      | _ -> y :: insert_list_ x tl

  let rec add_rec_ k x t = match t.cell with
    | E -> hashcons_ (L (k, [x]))
    | L (k', l) ->
      if k=k'
      then hashcons_ (L (k, insert_list_ x l))
      else join_ t k' (singleton_ k x) k
    | N (prefix, switch, l, r) ->
      if is_prefix_ ~prefix k ~bit:switch
      then if bit_is_0_ k ~bit:switch
        then hashcons_ (N(prefix, switch, add_rec_ k x l, r))
        else hashcons_ (N(prefix, switch, l, add_rec_ k x r))
      else join_ (singleton_ k x) k t prefix

  let add x t = add_rec_ (E.hash x) x t

  (*$Q & ~count:20
    Q.(list_of_size Gen.(0 -- 300) int) (fun l -> \
      let module S = Make(CCInt) in \
      let m = S.of_list l in \
      List.for_all (fun x -> S.mem x m) l)
  *)

  let rec remove_list_ x l = match l with
    | [] -> []
    | y :: tl ->
      match E.compare x y with
      | 0 -> tl (* eliminate *)
      | c when c<0 -> l  (* cannot be in [l] *)
      | _ -> y :: remove_list_ x tl

  let rec remove_rec_ k x t = match t.cell with
    | E -> empty
    | L (k', l) when k=k' ->
      mk_leaf_ k (remove_list_ x l)
    | L _ -> t  (* preserve *)
    | N (prefix, switch, l, r) ->
      if is_prefix_ ~prefix k ~bit:switch
      then if bit_is_0_ k ~bit:switch
        then mk_node_ prefix switch (remove_rec_ k x l) r
        else mk_node_ prefix switch l (remove_rec_ k x r)
      else t (* not present *)

  let remove x l = remove_rec_ (E.hash x) x l

  let doubleton v1 v2 = add v1 (singleton v2)

  let rec iter f t = match t.cell with
    | E -> ()
    | L (_, v) -> List.iter f v
    | N (_, _, l, r) -> iter f l; iter f r

  let rec fold f t acc = match t.cell with
    | E -> acc
    | L (_, l) -> List.fold_right f l acc
    | N (_, _, l, r) ->
      let acc = fold f l acc in
      fold f r acc

  let cardinal t = fold (fun _ n -> n+1) t 0

  let rec choose_exn t = match t.cell with
    | E -> raise Not_found
    | L (_, []) -> assert false
    | L (_, x :: _) -> x
    | N (_, _, l, _) -> choose_exn l

  let choose t =
    try Some (choose_exn t)
    with Not_found -> None

  let rec union_list_ l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | x1 :: tl1, x2 :: tl2 ->
      match E.compare x1 x2 with
      | 0 -> x1 :: union_list_ tl1 tl2
      | c when c<0 -> x1 :: union_list_ tl1 l2
      | _ -> x2 :: union_list_ l1 tl2

  (* add elements of [l], all of which have hash [k], to [t] *)
  let add_list_hash_ k l t =
    List.fold_left
      (fun t x -> add_rec_ k x t)
      t l

  let rec union a b = match a.cell, b.cell with
    | E, _ -> b
    | _, E -> a
    | L (k1, l1), L(k2, l2) when k1==k2 ->
      mk_leaf_ k1 (union_list_ l1 l2)  (* merge leaves *)
    | L (k, l), _ -> add_list_hash_ k l b
    | _, L (k, l) -> add_list_hash_ k l a
    | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
      if p1 = p2 && m1 = m2
      then mk_node_ p1 m1 (union l1 l2) (union r1 r2)
      else if m1 < m2 && is_prefix_ ~prefix:p2 p1 ~bit:m1
      then if bit_is_0_ p2 ~bit:m1
        then hashcons_ (N (p1, m1, union l1 b, r1))
        else hashcons_ (N (p1, m1, l1, union r1 b))
      else if m1 > m2 && is_prefix_ ~prefix:p1 p2 ~bit:m2
      then if bit_is_0_ p1 ~bit:m2
        then hashcons_ (N (p2, m2, union l2 a, r2))
        else hashcons_ (N (p2, m2, l2, union r2 a))
      else join_ a p1 b p2

  (*$Q & ~count:50
    Q.(list int) (fun l -> \
      let module S = Make(CCInt) in \
      let s = S.of_list l in S.equal s (S.union s s))
  *)

  (*$= & ~printer:(CCPrint.to_string (CCList.pp CCInt.pp))
    [1;2;4;5;6;7;8;10] (let module S = Make(CCInt) in \
    let s1 = S.of_list [1;2;4;5;  7;8   ] in \
    let s2 = S.of_list [  2;4;  6;7;  10] in \
    S.union s1 s2 |> S.to_list |> List.sort compare )
  *)

  let rec inter_list_ l1 l2 = match l1, l2 with
    | [], _
    | _, [] -> []
    | x1 :: tl1, x2 :: tl2 ->
      match E.compare x1 x2 with
      | 0 -> x1 :: inter_list_ tl1 tl2
      | c when c<0 -> inter_list_ tl1 l2
      | _ -> inter_list_ l1 tl2

  let rec inter a b = match a.cell, b.cell with
    | E, _ | _, E -> empty
    | L (k1, l1), L (k2, l2) when k1==k2 ->
      mk_leaf_ k1 (inter_list_ l1 l2)
    | L (k,l), _ ->
      mk_leaf_ k (List.filter (fun x -> mem_rec_ k x b) l)
    | _, L (k,l) ->
      mk_leaf_ k (List.filter (fun x -> mem_rec_ k x a) l)
    | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
      if p1 = p2 && m1 = m2
      then mk_node_ p1 m1 (inter l1 l2) (inter r1 r2)
      else if m1 < m2 && is_prefix_ ~prefix:p2 p1 ~bit:m1
      then if bit_is_0_ p2 ~bit:m1
        then inter l1 b
        else inter r1 b
      else if m1 > m2 && is_prefix_ ~prefix:p1 p2 ~bit:m2
      then if bit_is_0_ p1 ~bit:m2
        then inter a l2
        else inter a r2
      else empty

  (*$Q
    Q.(list_of_size Gen.(0 -- 300) int) (fun l -> \
      let module S = Make(CCInt) in \
      let s = S.of_list l in S.equal s (S.inter s s))
  *)

  (*$= & ~printer:(CCPrint.to_string (CCList.pp CCInt.pp))
    [2;4;7] (let module S = Make(CCInt) in \
    let s1 = S.of_list [1;2;4;5;  7;8   ] in \
    let s2 = S.of_list [  2;4;  6;7;  10] in \
    S.inter s1 s2 |> S.to_list |> List.sort compare )
  *)

  (* remove elements of [l] from [t]; they all have hash [k] *)
  let rec remove_list_hash_ k l t = match l with
    | [] -> t
    | x :: tl ->
      remove_list_hash_ k tl (remove_rec_ k x t)

  let rec diff_list_ l1 l2 = match l1, l2 with
    | [], _ -> []
    | _, [] -> l1
    | x1 :: tl1, x2 :: tl2 ->
      match E.compare x1 x2 with
      | 0 -> diff_list_ tl1 tl2
      | c when c<0 -> x1 :: diff_list_ tl1 l2
      | _ -> diff_list_ l1 tl2

  let rec diff a b = match a.cell, b.cell with
    | E, _ -> empty
    | _, E -> a
    | L (k1, l1), L (k2, l2) when k1==k2 ->
      mk_leaf_ k1 (diff_list_ l1 l2)
    | L (k,l), _ ->
      mk_leaf_ k (List.filter (fun x -> not (mem_rec_ k x b)) l)
    | _, L (k,l) -> remove_list_hash_ k l a
    | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
      if p1 = p2 && m1 = m2
      then mk_node_ p1 m1 (diff l1 l2) (diff r1 r2)
      else if m1 < m2 && is_prefix_ ~prefix:p2 p1 ~bit:m1
      then if bit_is_0_ p2 ~bit:m1
        then hashcons_ (N (p1, m1, diff l1 b, r1))
        else hashcons_ (N (p1, m1, l1, diff r1 b))
      else if m1 > m2 && is_prefix_ ~prefix:p1 p2 ~bit:m2
      then if bit_is_0_ p1 ~bit:m2
        then diff a l2
        else diff a r2
      else a

  (*$= & ~printer:(CCPrint.to_string (CCList.pp CCInt.pp))
    [1;5;8] (let module S = Make(CCInt) in \
    let s1 = S.of_list [1;2;4;5;  7;8   ] in \
    let s2 = S.of_list [  2;4;  6;7;  10] in \
    S.diff s1 s2 |> S.to_list |> List.sort compare )
  *)

  (** {2 Whole-collection operations} *)

  type 'a sequence = ('a -> unit) -> unit
  type 'a gen = unit -> 'a option

  let add_list t l = List.fold_left (fun t x -> add x t) t l

  let of_list l = add_list empty l

  let to_list t = fold (fun x l -> x:: l) t []

  (*$Q & ~count:50
    Q.(list int) (fun l -> \
      let module S = Make(CCInt) in \
      S.of_list l |> S.cardinal = List.length l)
  *)

  let add_seq t seq =
    let t = ref t in
    seq (fun x -> t := add x !t);
    !t

  let of_seq seq = add_seq empty seq

  let to_seq t yield = iter yield t
end
