
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

(** {1 Map specialized for Int keys} *)

(* "Fast Mergeable Integer Maps", Okasaki & Gill.
We use big-endian trees. *)

type 'a t =
  | E  (* empty *)
  | L of int * 'a  (* leaf *)
  | N of int (* common prefix *) * int (* bit switch *) * 'a t * 'a t

let empty = E

let bit_is_0_ x ~bit = x land bit = 0

let mask_ x ~mask = (x lor (mask -1)) land (lnot mask)
(* low endian: let mask_ x ~mask = x land (mask - 1) *)

let is_prefix_ ~prefix y ~bit = prefix = mask_ y ~mask:bit

(* loop down until x=lowest_bit_ x *)
let rec highest_bit_naive x m =
  if m = 0 then 0
  else if x land m = 0 then highest_bit_naive x (m lsr 1)
  else m

let highest_bit =
  (* the highest representable 2^n *)
  let max_log = 1 lsl (Sys.word_size - 2) in
  fun x ->
    if x > 1 lsl 20
    then (* small shortcut: remove least significant 20 bits *)
      let x' = x land (lnot ((1 lsl 20) -1)) in
      highest_bit_naive x' max_log
    else highest_bit_naive x max_log

(*$Q
  Q.int (fun i -> \
    let b = highest_bit i in \
    i < 0 || (b <= i && (i-b) < b))
*)

(* helper:

    let b_of_i i =
      let rec f acc i =
        if i=0 then acc else let q, r = i/2, i mod 2
      in
      f (r::acc) q in f [] i;;
*)

(* low endian: let branching_bit_ a _ b _ = lowest_bit_ (a lxor b) *)
let branching_bit_ a b =
  highest_bit (a lxor b)

let rec find_exn k t = match t with
  | E -> raise Not_found
  | L (k', v) when k = k' -> v
  | L _ -> raise Not_found
  | N (prefix, m, l, r) ->
    if is_prefix_ ~prefix k ~bit:m
    then if bit_is_0_ k ~bit:m
      then find_exn k l
      else find_exn k r
    else raise Not_found

    (* FIXME: valid if k < 0?
    if k <= prefix (* search tree *)
    then find_exn k l
    else find_exn k r
       *)

let find k t =
  try Some (find_exn k t)
  with Not_found -> None

let mem k t =
  try ignore (find_exn k t); true
  with Not_found -> false

let mk_node_ prefix switch l r = match l, r with
  | E, o | o, E -> o
  | _ -> N (prefix, switch, l, r)

(* join trees t1 and t2 with prefix p1 and p2 respectively
  (p1 and p2 do not overlap) *)
let join_ t1 p1 t2 p2 =
  let switch = branching_bit_ p1 p2 in
  let prefix = mask_ p1 ~mask:switch in
  if bit_is_0_ p1 ~bit:switch
  then mk_node_ prefix switch t1 t2
  else (assert (bit_is_0_ p2 ~bit:switch); mk_node_ prefix switch t2 t1)

let singleton k v = L (k, v)

(* c: conflict function *)
let rec insert_ c k v t = match t with
  | E -> L (k, v)
  | L (k', v') ->
    if k=k'
    then L (k, c ~old:v' v)
    else join_ t k' (L (k, v)) k
  | N (prefix, switch, l, r) ->
    if is_prefix_ ~prefix k ~bit:switch
    then if bit_is_0_ k ~bit:switch
      then N(prefix, switch, insert_ c k v l, r)
      else N(prefix, switch, l, insert_ c k v r)
    else join_ (L(k,v)) k t prefix

let add k v t = insert_ (fun ~old:_ v -> v) k v t

(*$Q
  Q.(list (pair int int)) (fun l -> \
    let l = CCList.Set.uniq l in let m = of_list l in \
    List.for_all (fun (k,v) -> find_exn k m = v) l)
*)

let rec remove k t = match t with
  | E -> E
  | L (k', _) -> if k=k' then E else t
  | N (prefix, switch, l, r) ->
    if is_prefix_ ~prefix k ~bit:switch
    then if bit_is_0_ k ~bit:switch
      then mk_node_ prefix switch (remove k l) r
      else mk_node_ prefix switch l (remove k r)
    else t (* not present *)

let update k f t =
  try
    let v = find_exn k t in
    begin match f (Some v) with
      | None -> remove k t
      | Some v' -> add k v' t
    end
  with Not_found ->
    match f None with
    | None -> t
    | Some v -> add k v t

let doubleton k1 v1 k2 v2 = add k1 v1 (singleton k2 v2)

let rec iter f t = match t with
  | E -> ()
  | L (k, v) -> f k v
  | N (_, _, l, r) -> iter f l; iter f r

let rec fold f t acc = match t with
  | E -> acc
  | L (k, v) -> f k v acc
  | N (_, _, l, r) ->
    let acc = fold f l acc in
    fold f r acc

let cardinal t = fold (fun _ _ n -> n+1) t 0

let rec choose_exn = function
  | E -> raise Not_found
  | L (k, v) -> k, v
  | N (_, _, l, _) -> choose_exn l

let choose t =
  try Some (choose_exn t)
  with Not_found -> None

let union _ _ _ = assert false

let inter _ _ _ = assert false

(** {2 Whole-collection operations} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_list t l = List.fold_left (fun t (k,v) -> add k v t) t l

let of_list l = add_list empty l

let to_list t = fold (fun k v l -> (k,v) :: l) t []

(*$Q
  Q.(list (pair int int)) (fun l -> \
    let l = List.map (fun (k,v) -> abs k,v) l in \
    let rec is_sorted = function [] | [_] -> true \
      | x::y::tail -> x <= y && is_sorted (y::tail) in \
    of_list l |> to_list |> List.rev_map fst |> is_sorted)
*)

(*$Q
  Q.(list (pair int int)) (fun l -> \
    of_list l |> cardinal = List.length l)
  *)

let add_seq t seq =
  let t = ref t in
  seq (fun (k,v) -> t := add k v !t);
  !t

let of_seq seq = add_seq empty seq

let to_seq t yield = iter (fun k v -> yield (k,v)) t

let keys t yield = iter (fun k _ -> yield k) t

let values t yield = iter (fun _ v -> yield v) t

type 'a tree = unit -> [`Nil | `Node of 'a * 'a tree list]

let rec as_tree t () = match t with
  | E -> `Nil
  | L (k, v) -> `Node (`Leaf (k, v), [])
  | N (prefix, switch, l, r) ->
    `Node (`Node (prefix, switch), [as_tree l; as_tree r])
