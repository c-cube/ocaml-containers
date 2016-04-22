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

(** Masks with exactly one bit active *)
module Bit : sig
  type t = private int
  val highest : int -> t
  val min_int : t
  val is_0 : bit:t -> int -> bool
  val is_1 : bit:t -> int -> bool
  val mask : mask:t -> int -> int   (* zeroes the bit, puts all lower bits to 1 *)
  val lt : t -> t -> bool
  val gt : t -> t -> bool
end = struct
  type t = int

  let min_int = min_int

  let rec highest_bit_naive x m =
    if x=m then m
    else highest_bit_naive (x land (lnot m)) (2*m)

  let mask_20_ = 1 lsl 20
  let mask_40_ = 1 lsl 40

  let highest x =
    if x<0 then min_int
    else if Sys.word_size > 40 && x > mask_40_
    then (* remove least significant 40 bits *)
      let x' = x land (lnot (mask_40_ -1)) in
      highest_bit_naive x' mask_40_
    else if x> mask_20_
    then (* small shortcut: remove least significant 20 bits *)
      let x' = x land (lnot (mask_20_ -1)) in
      highest_bit_naive x' mask_20_
    else highest_bit_naive x 1

  let is_0 ~bit x = x land bit = 0
  let is_1 ~bit x = x land bit = bit

  let mask ~mask x = (x lor (mask -1)) land (lnot mask)
  (* low endian: let mask_ x ~mask = x land (mask - 1) *)

  let gt a b = (b != min_int) && (a = min_int || a > b)
  let lt a b = gt b a
end

type 'a t =
  | E  (* empty *)
  | L of int * 'a  (* leaf *)
  | N of int (* common prefix *) * Bit.t (* bit switch *) * 'a t * 'a t

let empty = E

let is_prefix_ ~prefix y ~bit = prefix = Bit.mask y ~mask:bit

(*$inject
  let _list_uniq = CCList.sort_uniq ~cmp:(fun a b-> Pervasives.compare (fst a)(fst b))
 *)

(*$Q
  Q.int (fun i -> \
    let b = Bit.highest i in \
    ((b:>int) land i = (b:>int)) && (i < 0 || ((b:>int) <= i && (i-(b:>int)) < (b:>int))))
  Q.int (fun i -> (Bit.highest i = Bit.min_int) = (i < 0))
  Q.int (fun i -> ((Bit.highest i:>int) < 0) = (Bit.highest i = Bit.min_int))
  Q.int (fun i -> let j = (Bit.highest i :> int) in  j land (j-1) = 0)
*)

(*$T
  (Bit.highest min_int :> int) = min_int
  (Bit.highest 2 :> int) = 2
  (Bit.highest 17 :> int)  = 16
  (Bit.highest 300 :> int) = 256
 *)

(* helper:

    let b_of_i i =
      let rec f acc i =
        if i=0 then acc else let q, r = i/2, abs (i mod 2)
      in
      f (r::acc) q in f [] i;;
*)

(* low endian: let branching_bit_ a _ b _ = lowest_bit_ (a lxor b) *)
let branching_bit_ a b = Bit.highest (a lxor b)

(* TODO use hint in branching_bit_ *)

let check_invariants t =
  (* check that keys are prefixed by every node in their path *)
  let rec check_keys path t = match t with
    | E -> true
    | L (k, _) ->
        List.for_all
          (fun (prefix, switch, side) ->
            is_prefix_ ~prefix k ~bit:switch
            &&
            match side with
            | `Left -> Bit.is_0 k ~bit:switch
            | `Right -> Bit.is_1 k ~bit:switch
          ) path
    | N (prefix, switch, l, r) ->
        check_keys ((prefix, switch, `Left) :: path) l
        &&
        check_keys ((prefix, switch, `Right) :: path) r
  in
  check_keys [] t

(*$Q
  Q.(list (pair int bool)) (fun l -> \
    check_invariants (of_list l))
*)

let rec find_exn k t = match t with
  | E -> raise Not_found
  | L (k', v) when k = k' -> v
  | L _ -> raise Not_found
  | N (prefix, m, l, r) ->
    if is_prefix_ ~prefix k ~bit:m
    then if Bit.is_0 k ~bit:m
      then find_exn k l
      else find_exn k r
    else raise Not_found

    (* XXX could test with lt_unsigned_? *)

    (*
    if k <= prefix (* search tree *)
    then find_exn k l
    else find_exn k r
       *)

let find k t =
  try Some (find_exn k t)
  with Not_found -> None

(*$Q
  Q.(list (pair int int)) (fun l -> \
    let l = _list_uniq l in \
    let m = of_list l in \
    List.for_all (fun (k,v) -> find k m = Some v) l)
*)

let mem k t =
  try ignore (find_exn k t); true
  with Not_found -> false

(*$Q
  Q.(list (pair int int)) (fun l -> \
    let m = of_list l in \
    List.for_all (fun (k,_) -> mem k m) l)
*)

let mk_node_ prefix switch l r = match l, r with
  | E, o | o, E -> o
  | _ -> N (prefix, switch, l, r)

(* join trees t1 and t2 with prefix p1 and p2 respectively
  (p1 and p2 do not overlap) *)
let join_ t1 p1 t2 p2 =
  let switch = branching_bit_ p1 p2 in
  let prefix = Bit.mask p1 ~mask:switch in
  if Bit.is_0 p1 ~bit:switch
  then (
    assert (Bit.is_1 p2 ~bit:switch);
    mk_node_ prefix switch t1 t2
  ) else (
    assert (Bit.is_0 p2 ~bit:switch);
    mk_node_ prefix switch t2 t1
  )

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
    then if Bit.is_0 k ~bit:switch
      then N(prefix, switch, insert_ c k v l, r)
      else N(prefix, switch, l, insert_ c k v r)
    else join_ (L(k,v)) k t prefix

let add k v t = insert_ (fun ~old:_ v -> v) k v t

(*$Q & ~count:20
  Q.(list (pair int int)) (fun l -> \
    let l = _list_uniq l in let m = of_list l in \
    List.for_all (fun (k,v) -> find_exn k m = v) l)
*)

let rec remove k t = match t with
  | E -> E
  | L (k', _) -> if k=k' then E else t
  | N (prefix, switch, l, r) ->
    if is_prefix_ ~prefix k ~bit:switch
    then if Bit.is_0 k ~bit:switch
      then mk_node_ prefix switch (remove k l) r
      else mk_node_ prefix switch l (remove k r)
    else t (* not present *)

(*$Q & ~count:20
  Q.(list (pair int int)) (fun l -> \
    let l =  _list_uniq l in let m = of_list l in \
    List.for_all (fun (k,_) -> mem k m && not (mem k (remove k m))) l)
*)

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

(*$= & ~printer:Q.Print.(list (pair int int))
  [1,1; 2, 22; 3, 3] \
  (of_list [1,1;2,2;3,3] \
    |> update 2 (function None -> assert false | Some _ -> Some 22) \
    |> to_list |> List.sort Pervasives.compare)
*)

let doubleton k1 v1 k2 v2 = add k1 v1 (singleton k2 v2)

let rec equal ~eq a b = match a, b with
  | E, E -> true
  | L (ka, va), L (kb, vb) -> ka = kb && eq va vb
  | N (pa, sa, la, ra), N (pb, sb, lb, rb) ->
      pa=pb && sa=sb && equal ~eq la lb && equal ~eq ra rb
  | E, _
  | N _, _
  | L _, _ -> false

(*$Q
  Q.(list (pair int bool)) ( fun l -> \
    equal ~eq:(=) (of_list l) (of_list (List.rev l)))
*)

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

let rec mapi f t = match t with
  | E -> E
  | L (k, v) -> L (k, f k v)
  | N (p, s, l, r) ->
      N (p, s, mapi f l, mapi f r)

let rec map f t = match t with
  | E -> E
  | L (k, v) -> L (k, f v)
  | N (p, s, l, r) ->
      N (p, s, map f l, map f r)

let rec choose_exn = function
  | E -> raise Not_found
  | L (k, v) -> k, v
  | N (_, _, l, _) -> choose_exn l

let choose t =
  try Some (choose_exn t)
  with Not_found -> None

let rec union f t1 t2 = match t1, t2 with
  | E, o | o, E -> o
  | L (k, v), o
  | o, L (k, v) ->
    (* insert k, v into o *)
    insert_ (fun ~old v -> f k old v) k v o
  | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
    if p1 = p2 && m1 = m2
    then mk_node_ p1 m1 (union f l1 l2) (union f r1 r2)
    else if Bit.gt m1 m2 && is_prefix_ ~prefix:p1 p2 ~bit:m1
      then if Bit.is_0 p2 ~bit:m1
        then N (p1, m1, union f l1 t2, r1)
        else N (p1, m1, l1, union f r1 t2)
    else if Bit.lt m1 m2 && is_prefix_ ~prefix:p2 p1 ~bit:m2
      then if Bit.is_0 p1 ~bit:m2
        then N (p2, m2, union f t1 l2, r2)
        else N (p2, m2, l2, union f t1 r2)
    else join_ t1 p1 t2 p2

(*$Q & ~small:(fun (a,b) -> List.length a + List.length b)
  Q.(pair (list (pair int bool)) (list (pair int bool))) (fun (l1,l2) -> \
    check_invariants (union (fun _ _ x -> x) (of_list l1) (of_list l2)))
  Q.(pair (list (pair int bool)) (list (pair int bool))) (fun (l1,l2) -> \
    check_invariants (inter (fun _ _ x -> x) (of_list l1) (of_list l2)))
*)

(* associativity of union *)
(*$Q & ~small:(fun (a,b,c) -> List.(length a + length b + length c))
  Q.(let p = list (pair int int) in triple p p p) (fun (l1,l2,l3) -> \
    let m1 = of_list l1 and m2 = of_list l2 and m3 = of_list l3 in \
    let f _ x y = max x y in \
    equal ~eq:(=) (union f (union f m1 m2) m3) (union f m1 (union f m2 m3)))
*)

(*$R
  assert_equal ~cmp:(equal ~eq:(=)) ~printer:(CCFormat.to_string (print CCString.print))
    (of_list [1, "1"; 2, "2"; 3, "3"; 4, "4"])
    (union (fun _ a b -> a)
      (of_list [1, "1"; 3, "3"]) (of_list [2, "2"; 4, "4"]));
*)

(*$R
  assert_equal ~cmp:(equal ~eq:(=)) ~printer:(CCFormat.to_string (print CCString.print))
    (of_list [1, "1"; 2, "2"; 3, "3"; 4, "4"])
    (union (fun _ a b -> a)
      (of_list [1, "1"; 2, "2"; 3, "3"]) (of_list [2, "2"; 4, "4"]))
*)

(*$Q
   Q.(list (pair int bool)) (fun l -> \
    equal ~eq:(=) (of_list l) (union (fun _ a _ -> a) (of_list l)(of_list l)))
*)

let rec inter f a b = match a, b with
  | E, _ | _, E -> E
  | L (k, v), o
  | o, L (k, v) ->
    begin try
      let v' = find_exn k o in
      L (k, f k v v')
    with Not_found -> E
    end
  | N (p1, m1, l1, r1), N (p2, m2, l2, r2) ->
    if p1 = p2 && m1 = m2
    then mk_node_ p1 m1 (inter f l1 l2) (inter f r1 r2)
    else if Bit.gt m1 m2 && is_prefix_ ~prefix:p1 p2 ~bit:m1
      then if Bit.is_0 p2 ~bit:m1
        then inter f l1 b
        else inter f r1 b
    else if Bit.lt m1 m2 && is_prefix_ ~prefix:p2 p1 ~bit:m2
      then if Bit.is_0 p1 ~bit:m2
        then inter f l2 a
        else inter f r2 a
    else E

(*$R
  assert_equal ~cmp:(equal ~eq:(=)) ~printer:(CCFormat.to_string (print CCString.print))
    (singleton 2 "2")
    (inter (fun _ a b -> a)
      (of_list [1, "1"; 2, "2"; 3, "3"]) (of_list [2, "2"; 4, "4"]))
*)

(*$Q
   Q.(list (pair int bool)) (fun l -> \
    equal ~eq:(=) (of_list l) (inter (fun _ a _ -> a) (of_list l)(of_list l)))
*)

(* associativity of inter *)
(*$Q & ~small:(fun (a,b,c) -> List.(length a + length b + length c))
  Q.(let p = list (pair int int) in triple p p p) (fun (l1,l2,l3) -> \
    let m1 = of_list l1 and m2 = of_list l2 and m3 = of_list l3 in \
    let f _ x y = max x y in \
    equal ~eq:(=) (inter f (inter f m1 m2) m3) (inter f m1 (inter f m2 m3)))
*)


(** {2 Whole-collection operations} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

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

let rec add_gen m g =  match g() with
  | None -> m
  | Some (k,v) -> add_gen (add k v m) g

let of_gen g = add_gen empty g

let to_gen m =
  let st = Stack.create () in
  Stack.push m st;
  let rec next() =
    if Stack.is_empty st then None
    else explore (Stack.pop st)
  and explore n = match n with
    | E -> next()  (* backtrack *)
    | L (k,v) -> Some (k,v)
    | N (_, _, l, r) ->
        Stack.push r st;
        explore l
  in
  next

(*$T
  doubleton 1 "a" 2 "b" |> to_gen |> of_gen |> to_list \
    |> List.sort Pervasives.compare = [1, "a"; 2, "b"]
*)

(*$Q
  Q.(list (pair int bool)) (fun l -> \
    let m = of_list l in equal ~eq:(=) m (m |> to_gen |> of_gen))
*)

(* E < L < N; arbitrary order for switches *)
let compare ~cmp a b =
  let rec cmp_gen cmp a b = match a(), b() with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some (ka, va), Some (kb, vb) ->
        if ka=kb
          then
            let c = cmp va vb in
            if c=0 then cmp_gen cmp a b else c
          else Pervasives.compare ka kb
  in
  cmp_gen cmp (to_gen a) (to_gen b)

(*$Q
  Q.(list (pair int bool)) ( fun l -> \
    let m1 = of_list l and m2 = of_list (List.rev l) in \
    compare ~cmp:Pervasives.compare m1 m2 = 0)

*)

(*$QR
  Q.(pair (list (pair int bool)) (list (pair int bool))) (fun (l1, l2) ->
    let l1 = List.map (fun (k,v) -> abs k,v) l1 in
    let l2 = List.map (fun (k,v) -> abs k,v) l2 in
    let m1 = of_list l1 and m2 = of_list l2 in
    let c = compare ~cmp:Pervasives.compare m1 m2
    and c' = compare ~cmp:Pervasives.compare m2 m1 in
    (c = 0) = (c' = 0) && (c < 0) = (c' > 0) && (c > 0) = (c' < 0))
*)

(*$QR
  Q.(pair (list (pair int bool)) (list (pair int bool))) (fun (l1, l2) ->
    let l1 = List.map (fun (k,v) -> abs k,v) l1 in
    let l2 = List.map (fun (k,v) -> abs k,v) l2 in
    let m1 = of_list l1 and m2 = of_list l2 in
    (compare ~cmp:Pervasives.compare m1 m2 = 0) = equal ~eq:(=) m1 m2)
*)

let rec add_klist m l = match l() with
  | `Nil -> m
  | `Cons ((k,v), tl) -> add_klist (add k v m) tl

let of_klist l = add_klist empty l

let to_klist m =
  (* [st]: stack of alternatives *)
  let rec explore st m () = match m with
    | E -> next st ()
    | L (k,v) -> `Cons ((k, v), next st)
    | N (_, _, l, r) -> explore (r::st) l ()
  and next st () = match st with
    | [] -> `Nil
    | x :: st' -> explore st' x ()
  in
  next [m]

(*$Q
  Q.(list (pair int bool)) (fun l -> \
    let m = of_list l in equal ~eq:(=) m (m |> to_klist |> of_klist))
*)

type 'a tree = unit -> [`Nil | `Node of 'a * 'a tree list]

let rec as_tree t () = match t with
  | E -> `Nil
  | L (k, v) -> `Node (`Leaf (k, v), [])
  | N (prefix, switch, l, r) ->
    `Node (`Node (prefix, (switch:>int)), [as_tree l; as_tree r])

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

let print pp_x out m =
  Format.fprintf out "@[<hov2>intmap {@,";
  let first = ref true in
  iter
    (fun k v ->
      if !first then first := false else Format.pp_print_string out ", ";
      Format.fprintf out "%d -> " k;
      pp_x out v;
      Format.pp_print_cut out ()
    ) m;
  Format.fprintf out "}@]"
