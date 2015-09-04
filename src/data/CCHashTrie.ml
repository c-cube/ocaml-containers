
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Tries} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Fixed-Size Arrays} *)
module type FIXED_ARRAY = sig
  type 'a t
  val create : empty:'a -> 'a t
  val length_log : int
  val length : int  (* 2 power length_log *)
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val update : 'a t -> int -> ('a -> 'a) -> 'a t
  val remove : empty:'a -> 'a t -> int -> 'a t (* put back [empty] there *)
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

(* TODO: add update again, to call popcount only once *)

module type S = sig
  module A : FIXED_ARRAY

  type key

  type 'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val singleton : key -> 'a -> 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if key not present *)

  val remove : key -> 'a t -> 'a t

  val cardinal : _ t -> int

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b

  (** {6 Conversions} *)

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val of_list : (key * 'a) list -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t

  val of_seq : (key * 'a) sequence -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence

  (** {6 IO} *)

  val print : key printer -> 'a printer -> 'a t printer

  val as_tree : 'a t -> [`L of int * (key * 'a) list | `N ] ktree
  (** For debugging purpose: explore the structure of the tree,
      with [`L (h,l)] being a leaf (with shared hash [h])
      and [`N] an inner node *)
end

module type KEY = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Arrays} *)

(* regular array of 32 elements *)
module A32 : FIXED_ARRAY = struct
  type 'a t = 'a array

  let length_log = 5

  let length = 1 lsl length_log  (* 32 *)

  let create ~empty:x = Array.make length x

  let get a i = Array.get a i

  let set a i x =
    let a' = Array.copy a in
    a'.(i) <- x;
    a'

  let update a i f = set a i (f (get a i))

  let remove ~empty a i =
    let a' = Array.copy a in
    a'.(i) <- empty;
    a'

  let iter = Array.iter

  let fold = Array.fold_left
end

  (*
  from https://en.wikipedia.org/wiki/Hamming_weight

  //This uses fewer arithmetic operations than any other known  
  //implementation on machines with fast multiplication.
  //It uses 12 arithmetic operations, one of which is a multiply.
  int popcount_3(uint64_t x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits 
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits 
    return (x * h01)>>56;  //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ... 
  }
*)

let popcount64 (b:int64) =
  let open Int64 in
  let b = sub b (logand (shift_right_logical b 1) 0x5555555555555555L) in
  let b = add (logand b 0x3333333333333333L)
    (logand (shift_right_logical b 2) 0x3333333333333333L) in
  let b = logand (add b (shift_right_logical b 4)) 0x0F0F0F0F0F0F0F0FL in
  let b = shift_right_logical (mul b 0x0101010101010101L) 56 in
  Int64.to_int b

(*$T
  popcount64 5L = 2
  popcount64 256L = 1
  popcount64 255L = 8
  popcount64 0xFFFFFFFFL = 32
  popcount64 0xFFFFFFFFFFFFFFFFL = 64
*)

(*$Q
  Q.int (fun i -> \
    let i = Int64.of_int i in popcount64 i <= 64)
  *)

(* sparse array, using a bitfield and POPCOUNT *)
module A_SPARSE : FIXED_ARRAY = struct
  type 'a t = {
    bits: int64;
    arr: 'a array;
    empty: 'a;
  }

  let length_log = 6
  let length = 1 lsl length_log

  let popcount = popcount64

  let create ~empty = { bits=0L; arr= [| |]; empty; }

  let get a i =
    let open Int64 in
    let idx = shift_left 1L i in
    if logand a.bits idx = 0L
    then a.empty
    else
      let real_idx =popcount (logand a.bits (sub idx 1L)) in
      a.arr.(real_idx)

  let set a i x =
    let open Int64 in
    let idx = shift_left 1L i in
    let real_idx = popcount (logand a.bits (sub idx 1L)) in
    if logand a.bits idx = 0L
    then (
      (* insert at [real_idx] in a new array *)
      let bits = logor a.bits idx in
      let arr = Array.init (Array.length a.arr + 1)
        (fun j ->
          if j<real_idx then a.arr.(j)
          else if j=real_idx then x
          else a.arr.(j-1)
        ) in
      {a with bits; arr}
    ) else (
      (* replace element at [real_idx] *)
      let arr = Array.copy a.arr in
      arr.(real_idx) <- x;
      {a with arr}
    )

  let update a i f =
    let open Int64 in
    let idx = shift_left 1L i in
    let real_idx = popcount (logand a.bits (sub idx 1L)) in
    if logand a.bits idx = 0L
    then (
      (* not present *)
      let x = f a.empty in
      (* insert at [real_idx] in a new array *)
      let bits = logor a.bits idx in
      let arr = Array.init (Array.length a.arr + 1)
        (fun j ->
          if j<real_idx then a.arr.(j)
          else if j=real_idx then x
          else a.arr.(j-1)
        ) in
      {a with bits; arr}
    ) else (
      let x = f a.arr.(real_idx) in
      (* replace element at [real_idx] *)
      let arr = Array.copy a.arr in
      arr.(real_idx) <- x;
      {a with arr}
    )

  let remove ~empty:_ a i =
    let open Int64 in
    let idx = shift_left 1L i in
    let real_idx = popcount (logand a.bits (sub idx 1L)) in
    if logand a.bits idx = 0L
    then a (* not present *)
    else (
      (* remove at [real_idx] *)
      let bits = logand a.bits (lognot idx) in
      let arr = Array.init (Array.length a.arr - 1)
        (fun j ->
          if j>= real_idx then a.arr.(j+1) else a.arr.(j)
        ) in
      {a with bits; arr}
    )

  let iter f a = Array.iter f a.arr

  let fold f acc a = Array.fold_left f acc a.arr
end

(** {2 Functors} *)

module Make(Key : KEY)
: S with type key = Key.t
= struct
  module A = A_SPARSE

  let () = assert (A.length = 1 lsl A.length_log)

  module Hash : sig
    type t = private int
    val make : Key.t -> t
    val zero : t (* special "hash" *)
    val is_0 : t -> bool
    val rem : t -> int (* [A.length_log] last bits *)
    val quotient : t -> t (* remove [A.length_log] last bits *)
  end = struct
    type t = int
    let make = Key.hash
    let zero = 0
    let is_0 h = h==0
    let rem h = h land (A.length - 1)
    let quotient h = h lsr A.length_log
  end

  let hash_ = Hash.make

  type key = Key.t

  (* association list, without duplicates *)
  type 'a leaf =
    | Nil
    | Cons of key * 'a * 'a leaf

  type 'a t =
    | E
    | L of Hash.t * 'a leaf (* same hash for all elements *)
    | N of 'a leaf * 'a t A.t  (* leaf for hash=0, subnodes *)

  (* invariants:
      L [] --> E
      N [E, E,...., E] -> E
  *)

  let empty = E

  let is_empty = function
    | E -> true
    | L (_, Nil) -> assert false
    | L _
    | N _ -> false

  let leaf_ k v ~h = L (h, Cons(k,v,Nil))

  let singleton k v = leaf_ k v ~h:(hash_ k)

  let rec get_exn_list_ k l = match l with
    | Nil -> raise Not_found
    | Cons (k', v', tail) ->
        if Key.equal k k' then v' else get_exn_list_ k tail

  let rec get_exn_ k ~h m = match m with
    | E -> raise Not_found
    | L (_, l) -> get_exn_list_ k l
    | N (leaf, a) ->
        if Hash.is_0 h then get_exn_list_ k leaf
        else
          let i = Hash.rem h in
          let h' = Hash.quotient h in
          get_exn_ k ~h:h' (A.get a i)

  let get_exn k m = get_exn_ k ~h:(hash_ k) m

  let get k m =
    try Some (get_exn_ k ~h:(hash_ k) m)
    with Not_found -> None

  (* TODO: use Hash.combine if array only has one non-empty LEAF element? *)

  (* [left] list nodes already visited *)
  let rec add_list_ k v l = match l with
    | Nil -> Cons (k, v, Nil)
    | Cons (k', v', tail) ->
        if Key.equal k k'
        then Cons (k, v, tail) (* replace *)
        else Cons (k', v', add_list_ k v tail)

  (* [h]: hash, with the part required to reach this leaf removed *)
  let rec add_ k v ~h m = match m with
    | E -> leaf_ k v ~h
    | L (h', l) ->
        if h=h'
        then L (h, add_list_ k v l)
        else (* split into N *)
          let a = A.create ~empty:E in
          let a, leaf =
            if Hash.is_0 h' then a, l
            else
              (* put leaf in the right bucket *)
              let i = Hash.rem h' in
              let h'' = Hash.quotient h' in
              A.set a i (L (h'', l)), Nil
          in
          (* then add new node *)
          let a, leaf =
            if Hash.is_0 h then a, add_list_ k v leaf
            else add_to_array_ k v ~h a, leaf
          in
          N (leaf, a)
    | N (leaf, a) ->
        if Hash.is_0 h
        then N (add_list_ k v leaf, a)
        else N (leaf, add_to_array_ k v ~h a)

  (* add k->v to [a] *)
  and add_to_array_ k v ~h a =
    (* insert in a bucket *)
    let i = Hash.rem h in
    let h' = Hash.quotient h in
    A.update a i (fun x -> add_ k v ~h:h' x)

  let add k v m = add_ k v ~h:(hash_ k) m

  exception LocalExit

  let is_empty_arr_ a =
    try
      A.iter (fun t -> if not (is_empty t) then raise LocalExit) a;
      true
    with LocalExit -> false

  let is_empty_list_ = function
    | Nil -> true
    | Cons _ -> false

  let rec remove_list_ k l = match l with
    | Nil -> Nil
    | Cons (k', v', tail) ->
        if Key.equal k k'
          then tail
          else Cons (k', v', remove_list_ k tail)

  let rec remove_rec_ k ~h m = match m with
    | E -> E
    | L (h, l) ->
        let l = remove_list_ k l in
        if is_empty_list_ l then E else L (h, l)
    | N (leaf, a) ->
        let leaf, a =
          if Hash.is_0 h
            then remove_list_ k leaf, a
            else
              let i = Hash.rem h in
              let h' = Hash.quotient h in
              let new_t = remove_rec_ k ~h:h' (A.get a i) in
              if is_empty new_t
              then leaf, A.remove ~empty:E a i (* remove sub-tree *)
              else leaf, A.set a i new_t
        in
        if is_empty_list_ leaf && is_empty_arr_ a
          then E
          else N (leaf, a)

  let remove k m = remove_rec_ k ~h:(hash_ k) m

  let iter f t =
    let rec aux = function
      | E -> ()
      | L (_,l) -> aux_list l
      | N (l,a) -> aux_list l; A.iter aux a
    and aux_list = function
      | Nil -> ()
      | Cons (k, v, tl) -> f k v; aux_list tl
    in
    aux t

  let fold f acc t =
    let rec aux acc t = match t with
      | E -> acc
      | L (_,l) -> aux_list acc l
      | N (l,a) -> let acc = aux_list acc l in A.fold aux acc a
    and aux_list acc l = match l with
      | Nil -> acc
      | Cons (k, v, tl) -> let acc = f acc k v in aux_list acc tl
    in
    aux acc t

  let cardinal m = fold (fun n _ _ -> n+1) 0 m

  let to_list m = fold (fun acc k v -> (k,v)::acc) [] m

  let add_list m l = List.fold_left (fun acc (k,v) -> add k v acc) m l

  let of_list l = add_list empty l

  let add_seq m s =
    let m = ref m in
    s (fun (k,v) -> m := add k v !m);
    !m

  let of_seq s = add_seq empty s

  let to_seq m yield = iter (fun k v -> yield (k,v)) m

  let print ppk ppv out m =
    let first = ref true in
    iter
      (fun k v ->
        if !first then first := false else Format.fprintf out ";@ ";
        ppk out k;
        Format.pp_print_string out " -> ";
        ppv out v
      ) m

  let rec as_tree m () = match m with
    | E -> `Nil
    | L (h,l) -> `Node (`L ((h:>int), list_as_tree_ l), [])
    | N (l,a) -> `Node (`N, as_tree (L (Hash.zero, l)) :: array_as_tree_ a)
  and list_as_tree_ l = match l with
    | Nil -> []
    | Cons (k, v, tail) -> (k,v) :: list_as_tree_ tail
  and array_as_tree_ a = A.fold (fun acc t -> as_tree t :: acc) [] a
end

(*$R
  let module M = Make(CCInt) in
  let m = M.of_list CCList.(1 -- 1000 |> map (fun i->i,i)) in
  assert_equal ~printer:CCInt.to_string 1000 (M.cardinal m);
  assert_bool "check all get"
    (Sequence.for_all (fun i -> i = M.get_exn i m) Sequence.(1 -- 1000));
  let m = Sequence.(501 -- 1000 |> fold (fun m i -> M.remove i m) m) in
  assert_equal ~printer:CCInt.to_string 500 (M.cardinal m);
  assert_bool "check all get after remove"
    (Sequence.for_all (fun i -> i = M.get_exn i m) Sequence.(1 -- 500));
  assert_bool "check all get after remove"
    (Sequence.for_all (fun i -> None = M.get i m) Sequence.(501 -- 1000));
*)

