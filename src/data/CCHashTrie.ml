
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Tries} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Fixed-Size Arrays} *)
module type FIXED_ARRAY = sig
  type +'a t
  val create : 'a -> 'a t
  val length_log : int
  val length : int  (* 2 power length_log *)
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  (* temporary constructor *)
  type 'a mut
  val create_mut : 'a -> 'a mut
  val freeze_mut : 'a mut -> 'a t
  val set_mut : 'a mut -> int -> 'a -> unit
  val get_mut : 'a mut -> int -> 'a
end

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

module A32 : FIXED_ARRAY = struct
  type +'a t = { dummy1: 'a; dummy2 : 'a }  (* used for variance only *)

  type 'a mut = 'a array

  (* NOTE for safety:

    the array and the record are both boxed types, in the heap
    (since it has two fields it should not change in the future).

    using an array as covariant is safe because we ALWAYS copy before writing,
    so we cannot put a wrong value in [a] by upcasting it and writing.
  *)

  external hide_array_ : 'a array -> 'a t = "%identity"
  external get_array_ : 'a t -> 'a array = "%identity"

  let length_log = 5

  let length = 1 lsl length_log  (* 32 *)

  let create x = hide_array_ (Array.make length x)

  let get a i = Array.get (get_array_ a) i

  let set a i x =
    let a' = Array.copy (get_array_ a) in
    a'.(i) <- x;
    hide_array_ a'

  let iter f a = Array.iter f (get_array_ a)

  let fold f acc a = Array.fold_left f acc (get_array_ a)

  let create_mut x = Array.make length x

  let freeze_mut a = hide_array_ a

  let set_mut a i x = a.(i) <- x

  let get_mut a i = a.(i)
end

(** {2 Functors} *)

module Make(Key : KEY)
: S with type key = Key.t
= struct
  module A = A32

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
          let a = A.create_mut E in
          let leaf =
            if Hash.is_0 h' then l
            else (
              (* put leaf in the right bucket *)
              let i = Hash.rem h' in
              let h'' = Hash.quotient h' in
              A.set_mut a i (L (h'', l));
              Nil
            )
          in
          (* then add new node *)
          let leaf =
            if Hash.is_0 h then add_list_ k v leaf
            else (
              let i = Hash.rem h in
              let h' = Hash.quotient h in
              A.set_mut a i (add_ k v ~h:h' (A.get_mut a i));
              leaf
            )
          in
          N (leaf, A.freeze_mut a)
    | N (leaf, a) ->
        if Hash.is_0 h
        then N (add_list_ k v leaf, a)
        else N (leaf, add_to_array_ k v ~h a)

  (* add k->v to [a] *)
  and add_to_array_ k v ~h a =
    (* insert in a bucket *)
    let i = Hash.rem h in
    let h' = Hash.quotient h in
    let new_t = add_ k v ~h:h' (A.get a i) in
    A.set a i new_t

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
              leaf, A.set a i (remove_rec_ k ~h:h' (A.get a i))
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
