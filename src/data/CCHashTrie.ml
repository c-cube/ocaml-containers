
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Tries} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Fixed-Size Arrays} *)
module type FIXED_ARRAY = sig
  type 'a t
  val create : 'a -> 'a t
  val length : int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val for_all : ('a -> bool) -> 'a t -> bool
end

(* TODO: add an "update" function? *)

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

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val of_list : (key * 'a) list -> 'a t

  val print : key printer -> 'a printer -> 'a t printer

  val as_tree : 'a t -> [`L of int * (key * 'a) list | `N ] ktree
end

module type KEY = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Arrays} *)
module A8 : FIXED_ARRAY = struct
  type 'a t = {
    a0 : 'a;
    a1 : 'a;
    a2 : 'a;
    a3 : 'a;
    a4 : 'a;
    a5 : 'a;
    a6 : 'a;
    a7 : 'a;
  }

  let create x = {a0=x; a1=x; a2=x; a3=x; a4=x; a5=x; a6=x;a7=x}

  let length = 8

  let get a i = match i with
    | 0 -> a.a0
    | 1 -> a.a1
    | 2 -> a.a2
    | 3 -> a.a3
    | 4 -> a.a4
    | 5 -> a.a5
    | 6 -> a.a6
    | 7 -> a.a7
    | _ -> invalid_arg "A8.get"

  let set a i x = match i with
    | 0 -> {a with a0=x}
    | 1 -> {a with a1=x}
    | 2 -> {a with a2=x}
    | 3 -> {a with a3=x}
    | 4 -> {a with a4=x}
    | 5 -> {a with a5=x}
    | 6 -> {a with a6=x}
    | 7 -> {a with a7=x}
    | _ -> invalid_arg "A8.set"

  let iter f a =
    f a.a0;
    f a.a1;
    f a.a2;
    f a.a3;
    f a.a4;
    f a.a5;
    f a.a6;
    f a.a7;
    ()

  let fold f acc a =
    let acc = f acc a.a0 in
    let acc = f acc a.a1 in
    let acc = f acc a.a2 in
    let acc = f acc a.a3 in
    let acc = f acc a.a4 in
    let acc = f acc a.a5 in
    let acc = f acc a.a6 in
    let acc = f acc a.a7 in
    acc

  let for_all p a =
    p a.a0 &&
    p a.a1 &&
    p a.a2 &&
    p a.a3 &&
    p a.a4 &&
    p a.a5 &&
    p a.a6 &&
    p a.a7
end

(** {2 Functors} *)

module Hash : sig
  type t = private int
  val make_unsafe : int -> t
  val rem : t -> int (* 3 last bits *)
  val quotient : t -> t (* remove 3 last bits *)
end = struct
  type t = int
  let make_unsafe i = i
  let rem h = h land 7
  let quotient h = h lsr 3
end

module Make(Key : KEY)
: S with module A = A8 and type key = Key.t
= struct
  module A = A8

  let () = assert (A.length = 8)

  let hash_ x = Hash.make_unsafe (Key.hash x)

  type key = Key.t

  (* association list, without duplicates *)
  type 'a leaf =
    | Nil
    | Cons of key * 'a * 'a leaf

  type 'a t =
    | E
    | L of Hash.t * 'a leaf (* same hash for all elements *)
    | N of 'a t A.t

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
    | N a ->
        let i = Hash.rem h in
        let h' = Hash.quotient h in
        get_exn_ k ~h:h' (A.get a i)

  let get_exn k m = get_exn_ k ~h:(hash_ k) m

  let get k m =
    try Some (get_exn_ k ~h:(hash_ k) m)
    with Not_found -> None

  (* TODO: use Hash.combine if array only has one non-empty element *)

  (* [h]: hash, with the part required to reach this leaf removed *)
  let rec add_ k v ~h m = match m with
    | E -> leaf_ k v ~h
    | L (h', l) ->
        if h=h'
        then L (h, add_list_ k v ~h l)
        else (* split into N *)
          let a = A.create E in
          (* put leaf in the right bucket *)
          let i = Hash.rem h' in
          let h'' = Hash.quotient h' in
          let a = A.set a i (L (h'', l)) in
          (* then add new node *)
          let a = add_to_array_ k v ~h a in
          N a
    | N a -> N (add_to_array_ k v ~h a)

  (* [left] list nodes already visited *)
  and add_list_ k v ~h l = match l with
    | Nil -> Cons (k, v, Nil)
    | Cons (k', v', tail) ->
        if Key.equal k k'
        then Cons (k, v, tail) (* replace *)
        else Cons (k', v', add_list_ k v ~h tail)

  (* add k->v to [a] *)
  and add_to_array_ k v ~h a =
    (* insert in a bucket *)
    let i = Hash.rem h in
    let h' = Hash.quotient h in
    A.set a i (add_ k v ~h:h' (A.get a i))

  let add k v m = add_ k v ~h:(hash_ k) m

  let is_empty_arr_ a = A.for_all is_empty a

  let rec remove_list_ k l = match l with
    | Nil -> Nil
    | Cons (k', v', tail) ->
        if Key.equal k k'
          then tail
          else Cons (k', v', remove_list_ k tail)

  let rec remove_rec_ k ~h m = match m with
    | E -> E
    | L (h, l) ->
        begin match remove_list_ k l with
        | Nil -> E
        | Cons _ as res -> L (h, res)
        end
    | N a ->
        let i = Hash.rem h in
        let h' = Hash.quotient h in
        let a' = A.set a i (remove_rec_ k ~h:h' (A.get a i)) in
        if is_empty_arr_ a'
          then E
          else N a'

  let remove k m = remove_rec_ k ~h:(hash_ k) m

  let iter f t =
    let rec aux = function
      | E -> ()
      | L (_,l) -> aux_list l
      | N a -> A.iter aux a
    and aux_list = function
      | Nil -> ()
      | Cons (k, v, tl) -> f k v; aux_list tl
    in
    aux t

  let fold f acc t =
    let rec aux acc t = match t with
      | E -> acc
      | L (_,l) -> aux_list acc l
      | N a -> A.fold aux acc a
    and aux_list acc l = match l with
      | Nil -> acc
      | Cons (k, v, tl) -> let acc = f acc k v in aux_list acc tl
    in
    aux acc t

  let cardinal m = fold (fun n _ _ -> n+1) 0 m

  let to_list m = fold (fun acc k v -> (k,v)::acc) [] m

  let add_list m l = List.fold_left (fun acc (k,v) -> add k v acc) m l

  let of_list l = add_list empty l

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
    | N a -> `Node (`N, array_as_tree_ a)
  and list_as_tree_ l = match l with
    | Nil -> []
    | Cons (k, v, tail) -> (k,v) :: list_as_tree_ tail
  and array_as_tree_ a = A.fold (fun acc t -> as_tree t :: acc) [] a
end
