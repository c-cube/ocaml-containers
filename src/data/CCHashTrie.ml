
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Tries} *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Fixed-Size Arrays} *)
module type FIXED_ARRAY = sig
  type 'a t
  val create : 'a -> 'a t
  val length_log : int
  val length : int  (* 2 power length_log *)
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val update : 'a t -> int -> ('a -> 'a) -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val foldi : (int -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b
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

module A32 : FIXED_ARRAY = struct
  type 'a t = 'a array

  let length_log = 5

  let length = 32

  let create x = Array.make length x

  let get a i = a.(i)

  let set a i x =
    let a' = Array.copy a in
    a'.(i) <- x;
    a'

  let update a i f =
    let x = a.(i) in
    let y = f a.(i) in
    if x==y then a else set a i x

  let iter = Array.iter

  let foldi f acc a =
    let rec aux f acc a i =
      if i = length then acc
      else
        let acc = f i acc (Array.unsafe_get a i) in
        aux f acc a (i+1)
    in
    aux f acc a 0
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
    val rem : t -> int (* [A.length_log] last bits *)
    val quotient : t -> t (* remove [A.length_log] last bits *)
    val combine : t -> int -> t
  end = struct
    type t = int
    let make = Key.hash
    let rem h = h land (A.length - 1)
    let quotient h = h lsr A.length_log
    let combine q r = (q lsl A.length_log) lor r
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
    A.update a i (fun x -> add_ k v ~h:h' x)

  let add k v m = add_ k v ~h:(hash_ k) m

  type count_array = {
    num_non_empty : int;  (* number of non empty slots *)
    idx_non_empty : int;  (* the index of a non-empty element, if any *)
  }

  let count_arr_ a =
    A.foldi
      (fun i acc t ->
        if is_empty t
          then acc
          else {num_non_empty=acc.num_non_empty+1; idx_non_empty=i}
      ) {num_non_empty=0; idx_non_empty=0} a

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
        match count_arr_ a' with
        | {num_non_empty=0; _} -> E
        | {num_non_empty=1; idx_non_empty=j} ->
            (* remove array since it has only one bucket *)
            A.get a' j
        | _ -> N a'

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
    let rec aux _ acc t = match t with
      | E -> acc
      | L (_,l) -> aux_list acc l
      | N a -> A.foldi aux acc a
    and aux_list acc l = match l with
      | Nil -> acc
      | Cons (k, v, tl) -> let acc = f acc k v in aux_list acc tl
    in
    aux 0 (* any int *) acc t

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
  and array_as_tree_ a = A.foldi (fun _ acc t -> as_tree t :: acc) [] a
end
