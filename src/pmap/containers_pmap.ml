type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit

module type KEY = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : _ t -> bool
  val singleton : key -> 'a -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> _ t -> bool
  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if key not present *)

  val remove : key -> 'a t -> 'a t

  val update : key -> f:('a option -> 'a option) -> 'a t -> 'a t
  (** [update k ~f m] calls [f (Some v)] if [get k m = Some v],
      [f None] otherwise. Then, if [f] returns [Some v'] it binds [k] to [v'],
      if [f] returns [None] it removes [k] *)

  val cardinal : _ t -> int
  val choose : 'a t -> (key * 'a) option

  val choose_exn : 'a t -> key * 'a
  (** @raise Not_found if no pair was found *)

  val iter : f:(key -> 'a -> unit) -> 'a t -> unit
  val fold : f:('b -> key -> 'a -> 'b) -> x:'b -> 'a t -> 'b
  val to_list : 'a t -> (key * 'a) list
  val add_list : 'a t -> (key * 'a) list -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val add_iter : 'a t -> (key * 'a) iter -> 'a t
  val of_iter : (key * 'a) iter -> 'a t
  val to_iter : 'a t -> (key * 'a) iter
  val add_gen : 'a t -> (key * 'a) gen -> 'a t
  val of_gen : (key * 'a) gen -> 'a t
  val to_gen : 'a t -> (key * 'a) gen
  val pp : key printer -> 'a printer -> 'a t printer
end

let num_bits = 4
let branching_factor = 1 lsl num_bits
let bitmask = branching_factor - 1

external popcount : int -> int
  = "caml_cc_pmap_popcount" "caml_cc_pmap_popcount_byte"

(** Sparse array using bitmap + dense array *)
module A = struct
  open Array

  type 'a t = {
    bitmap: int;
    arr: 'a array;
  }

  let empty = { bitmap = 0; arr = [||] }
  let[@inline] length self = Array.length self.arr
  let[@inline] is_empty self = self.bitmap = 0
  let[@inline] real_index bitmap i = popcount (bitmap land ((1 lsl i) - 1))

  let get ~default self i =
    let mask = 1 lsl i in
    if self.bitmap land mask = 0 then
      default
    else
      unsafe_get self.arr (real_index self.bitmap i)

  let set self i x =
    let mask = 1 lsl i in
    if self.bitmap land mask = 0 then (
      let idx = real_index self.bitmap i in
      let n = length self in
      let new_arr = Array.make (n + 1) x in
      if idx > 0 then blit self.arr 0 new_arr 0 idx;
      if idx < n then blit self.arr idx new_arr (idx + 1) (n - idx);
      { bitmap = self.bitmap lor mask; arr = new_arr }
    ) else (
      let idx = real_index self.bitmap i in
      let new_arr = Array.copy self.arr in
      new_arr.(idx) <- x;
      { self with arr = new_arr }
    )

  let remove self i =
    let mask = 1 lsl i in
    if self.bitmap land mask = 0 then
      self
    else (
      let idx = real_index self.bitmap i in
      let n = length self in
      if n = 1 then
        empty
      else (
        let new_arr = Array.make (n - 1) (unsafe_get self.arr 0) in
        if idx > 0 then blit self.arr 0 new_arr 0 idx;
        if idx < n - 1 then blit self.arr (idx + 1) new_arr idx (n - idx - 1);
        { bitmap = self.bitmap land lnot mask; arr = new_arr }
      )
    )

  let[@inline] iter f self = Array.iter f self.arr
  let[@inline] fold f acc self = Array.fold_left f acc self.arr
end

(* Association list for hash collisions *)
type ('key, 'a) leaf =
  | One of 'key * 'a
  | Two of 'key * 'a * 'key * 'a
  | Cons of 'key * 'a * ('key, 'a) leaf

module Make (Key : KEY) : S with type key = Key.t = struct
  type key = Key.t

  type 'a t =
    | E
    | L of int * (key, 'a) leaf
    | N of 'a t A.t
  (* invariants:
      - N with empty sparse array should never exist (use E instead)
      - In L (h, leaf), all keys have the same hash h
      - In N a, children may be E, L, or N *)

  let empty = E

  let[@inline] is_empty = function
    | E -> true
    | _ -> false

  let singleton k v = L (Key.hash k, One (k, v))

  let rec find_in_leaf k l =
    match l with
    | One (k', v) ->
      if Key.equal k k' then
        Some v
      else
        None
    | Two (k1, v1, k2, v2) ->
      if Key.equal k k1 then
        Some v1
      else if Key.equal k k2 then
        Some v2
      else
        None
    | Cons (k', v, tl) ->
      if Key.equal k k' then
        Some v
      else
        find_in_leaf k tl

  let rec add_leaf k v l =
    match l with
    | One (k1, v1) ->
      if Key.equal k k1 then
        One (k, v)
      else
        Two (k, v, k1, v1)
    | Two (k1, v1, k2, v2) ->
      if Key.equal k k1 then
        Two (k, v, k2, v2)
      else if Key.equal k k2 then
        Two (k, v, k1, v1)
      else
        Cons (k, v, l)
    | Cons (k', v', tl) ->
      if Key.equal k k' then
        Cons (k, v, tl)
      else
        Cons (k', v', add_leaf k v tl)

  exception Empty_leaf

  let rec remove_leaf k l =
    match l with
    | One (k', _) ->
      if Key.equal k k' then
        raise_notrace Empty_leaf
      else
        l
    | Two (k1, v1, k2, v2) ->
      if Key.equal k k1 then
        One (k2, v2)
      else if Key.equal k k2 then
        One (k1, v1)
      else
        l
    | Cons (k', v', tl) ->
      if Key.equal k k' then
        tl
      else
        Cons (k', v', remove_leaf k tl)

  let rec get k m =
    match m with
    | E -> None
    | L (h, l) ->
      if Key.hash k = h then
        find_in_leaf k l
      else
        None
    | N a ->
      let h = Key.hash k in
      get_rec h k (A.get ~default:E a (h land bitmask))

  and get_rec h k m =
    match m with
    | E -> None
    | L (h', l) ->
      if h = h' then
        find_in_leaf k l
      else
        None
    | N a ->
      get_rec (h lsr num_bits) k
        (A.get ~default:E a ((h lsr num_bits) land bitmask))

  let get_exn k m =
    match get k m with
    | Some v -> v
    | None -> raise Not_found

  let mem k m = Option.is_some (get k m)

  let rec add k v m =
    let h = Key.hash k in
    match m with
    | E -> L (h, One (k, v))
    | L (h', l) ->
      if h = h' then
        L (h, add_leaf k v l)
      else (
        let a = A.empty in
        let a = A.set a (h' land bitmask) (L (h' lsr num_bits, l)) in
        N (A.set a (h land bitmask) (L (h lsr num_bits, One (k, v))))
      )
    | N a -> N (add_in_array h k v a)

  and add_in_array h k v a =
    let i = h land bitmask in
    let h' = h lsr num_bits in
    let sub = A.get ~default:E a i in
    A.set a i (add_rec h' k v sub)

  and add_rec h k v m =
    match m with
    | E -> L (h, One (k, v))
    | L (h', l) ->
      if h = h' then
        L (h, add_leaf k v l)
      else (
        let a = A.empty in
        let a = A.set a (h' land bitmask) (L (h' lsr num_bits, l)) in
        let a = A.set a (h land bitmask) (L (h lsr num_bits, One (k, v))) in
        N a
      )
    | N a -> N (add_in_array h k v a)

  (* --- Remove --- *)

  and remove k m =
    let h = Key.hash k in
    match m with
    | E -> E
    | L (h', l) ->
      if h = h' then (
        try L (h, remove_leaf k l) with Empty_leaf -> E
      ) else
        m
    | N a ->
      let new_a = remove_in_array h k a in
      if A.is_empty new_a then
        E
      else
        N new_a

  and remove_in_array h k a =
    let i = h land bitmask in
    let h' = h lsr num_bits in
    let sub = A.get ~default:E a i in
    let new_sub = remove_rec h' k sub in
    if is_empty new_sub then
      A.remove a i
    else
      A.set a i new_sub

  and remove_rec h k m =
    match m with
    | E -> E
    | L (h', l) ->
      if h = h' then (
        try L (h, remove_leaf k l) with Empty_leaf -> E
      ) else
        m
    | N a ->
      let new_a = remove_in_array h k a in
      if A.is_empty new_a then
        E
      else
        N new_a

  and update k ~f m =
    let opt_v = get k m in
    match f opt_v with
    | None ->
      (match opt_v with
      | None -> m
      | Some _ -> remove k m)
    | Some v -> add k v m

  let rec iter ~f = function
    | E -> ()
    | L (_, l) -> iter_leaf f l
    | N a -> A.iter (iter ~f) a

  and iter_leaf f = function
    | One (k, v) -> f k v
    | Two (k1, v1, k2, v2) ->
      f k1 v1;
      f k2 v2
    | Cons (k, v, tl) ->
      f k v;
      iter_leaf f tl

  let fold ~f ~x t =
    let rec aux acc = function
      | E -> acc
      | L (_, l) -> aux_leaf acc l
      | N a -> A.fold aux acc a
    and aux_leaf acc l =
      match l with
      | One (k, v) -> f acc k v
      | Two (k1, v1, k2, v2) -> f (f acc k1 v1) k2 v2
      | Cons (k, v, tl) -> aux_leaf (f acc k v) tl
    in
    aux x t

  let[@inline] cardinal m = fold ~f:(fun n _ _ -> n + 1) ~x:0 m
  let[@inline] to_list m = fold ~f:(fun acc k v -> (k, v) :: acc) ~x:[] m
  let[@inline] add_list m l = List.fold_left (fun acc (k, v) -> add k v acc) m l
  let[@inline] of_list l = add_list empty l

  let[@inline] add_iter m seq =
    let m = ref m in
    seq (fun (k, v) -> m := add k v !m);
    !m

  let[@inline] of_iter s = add_iter empty s
  let[@inline] to_iter m yield = iter ~f:(fun k v -> yield (k, v)) m

  let rec add_gen m g =
    match g () with
    | None -> m
    | Some (k, v) -> add_gen (add k v m) g

  let of_gen g = add_gen empty g

  let to_gen m =
    let st = Stack.create () in
    Stack.push m st;
    let rec next () =
      if Stack.is_empty st then
        None
      else (
        match Stack.pop st with
        | E -> next ()
        | L (_, l) ->
          (match l with
          | One (k, v) -> Some (k, v)
          | Two (k1, v1, k2, v2) ->
            Stack.push (L (0, One (k2, v2))) st;
            Some (k1, v1)
          | Cons (k, v, tl) ->
            Stack.push (L (0, tl)) st;
            Some (k, v))
        | N a ->
          A.iter (fun sub -> Stack.push sub st) a;
          next ()
      )
    in
    next

  let choose m = to_gen m ()

  let choose_exn m =
    match choose m with
    | None -> raise Not_found
    | Some (k, v) -> k, v

  let pp ppk ppv out m =
    let first = ref true in
    iter m ~f:(fun k v ->
        if !first then
          first := false
        else
          Format.fprintf out ";@ ";
        ppk out k;
        Format.pp_print_string out " -> ";
        ppv out v)
end
