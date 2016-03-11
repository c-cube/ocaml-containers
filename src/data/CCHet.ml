
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Associative containers with Heterogenerous Values} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

module Key = struct
  type 'a t = int

  let create =
    let _n = ref 0 in
    fun () ->
      incr _n;
      !_n

  let id a = a

  let equal
    : type a b. a t -> b t -> bool
    = fun a b ->
      let ia = (a : a t :> int) in
      let ib = (b : b t :> int) in
      ia=ib

  (* XXX: the only ugly part *)
  (* [cast_res k1 k2 v2] casts [v2] into a value of type [a] if [k1=k2] *)
  let cast_res_ : type a b. a t -> b t -> b -> a
    = fun k1 k2 v2 ->
      if k1=k2 then Obj.magic v2 else raise Not_found
end

type pair =
  | Pair : 'a Key.t * 'a -> pair

module Tbl = struct
  module M = Hashtbl.Make(struct
      type t = int
      let equal (i:int) j = i=j
      let hash (i:int) = Hashtbl.hash i
    end)

  type t = pair M.t

  let create ?(size=16) () = M.create size

  let mem t k = M.mem t (Key.id k)

  let find_exn (type a) t (k : a Key.t) : a =
    let Pair (k', v) = M.find t (Key.id k) in
    Key.cast_res_ k k' v

  let find t k =
    try Some (find_exn t k)
    with Not_found -> None

  let add_pair_ t p =
    let Pair (k,_) = p in
    M.replace t (Key.id k) p

  let add t k v = add_pair_ t (Pair (k,v))

  let length t = M.length t

  let iter f t = M.iter (fun _ pair -> f pair) t

  let to_seq t yield = iter yield t

  let to_list t = M.fold (fun _ p l -> p::l) t []

  let add_list t l = List.iter (add_pair_ t) l

  let add_seq t seq = seq (add_pair_ t)

  let of_list l =
    let t = create() in
    add_list t l;
    t

  let of_seq seq =
    let t = create() in
    add_seq t seq;
    t
end

module Map = struct
  module M = Map.Make(struct
      type t = int
      let compare (i:int) j = Pervasives.compare i j
    end)

  type t = pair M.t

  let empty = M.empty

  let mem k t = M.mem (Key.id k) t

  let find_exn (type a) (k : a Key.t) t : a =
    let Pair (k', v) = M.find (Key.id k) t in
    Key.cast_res_ k k' v

  let find k t =
    try Some (find_exn k t)
    with Not_found -> None

  let add_pair_ p t =
    let Pair (k,_) = p in
    M.add (Key.id k) p t

  let add k v t = add_pair_ (Pair (k,v)) t

  let cardinal t = M.cardinal t

  let length = cardinal

  let iter f t = M.iter (fun _ pair -> f pair) t

  let to_seq t yield = iter yield t

  let to_list t = M.fold (fun _ p l -> p::l) t []

  let add_list t l = List.fold_right add_pair_ l t

  let add_seq t seq =
    let t = ref t in
    seq (fun pair -> t := add_pair_ pair !t);
    !t

  let of_list l = add_list empty l

  let of_seq seq = add_seq empty seq
end
