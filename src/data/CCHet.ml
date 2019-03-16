
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Associative containers with Heterogenerous Values} *)

(*$R
  let k1 : int Key.t = Key.create() in
  let k2 : int Key.t = Key.create() in
  let k3 : string Key.t = Key.create() in
  let k4 : float Key.t = Key.create() in

  let tbl = Tbl.create () in

  Tbl.add tbl k1 1;
  Tbl.add tbl k2 2;
  Tbl.add tbl k3 "k3";

  assert_equal (Some 1) (Tbl.find tbl k1);
  assert_equal (Some 2) (Tbl.find tbl k2);
  assert_equal (Some "k3") (Tbl.find tbl k3);
  assert_equal None (Tbl.find tbl k4);
  assert_equal 3 (Tbl.length tbl);

  Tbl.add tbl k1 10;
  assert_equal (Some 10) (Tbl.find tbl k1);
  assert_equal 3 (Tbl.length tbl);
  assert_equal None (Tbl.find tbl k4);

  Tbl.add tbl k4 0.0;
  assert_equal (Some 0.0) (Tbl.find tbl k4);

  ()


*)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

module type KEY_IMPL = sig
  type t
  exception Store of t
  val id : int
end

module Key = struct
  type 'a t = (module KEY_IMPL with type t = 'a)

  let _n = ref 0

  let create (type k) () =
    incr _n;
    let id = !_n in
    let module K = struct
      type t = k
      let id = id
      exception Store of k
    end in
    (module K : KEY_IMPL with type t = k)

  let id (type k) (module K : KEY_IMPL with type t = k) = K.id

  let equal
    : type a b. a t -> b t -> bool
    = fun (module K1) (module K2) -> K1.id = K2.id
end

type pair =
  | Pair : 'a Key.t * 'a -> pair

type exn_pair =
  | E_pair : 'a Key.t * exn -> exn_pair

let pair_of_e_pair (E_pair (k,e)) =
  let module K = (val k) in
  match e with
    | K.Store v -> Pair (k,v)
    | _ -> assert false

module Tbl = struct
  module M = Hashtbl.Make(struct
      type t = int
      let equal (i:int) j = i=j
      let hash (i:int) = Hashtbl.hash i
    end)

  type t = exn_pair M.t

  let create ?(size=16) () = M.create size

  let mem t k = M.mem t (Key.id k)

  let find_exn (type a) t (k : a Key.t) : a =
    let module K = (val k) in
    let E_pair (_, v) = M.find t K.id in
    match v with
      | K.Store v -> v
      | _ -> assert false

  let find t k =
    try Some (find_exn t k)
    with Not_found -> None

  let add_pair_ t p =
    let Pair (k,v) = p in
    let module K = (val k) in
    let p = E_pair (k, K.Store v) in
    M.replace t K.id p

  let add t k v = add_pair_ t (Pair (k,v))

  let remove (type a) t (k:a Key.t) =
    let module K = (val k) in
    M.remove t K.id

  let length t = M.length t

  let iter f t = M.iter (fun _ pair -> f (pair_of_e_pair pair)) t

  let to_seq t yield = iter yield t

  let to_list t = M.fold (fun _ p l -> pair_of_e_pair p::l) t []

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
      let compare (i:int) j = Stdlib.compare i j
    end)

  type t = exn_pair M.t

  let empty = M.empty

  let mem k t = M.mem (Key.id k) t

  let find_exn (type a) (k : a Key.t) t : a =
    let module K = (val k) in
    let E_pair (_, e) = M.find K.id t in
    match e with
      | K.Store v -> v
      | _ -> assert false

  let find k t =
    try Some (find_exn k t)
    with Not_found -> None

  let add_e_pair_ p t =
    let E_pair ((module K),_) = p in
    M.add K.id p t

  let add_pair_ p t =
    let Pair ((module K) as k,v) = p in
    let p = E_pair (k, K.Store v) in
    M.add K.id p t

  let add (type a) (k : a Key.t) v t =
    let module K = (val k) in
    add_e_pair_ (E_pair (k, K.Store v)) t
    
  let remove (type a) (k: a Key.t) t =
    let module K = (val k) in
    M.remove K.id t

  let cardinal t = M.cardinal t

  let length = cardinal

  let iter f t = M.iter (fun _ p -> f (pair_of_e_pair p)) t

  let to_seq t yield = iter yield t

  let to_list t = M.fold (fun _ p l -> pair_of_e_pair p::l) t []

  let add_list t l = List.fold_right add_pair_ l t

  let add_seq t seq =
    let t = ref t in
    seq (fun pair -> t := add_pair_ pair !t);
    !t

  let of_list l = add_list empty l

  let of_seq seq = add_seq empty seq
end
