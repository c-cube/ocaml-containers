(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bijection} *)

type 'a iter = ('a -> unit) -> unit

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  type left
  type right

  val empty : t
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val add : left -> right -> t -> t
  val cardinal : t -> int
  val mem : left -> right -> t -> bool
  val mem_left : left -> t -> bool
  val mem_right : right -> t -> bool
  val find_left : left -> t -> right
  val find_right : right -> t -> left
  val remove  : left -> right -> t -> t
  val remove_left : left -> t -> t
  val remove_right : right -> t -> t
  val list_left : t -> (left * right) list
  val list_right : t -> (right * left) list
  val add_iter : (left * right) iter -> t -> t
  val of_iter : (left * right) iter -> t
  val to_iter : t -> (left * right) iter
  val add_list : (left * right) list -> t -> t
  val of_list : (left * right) list -> t
  val to_list : t -> (left * right) list
end

module Make(L : OrderedType)(R : OrderedType) = struct
  type left = L.t
  type right = R.t

  module MapL = Map.Make(L)
  module MapR = Map.Make(R)

  type t = {
    left : right MapL.t;
    right : left MapR.t;
  }

  let empty = {
    left = MapL.empty;
    right = MapR.empty;
  }

  let cardinal m = MapL.cardinal m.left

  let is_empty m =
    let res = MapL.is_empty m.left in
    assert (res = MapR.is_empty m.right);
    res

  let equal a b = MapL.equal (fun a b -> R.compare a b = 0) a.left b.left
  let compare a b = MapL.compare R.compare a.left b.left

  let add a b m = {
    left =
      (try let found = MapR.find b m.right in
         if L.compare found a <> 0 then MapL.remove found m.left else m.left
       with Not_found -> m.left)
      |> MapL.add a b;
    right =
      (try let found = MapL.find a m.left in
         if R.compare found b <> 0 then MapR.remove found m.right else m.right
       with Not_found -> m.right)
      |> MapR.add b a;
  }

  let find_left  key m = MapL.find key m.left
  let find_right key m = MapR.find key m.right

  let mem left right m = try R.compare right (find_left left m) = 0 with Not_found -> false
  let mem_left  key m  = MapL.mem key m.left
  let mem_right key m  = MapR.mem key m.right

  let remove a b m =
    if mem a b m then
      {
        left  = MapL.remove a m.left;
        right = MapR.remove b m.right;
      }
    else m

  let remove_left a m =
    let right = try MapR.remove (find_left a m) m.right with Not_found -> m.right in
    { right; left  = MapL.remove a m.left  }

  let remove_right b m =
    let left = try MapL.remove (find_right b m) m.left  with Not_found -> m.left  in
    { left;  right = MapR.remove b m.right }

  let list_left  m = MapL.bindings m.left
  let list_right m = MapR.bindings m.right

  let add_list l m = List.fold_left (fun m (a,b) -> add a b m) m l
  let of_list l = add_list l empty
  let to_list = list_left

  let add_iter seq m =
    let m = ref m in
    seq (fun (k,v) -> m := add k v !m);
    !m

  let of_iter l = add_iter l empty

  let to_iter m yield = MapL.iter (fun k v -> yield (k,v)) m.left
end

(*$inject
  open Containers
  module M = Make(Int)(String)

*)

(*$=
  2     (M.of_list [1,"1"; 2, "2"] |> M.cardinal)
  "1"   (M.of_list [1,"1"; 2, "2"] |> M.find_left 1)
  "2"   (M.of_list [1,"1"; 2, "2"] |> M.find_left 2)
  1     (M.of_list [1,"1"; 2, "2"] |> M.find_right "1")
  2     (M.of_list [1,"1"; 2, "2"] |> M.find_right "2")
*)
