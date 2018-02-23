(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Bijection} *)
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
  val add : left -> right -> t -> t
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
end

module Make(L : OrderedType)(R : OrderedType) = struct
  type left = L.t
  type right = R.t

  module MapL = Map.Make(L)
  module MapR = Map.Make(R)

  exception Incoherence of string

  type t = {
      left : right MapL.t;
      right : left MapR.t;
    }

  let empty = {
      left = MapL.empty;
      right = MapR.empty;
    }

  let is_empty m = match MapL.is_empty m.left, MapR.is_empty m.right with
    | l, r when l = r -> l
    | l, r -> raise (Incoherence ("is_empty left: " ^ string_of_bool l ^ ", right: " ^ string_of_bool r))

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

end
