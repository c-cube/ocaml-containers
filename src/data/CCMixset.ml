
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Set of Heterogeneous Values} *)

module IMap = Map.Make(struct
    type t = int
    let compare : int -> int -> int = compare
  end)

type t = (unit -> unit) IMap.t
and 'a key = {
  id: int;
  mutable opt : 'a option;
};;

let newkey_n_ = ref 0

let newkey () =
  let id = !newkey_n_ in
  incr newkey_n_;
  { id; opt=None; }

let empty = IMap.empty

let get ~key set =
  key.opt <- None;
  try
    (IMap.find key.id set) ();
    key.opt
  with Not_found -> None

let get_exn ~key set = match get ~key set with
  | None -> raise Not_found
  | Some v -> v

let set ~key v set =
  IMap.add key.id (fun () -> key.opt <- Some v) set

let cardinal set = IMap.cardinal set
