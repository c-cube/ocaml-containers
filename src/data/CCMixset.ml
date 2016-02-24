
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Set of Heterogeneous Values} *)

module IMap = Map.Make(struct
  type t = int
  let compare : int -> int -> int = compare
end)

(*$R
  let k1 : int key = newkey () in
  let k2 : int key = newkey () in
  let k3 : string key = newkey () in
  let set =
    empty
    |> set ~key:k1 1
    |> set ~key:k2 2
    |> set ~key:k3 "3"
  in
  assert (get ~key:k1 set = Some 1);
  assert (get ~key:k2 set = Some 2);
  assert (get ~key:k3 set = Some "3");
  ()
*)

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
