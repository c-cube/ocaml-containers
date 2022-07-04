(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Table with Heterogeneous Keys} *)

type 'b injection = {
  get: (unit -> unit) -> 'b option;
  set: 'b -> unit -> unit;
}

type 'a t = ('a, unit -> unit) Hashtbl.t

let create n = Hashtbl.create n

let create_inj () =
  let r = ref None in
  let get f =
    r := None;
    f ();
    !r
  and set v () = r := Some v in
  { get; set }

let get ~inj tbl x = try inj.get (Hashtbl.find tbl x) with Not_found -> None
let set ~inj tbl x y = Hashtbl.replace tbl x (inj.set y)
let length tbl = Hashtbl.length tbl
let clear tbl = Hashtbl.clear tbl
let remove tbl x = Hashtbl.remove tbl x
let copy tbl = Hashtbl.copy tbl

let is_some = function
  | None -> false
  | Some _ -> true

let mem ~inj tbl x =
  try is_some (inj.get (Hashtbl.find tbl x)) with Not_found -> false

let find ~inj tbl x =
  match inj.get (Hashtbl.find tbl x) with
  | None -> raise Not_found
  | Some v -> v

let iter_keys tbl f = Hashtbl.iter (fun x _ -> f x) tbl
let fold_keys tbl acc f = Hashtbl.fold (fun x _ acc -> f acc x) tbl acc

(** {2 Iterators} *)

type 'a iter = ('a -> unit) -> unit

let keys_iter tbl yield = Hashtbl.iter (fun x _ -> yield x) tbl

let bindings_of ~inj tbl yield =
  Hashtbl.iter
    (fun k value ->
      match inj.get value with
      | None -> ()
      | Some v -> yield (k, v))
    tbl

type value = Value : ('b injection -> 'b option) -> value

let bindings tbl yield =
  Hashtbl.iter (fun x y -> yield (x, Value (fun inj -> inj.get y))) tbl
