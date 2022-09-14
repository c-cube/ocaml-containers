
module A = CCAtomic

[@@@ifge 5.00]

type key = int*int

let get_key_ () : key =
  Domain.id (Domain.self()), Thread.id (Thread.self())

module Key_map_ = CCMap.Make(struct
  type t = key
  let compare : t -> t -> int = compare
end)

[@@@else_]

type key = int

let get_key_ () : key =
  Thread.id (Thread.self())

module Key_map_ = CCMap.Make(struct
  type t = key
  let compare : t -> t -> int = CCInt.compare
end)

[@@@endif]

type 'a t = 'a option ref Key_map_.t A.t

let create () : _ t = A.make Key_map_.empty

let[@inline] n_entries self = Key_map_.cardinal (A.get self)

let get_exn self =
  let m = A.get self in
  let key = get_key_ () in
  let r = Key_map_.find key m in
  match !r with
  | Some x -> x
  | None -> raise Not_found

let[@inline] get self =
  try Some (get_exn self)
  with Not_found -> None

let[@inline] get_or ~default self =
  try get_exn self
  with Not_found -> default

(* remove reference for the key *)
let[@inline] remove_ref_ self key =
  while
    let m = A.get self in
    let m' = Key_map_.remove key m in
    not (A.compare_and_set self m m')
  do() done

type get_or_create_status =
  | Created
  | Reused

(* get or associate a reference to [key], and return it.
   Also return a function to remove the reference if we just created it. *)
let get_or_create_ref_ (self:_ t ) key : _ ref * get_or_create_status =
  try Key_map_.find key (A.get self), Reused
  with Not_found ->
    let r = ref None in
    while
      let m = A.get self in
      let m' = Key_map_.add key r m in
      not (A.compare_and_set self m m')
    do () done;
    r, Created

let set (self: _ t ) v : unit =
  let key = get_key_ () in
  let r, _ = get_or_create_ref_ self key in
  r := Some v

let set_get (self: _ t ) v : _ option =
  let key = get_key_ () in
  let r, _ = get_or_create_ref_ self key in
  let old = !r in
  r := Some v;
  old

let with_ self x f =
  let key = get_key_ () in
  let r, status = get_or_create_ref_ self key in
  let old = !r in
  r := Some x;

  try
    let res = f () in
    r := old;
    if status == Created then remove_ref_ self key;
    res
  with e ->
    r := old;
    if status == Created then remove_ref_ self key;
    raise e

