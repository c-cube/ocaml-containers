
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

type 'a t = 'a Key_map_.t A.t

let create () : _ t = A.make Key_map_.empty

let[@inline] n_entries self = Key_map_.cardinal (A.get self)

let get (self: _ t ) : _ option =
  let m = A.get self in
  let key = get_key_ () in
  Key_map_.get key m

let get_exn self =
  let m = A.get self in
  let key = get_key_ () in
  Key_map_.find key m

let get_or ~default self =
  try get_exn self
  with Not_found -> default

let set (self: _ t ) v : unit =
  let key = get_key_ () in
  while
    let m = A.get self in
    let m' = Key_map_.add key v m in
    not (A.compare_and_set self m m')
  do () done

let set_get (self: _ t ) v : _ option =
  let key = get_key_ () in
  let rec loop () =
    let m = A.get self in
    let m' = Key_map_.add key v m in
    if A.compare_and_set self m m' then Key_map_.get key m
    else loop()
  in loop ()

let remove self =
  let key = get_key_ () in
  while
    let m = A.get self in
    let m' = Key_map_.remove key m in
    not (A.compare_and_set self m m')
  do () done

let[@inline] set_opt_ self v =
  match v with
  | None -> remove self
  | Some v' -> set self v'

let with_ self x f =
  let old = set_get self x in
  try
    let r = f() in
    set_opt_ self old;
    r
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    set_opt_ self old;
    Printexc.raise_with_backtrace e bt
