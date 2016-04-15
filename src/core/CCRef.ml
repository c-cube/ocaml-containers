
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 References}

@since 0.9 *)

type 'a print = Format.formatter -> 'a -> unit
type 'a pp = Buffer.t -> 'a -> unit
type 'a ord = 'a -> 'a -> int
type 'a eq = 'a -> 'a -> bool
type 'a sequence = ('a -> unit) -> unit

type 'a t = 'a ref

let create x = ref x

let map f r = ref (f !r)

let iter f r = f !r

let update f r = r := (f !r)

let incr_then_get r =
  incr r; !r

let get_then_incr r =
  let x = !r in
  incr r;
  x

let compare f r1 r2 = f !r1 !r2

let equal f r1 r2 = f !r1 !r2

let to_list r = [!r]
let to_seq r yield = yield !r

let print pp_x fmt r = pp_x fmt !r

let pp pp_x buf r = pp_x buf !r
