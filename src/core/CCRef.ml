
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 References}

    @since 0.9 *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a ord = 'a -> 'a -> int
type 'a eq = 'a -> 'a -> bool
type 'a iter = ('a -> unit) -> unit

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

let swap a b =
  let x = !a in
  a := !b;
  b := x

let to_list r = [!r]
let to_iter r yield = yield !r

let pp pp_x out r = pp_x out !r
