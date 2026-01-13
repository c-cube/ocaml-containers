[@@@ifge 4.12]

include Atomic

[@@@else_]

open Stdlib (* for == *)

type 'a t = { mutable x: 'a }

let[@inline] make x = { x }
let[@inline] get { x } = x
let[@inline] set r x = r.x <- x

let[@inline never] exchange r x =
  (* atomic *)
  let y = r.x in
  r.x <- x;
  (* atomic *)
  y

let[@inline never] compare_and_set r seen v =
  (* atomic *)
  if r.x == seen then (
    r.x <- v;
    (* atomic *)
    true
  ) else
    false

let[@inline never] fetch_and_add r x =
  (* atomic *)
  let v = r.x in
  r.x <- x + r.x;
  (* atomic *)
  v

let[@inline never] incr r =
  (* atomic *)
  r.x <- 1 + r.x
(* atomic *)

let[@inline never] decr r =
  (* atomic *)
  r.x <- r.x - 1
(* atomic *)

[@@@endif]

(** Update loop with a compare-and-swap, and some basic backoff behavior.
    [update_cas atomic f] is, in essence,
    [let res, x = f !atomic in atomic := x; res]
    done atomically. [f] might be called multiple times and must be as cheap
    as possible.
    @since 3.17 *)
let update_cas (type res) (self : 'a t) (f : 'a -> res * 'a) : res =
  let exception Ret of res in
  let backoff = ref 1 in
  try
    while true do
      let old_val = get self in
      let res, new_val = f old_val in
      if compare_and_set self old_val new_val then raise_notrace (Ret res);

      Containers_domain.relax_loop !backoff;
      backoff := min 128 (2 * !backoff)
    done;
    assert false
  with Ret r -> r
