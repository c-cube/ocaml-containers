
[%IFGE 4.12]

include Atomic


[%ELSE]

open CCShims_.Stdlib (* for == *)

type 'a t = {mutable x: 'a}
let[@inline] make x = {x}
let[@inline] get {x} = x
let[@inline] set r x = r.x <- x
let[@inline] exchange r x =
  let y = r.x in
  r.x <- x;
  y

let[@inline] compare_and_set r seen v =
  if r.x == seen then (
    r.x <- v;
    true
  ) else false

let[@inline] fetch_and_add r x =
  let v = r.x in
  r.x <- x + r.x;
  v

let[@inline] incr r = r.x <- 1 + r.x
let[@inline] decr r = r.x <- r.x - 1


[%ENDIF]
