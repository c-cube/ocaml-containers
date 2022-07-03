module Test = (val Containers_testlib.make ~__FILE__ ())
open Test

type elt = { x: string; mutable rank: int; mutable idx: int }

module Elt = struct
  type t = elt

  let idx x = x.idx
  let set_idx x i = x.idx <- i

  let lt a b =
    if a.rank = b.rank then
      a.x < b.x
    else
      a.rank < b.rank
end

module H = CCMutHeap.Make (Elt);;

t @@ fun () ->
let h = H.create () in
let x1 = { x = "a"; rank = 10; idx = -1 } in
let x2 = { x = "b"; rank = 10; idx = -1 } in
let x3 = { x = "c"; rank = 10; idx = -1 } in
H.insert h x1;
assert (H.in_heap x1);
assert (not (H.in_heap x2));
assert (not (H.in_heap x3));
H.insert h x2;
H.insert h x3;

assert (Elt.lt x1 x2);
assert (Elt.lt x2 x3);

let x = H.remove_min h in
assert (x == x1);

let x = H.remove_min h in
assert (x == x2);

let x = H.remove_min h in
assert (x == x3);

assert (
  try
    ignore (H.remove_min h);
    false
  with Not_found -> true);
true
;;

t @@ fun () ->
let h = H.create () in
let x1 = { x = "a"; rank = 10; idx = -1 } in
let x2 = { x = "b"; rank = 10; idx = -1 } in
let x3 = { x = "c"; rank = 10; idx = -1 } in
H.insert h x1;
H.insert h x2;
H.insert h x3;

x3.rank <- 2;
H.decrease h x3;

assert (Elt.lt x3 x1);
assert (Elt.lt x3 x2);

let x = H.remove_min h in
assert (x == x3);

x1.rank <- 20;
H.increase h x1;

let x = H.remove_min h in
assert (x == x2);

let x = H.remove_min h in
assert (x == x1);

assert (
  try
    ignore (H.remove_min h);
    false
  with Not_found -> true);
true
