
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Functions} *)

(* default implem for some operators *)

let (|>) x f = f x
let (@@) f x = f x

let opaque_identity x = x

(* import standard implementations, if any *)

include Sys
include CCShims_.Stdlib
include CCShimsFun_

let compose f g x = g (f x)

let compose_binop f g x y = g (f x) (f y)

let curry f x y = f (x,y)

let uncurry f (x,y) = f x y

let tap f x = ignore (f x); x

let (%>) = compose

let (%) f g x = f (g x)

let lexicographic f1 f2 x y =
  let c = f1 x y in
  if c <> 0 then c else f2 x y

let finally ~h ~f =
  try
    let x = f () in
    ignore (h ());
    x
  with e ->
    ignore (h ());
    raise e

let finally1 ~h f x =
  try
    let res = f x in
    ignore (h ());
    res
  with e ->
    ignore (h ());
    raise e

let finally2 ~h f x y =
  try
    let res = f x y in
    ignore (h ());
    res
  with e ->
    ignore (h ());
    raise e

let rec iterate n f x =
  if n < 0 then
    invalid_arg "CCFun.iterate"
  else if n = 0 then
    x
  else
    iterate (n - 1) f (f x)

(*$= iterate & ~printer:Q.Print.int
  10 (iterate 0 succ 10)
  11 (iterate 1 succ 10)
  12 (iterate 2 succ 10)
  15 (iterate 5 succ 10)
*)
(*$R
  assert_raises
  (Invalid_argument "CCFun.iterate")
  (fun () -> iterate (-1) succ 10)
*)

module Monad(X : sig type t end) = struct
  type 'a t = X.t -> 'a
  let return x _ = x
  let (>|=) f g x = g (f x)
  let (>>=) f g x = g (f x) x
end
