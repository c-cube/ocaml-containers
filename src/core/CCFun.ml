(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Functions} *)

(* import standard implementations, if any *)

include Sys
include Stdlib
include Fun

let[@inline] and_pred f g x = f x && g x
let[@inline] or_pred f g x = f x || g x
let[@inline] compose f g x = g (f x)
let[@inline] compose_binop f g x y = g (f x) (f y)
let[@inline] curry f x y = f (x, y)
let[@inline] uncurry f (x, y) = f x y

let[@inline] tap f x =
  ignore (f x);
  x

let lexicographic f1 f2 x y =
  let c = f1 x y in
  if c <> 0 then
    c
  else
    f2 x y

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

let[@inline] with_return (type ret) f : ret =
  let exception E of ret in
  let return x = raise_notrace (E x) in
  match f return with
  | res -> res
  | exception E res -> res

module Infix = struct
  (* default implem for some operators *)
  let ( %> ) = compose
  let[@inline] ( % ) f g x = f (g x)
  let ( let@ ) = ( @@ )
  let ( ||> ) (a, b) f = f a b
  let ( |||> ) (a, b, c) f = f a b c
end

include Infix

module Monad (X : sig
  type t
end) =
struct
  type 'a t = X.t -> 'a

  let[@inline] return x _ = x
  let[@inline] ( >|= ) f g x = g (f x)
  let[@inline] ( >>= ) f g x = g (f x) x
end
[@@inline]
