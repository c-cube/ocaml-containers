
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Functions} *)

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"

#else

let (|>) x f = f x
let (@@) f x = f x

#endif

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 3

let opaque_identity = Sys.opaque_identity

#else

let opaque_identity x = x

#endif

let compose f g x = g (f x)

let compose_binop f g x y = g (f x) (f y)

let flip f x y = f y x

let curry f x y = f (x,y)

let id x = x

let const x _ = x

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

module Monad(X : sig type t end) = struct
  type 'a t = X.t -> 'a
  let return x _ = x
  let (>|=) f g x = g (f x)
  let (>>=) f g x = g (f x) x
end
