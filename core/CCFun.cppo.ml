
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Basic Functions} *)

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"

#else

let (|>) x f = f x
let (@@) f x = f x

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
    h ();
    x
  with e ->
    h ();
    raise e

module Monad(X : sig type t end) = struct
  type 'a t = X.t -> 'a
  let return x _ = x
  let (>|=) f g x = g (f x)
  let (>>=) f g x = g (f x) x
end
