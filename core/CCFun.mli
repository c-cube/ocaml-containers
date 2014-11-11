
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

val (|>) : 'a -> ('a -> 'b) -> 'b
(** Pipeline. [x |> f] is the same as [f x]. *)

val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition *)

val (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Alias to [compose] *)

val (@@) : ('a -> 'b) -> 'a -> 'b
(** [f @@ x] is the same as [f x], but right-associative.
    @since NEXT_RELEASE *)

val id : 'a -> 'a
(** Identity function *)

val const : 'a -> 'b -> 'a
(** [const x y = x] for any [y] *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** flip arguments *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c

val tap : ('a -> 'b) -> 'a -> 'a
(** [tap f x] evaluates [f x], discards it, then returns [x]. Useful
    in a pipeline, for instance:
    {[CCArray.(1 -- 10)
      |> tap CCArray.shuffle
      |> tap CCArray.sort Pervasives.compare
    ]}
*)

val (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Mathematical composition *)

val lexicographic : ('a -> 'a -> int) -> ('a -> 'a -> int) -> 'a -> 'a -> int
(** Lexicographic combination of comparison functions *)

val finally : h:(unit -> unit) -> f:(unit -> 'a) -> 'a
  (** [finally h f] calls [f ()] and returns its result. If it raises, the
      same exception is raised; in {b any} case, [h ()] is called after
      [f ()] terminates. *)

(** {2 Monad}

functions with a fixed domain are monads in their codomain *)

module Monad(X : sig type t end) : sig
  type 'a t = X.t -> 'a
  val return : 'a -> 'a t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end
