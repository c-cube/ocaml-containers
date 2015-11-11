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

(** {1 Categorical Constructs}

Attempt to copy some structures from Haskell and the likes. Disclaimer:
I don't know much about category theory, only about type signatures ;). *)

(** {2 Signatures} *)

module type MONOID = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module type FUNCTOR = sig
  type +'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  type +'a t
  include FUNCTOR with type 'a t := 'a t
  val pure : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

module type MONAD_BARE = sig
  type +'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD = sig
  include MONAD_BARE
  include APPLICATIVE with type 'a t := 'a t
end

module type MONAD_TRANSFORMER = sig
  include MONAD
  module M : MONAD
  val lift : 'a M.t -> 'a t
end

(** Cheating: use an equivalent of "to List" with a sequence *)
type 'a sequence = ('a -> unit) -> unit

module type FOLDABLE = sig
  type 'a t
  val to_seq : 'a t -> 'a sequence
end

module type TRAVERSE = functor(M : MONAD) -> sig
  type +'a t

  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t
end

(** The free monad is built by nesting applications of a functor [F].

For instance, Lisp-like nested lists can be built and dealt with like this:
{[
  module Lisp = CCCat.FreeMonad(CCList);;

  let l = Lisp.(inj [1;2;3]  >>= fun x -> inj [x; x*2; x+100]);;
]} *)
module type FREE_MONAD = sig
  module F : FUNCTOR

  type +'a t =
    | Return of 'a
    | Roll of 'a t F.t

  include MONAD with type 'a t := 'a t
  val inj : 'a F.t -> 'a t
end

(** {2 Some Implementations} *)

(** Implement the applicative and functor modules from only return and bind *)
module WrapMonad(M : MONAD_BARE) : MONAD with type 'a t = 'a M.t

module MakeFree(F : FUNCTOR) : FREE_MONAD with module F = F

module MakeFreeFold(FM : FREE_MONAD)(Fold : FOLDABLE with type 'a t = 'a FM.F.t)
  : FOLDABLE with type 'a t = 'a FM.t
