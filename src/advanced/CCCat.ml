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

(** {1 Categorical Constructs} *)

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

module type FREE_MONAD = sig
  module F : FUNCTOR

  type +'a t =
    | Return of 'a
    | Roll of 'a t F.t

  include MONAD with type 'a t := 'a t
  val inj : 'a F.t -> 'a t
end

(** {2 Some Implementations} *)

module WrapMonad(M : MONAD_BARE) = struct
  include M

  let map f x = x >>= (fun x -> return (f x))

  let pure = return

  let (<*>) f x = f >>= fun f -> x >>= fun x -> return (f x)
end


module MakeFree(F : FUNCTOR) = struct
  module F = F

  type 'a t =  Return of 'a | Roll of ('a t F.t)

  let return x = Return x
  let pure = return

  let rec map : type a b. (a -> b) -> a t -> b t
    = fun f x -> match x with
    | Return x -> Return (f x)
    | Roll xs -> Roll (F.map (map f) xs)

  let rec _bind : type a b. (a -> b t) -> a t -> b t
    = fun f x -> match x with
    | Return x -> f x
    | Roll y -> Roll (F.map (_bind f) y)

  let (>>=) x f = _bind f x

  let rec _app : type a b. (a -> b) t -> a t -> b t
    = fun f x -> match f, x with
    | Return f, Return x -> Return (f x)
    | Return f, Roll xs -> Roll (F.map (map f) xs)
    | Roll fs, _ -> Roll (F.map (fun f -> _app f x) fs)

  let (<*>) = _app

  let inj x = Roll (F.map return x)
end

module MakeFreeFold(FM : FREE_MONAD)(Fold : FOLDABLE with type 'a t = 'a FM.F.t) = struct
  type 'a t = 'a FM.t

  let rec to_seq : type a. a FM.t -> a sequence
    = fun x k -> match x with
    | FM.Return x -> k x
    | FM.Roll xs -> Fold.to_seq xs (fun x -> to_seq x k)
end
