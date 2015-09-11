(*
copyright (c) 2014, Carmelo Piccione
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

(** {1 Basic Float functions}
@since 0.6.1 *)

type t = float
type fpclass = Pervasives.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

val nan : t

val max_value : t
val min_value : t

val max_finite_value : t

val epsilon : float

val is_nan : t -> bool

val add : t -> t -> t

val sub : t -> t -> t

val neg : t -> t

val abs : t -> t

val scale : t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val equal : t -> t -> bool

val compare : float -> float -> int

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val pp : t printer
val print : t formatter

val hash : t -> int

val random : t -> t random_gen
val random_small : t random_gen
val random_range : t -> t -> t random_gen

val sign : t -> int
(** [sign t] is one of [-1, 0, 1], depending on how the float
    compares to [0.]
    @deprecated since 0.7 use {! fsign} or {!sign_exn} since it's more accurate *)

val fsign : t -> float
(** [fsign x] is one of [-1., -0., +0., +1.], or [nan] if [x] is NaN.
    @since 0.7 *)

exception TrapNaN of string
val sign_exn : t -> int
(** [sign_exn x] will return the sign of [x] as [1, 0] or [-1], or raise an
    exception [TrapNaN] if [x] is a NaN.
    Note that infinities have defined signs in OCaml.
    @since 0.7 *)

val to_int : t -> int
val of_int : int -> t

val to_string : t -> string
val of_string : string -> t


val equal_precision : epsilon:t -> t -> t -> bool
(** Equality with allowed error up to a non negative epsilon value *)

val classify : float -> fpclass
