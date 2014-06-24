
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

(** {1 Random Generators} *)

type state = Random.State.t

type 'a t = state -> 'a
(** Random generator for values of type ['a] *)

type 'a random_gen = 'a t

val return : 'a -> 'a t
(** [return x] is the generator that always returns [x].
    Example:  [let random_int = return 4 (* fair dice roll *)] *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val choose : 'a t list -> 'a t
(** Choose a generator within the list.
    @raise Invalid_argument if the list is empty *)

val choose_array : 'a t array -> 'a t

val choose_return : 'a list -> 'a t
(** Choose among the list
    @raise Invalid_argument if the list is empty *)

(** {2 Fuel and Backtracking} *)

module Fuel : sig
  type fuel = int
  (** The fuel is a value that represents some "resource" used by the
      random generator. *)

  val split : fuel -> (fuel * fuel) option t
  (** Split a (fuel) value [n] into [n1,n2] where [n = n1 + n2].
      @return [None] if the value is too small *)

  val split_list : fuel -> len:int -> fuel list option t
  (** Split a (fuel) value [n] into a list of values whose sum is [n]
      and whose length is [length].
      @return [None] if the value is too small *)

  (** {6 Fueled Generators} *)

  type 'a t = fuel -> state -> (fuel * 'a) option
  (** Fueled generators use some fuel to generate a value.
      Can fail by lack of fuel. *)

  val return : 'a -> 'a t
  (** [return x] is the generator that always returns [x], and consumes one
      fuel doing it. *)

  val return' : fuel -> 'a -> 'a t
  (** [return' f x] returns [x] but also consumes [fuel]. *)

  val flat_map : ('a -> 'b t) -> 'a t -> 'b t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t

  val consume : unit t
  (** Consume one fuel value *)

  val consume' : fuel -> unit t
  (** Consume the given amount of fuel *)

  val fail : 'a t
  (** Always fails. *)

  val retry : ?max:int -> 'a t -> 'a t
  (** [retry g] calls [g] until it returns some value, or until the maximum
      number of retries was reached. If [g] fails,
      then it counts for one iteration, and the generator retries.
      @param max: maximum number of retries. Default [10] *)

  val try_successively : 'a t list -> 'a t
  (** [try_successively l] tries each generator of [l], one after the other.
      If some generator succeeds its result is returned, else the
      next generator is tried *)

  val (<?>) : 'a t -> 'a t -> 'a t
  (** [a <?> b] is a choice operator. It first tries [a], and returns its
      result if successful. If [a] fails, then [b] is returned. *)

  val fix : ('a t -> 'a t) -> 'a t
  (** Recursion combinators, for building (fueled) recursive values *)

  val lift : 'a random_gen -> 'a t
  (** lifts a regular random generator into a fueled one, that consumes
      one fuel unit *)

  val lift' : fuel -> 'a random_gen -> 'a t
  (** lifts a regular random generator into a fueled one, that consumes
      one fuel unit *)

  (** {6 Running} *)

  val run : ?fuel:fuel random_gen -> 'a t -> 'a option random_gen
  (** Run the given fueled generator with an amount of fuel
      given by the [fuel] generator.
      @return None if the *)

  exception GenFailure

  val run_exn : ?fuel:fuel random_gen -> 'a t -> 'a random_gen
  (** Same as {!run}, but in case of failure it raises an exception.
      @raise GenFailure in case the generator fails *)
end
