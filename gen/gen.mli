(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Generators}

Values of type ['a Gen.t] represent a possibly infinite sequence of values
of type 'a. One can only iterate once on the sequence, as it is consumed
by iteration/deconstruction/access. [None] is returned when the generator
is exhausted.

The submodule {!Restart} provides utilities to work with
{b restartable generators}, that is, functions [unit -> 'a Gen.t] that
allow to build as many generators from the same source as needed.
*)

(** {2 Global type declarations} *)

type 'a t = unit -> 'a option
  (** A generator may be called several times, yielding the next value
      each time. It returns [None] when no elements remain *)

type 'a gen = 'a t

module type S = Gen_intf.S

(** {2 Transient generators} *)

val get : 'a t -> 'a option
  (** Get the next value *)

val next : 'a t -> 'a option
  (** Synonym for {!get} *)

val get_exn : 'a t -> 'a
  (** Get the next value, or fails
      @raise Invalid_argument if no element remains *)

val junk : 'a t -> unit
  (** Drop the next value, discarding it. *)

val repeatedly : (unit -> 'a) -> 'a t
  (** Call the same function an infinite number of times (useful for instance
      if the function is a random generator). *)

include S with type 'a t := 'a gen
  (** Operations on {b transient} generators *)

(** {2 Restartable generators} *)

module Restart : sig
  type 'a t = unit -> 'a gen

  type 'a restartable = 'a t

  include S with type 'a t := 'a restartable

  val cycle : 'a t -> 'a t
    (** Cycle through the enum, endlessly. The enum must not be empty. *)

  val lift : ('a gen -> 'b) -> 'a t -> 'b

  val lift2 : ('a gen -> 'b gen -> 'c) -> 'a t -> 'b t -> 'c
end

(** {2 Utils} *)

val persistent : 'a t -> 'a Restart.t
  (** Store content of the transient generator in memory, to be able to iterate
      on it several times later. If possible, consider using combinators
      from {!Restart} directly instead. *)

val persistent_lazy : 'a t -> 'a Restart.t
  (** Same as {!persistent}, but consumes the generator on demand (by chunks).
      This allows to make a restartable generator out of an ephemeral one,
      without paying a big cost upfront (nor even consuming it fully).
      @since 0.2.2 *)

val start : 'a Restart.t -> 'a t
  (** Create a new transient generator.
      [start gen] is the same as [gen ()] but is included for readability. *)
