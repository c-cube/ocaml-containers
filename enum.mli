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

(** {1 Consumable generators} *)

(** This structure is inspired from Ocaml Batteries' BatEnum.t. It features
    restartable generators. *)

exception EOG
  (** End of Generation *)

type 'a t = unit -> 'a generator
  (** An enum is a generator of generators *)
and 'a generator = unit -> 'a
  (** A generator may be called several times, yielding the next value
      each time. It raises EOG when it reaches the end. *)

val empty : 'a t
  (** Enmpty enum *)

val singleton : 'a -> 'a t
  (** One-element enum *)

val start : 'a t -> 'a generator
  (** Create a new generator *)

val next : 'a generator -> 'a
  (** Get next element, or raise EOG *)

val junk : 'a generator -> unit
  (** Drop element *)

val is_empty : _ t -> bool
  (** Check whether the enum is empty *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on the generator *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on the enum *)

val length : _ t -> int
  (** Length of an enum (linear time) *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Lazy map *)

val append : 'a t -> 'a t -> 'a t
  (** Append the two enums *)

val flatten : 'a t t -> 'a t
  (** Flatten the enum of enum *)

val flatMap : ('a -> 'b t) -> 'a t -> 'b t
  (** Monadic bind *)

val of_list : 'a list -> 'a t
  (** Enumerate the list *)

val to_list : 'a t -> 'a list
  (** non tail-call trasnformation to list *)

val to_rev_list : 'a t -> 'a list
  (** Tail call conversion to list, in reverse order *)

