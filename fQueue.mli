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

(** {1 Functional queues (fifo)} *)

type 'a t
  (** Queue containing elements of type 'a *)

val empty : 'a t

val is_empty : 'a t -> bool

val push : 'a t -> 'a -> 'a t
  (** Push element at the end of the queue *)

val peek : 'a t -> 'a
  (** Get first element, or raise Invalid_argument *)

val pop : 'a t -> 'a * 'a t
  (** Get and remove the first element, or raise Invalid_argument *)

val junk : 'a t -> 'a t
  (** Remove first element. If queue is empty, do nothing. *)

val append : 'a t -> 'a t -> 'a t
  (** Append two queues. Elements from the second one come
      after elements of the first one *)

val size : 'a t -> int
  (** Number of elements in the queue (linear in time) *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val iter : ('a -> unit) -> 'a t -> unit

val to_seq : 'a t -> 'a Sequence.t

val of_seq : 'a Sequence.t -> 'a t

