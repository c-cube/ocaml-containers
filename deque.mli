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

(** {1 Imperative deque} *)

type 'a t
  (** Contains 'a elements, queue in both ways *)

exception Empty

val create : unit -> 'a t
  (** New deque *)

val is_empty : 'a t -> bool
  (** Is the deque empty? *)

val length : 'a t -> int
  (** Number of elements (linear) *)

val push_front : 'a t -> 'a -> unit
  (** Push value at the front *)

val push_back : 'a t -> 'a -> unit
  (** Push value at the back *)

val peek_front : 'a t -> 'a
  (** First value, or Empty *)

val peek_back : 'a t -> 'a
  (** Last value, or Empty *)

val take_back : 'a t -> 'a
  (** Take last value, or raise Empty *)

val take_front : 'a t -> 'a
  (** Take first value, or raise Empty *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on elements *)

val of_seq : ?deque:'a t -> 'a Sequence.t -> 'a t
val to_seq : 'a t -> 'a Sequence.t

val copy : 'a t -> 'a t
  (** Fresh copy *)
