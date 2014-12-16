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

(** {1 Mutable polymorphic hash-set} *)

type 'a sequence = ('a -> unit) -> unit

type 'a t = ('a, unit) PHashtbl.t
  (** A set is a hashtable, with trivial values *)

val empty : ?max_load:float -> ?eq:('a -> 'a -> bool) ->
            ?hash:('a -> int) -> int -> 'a t
  (** See {!PHashtbl.create} *)

val copy : 'a t -> 'a t

val clear : 'a t -> unit

val cardinal : 'a t -> int

val mem : 'a t -> 'a -> bool

val add : 'a t -> 'a -> unit

val remove : 'a t -> 'a -> unit

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val filter : ('a -> bool) -> 'a t -> unit
  (** destructive filter (remove elements that do not satisfy the predicate) *)

val to_seq : 'a t -> 'a sequence

val of_seq : 'a t -> 'a sequence -> unit

val union : ?into:'a t -> 'a t -> 'a t -> 'a t
  (** Set union. The result is stored in [into] *)

val inter : ?into:'a t -> 'a t -> 'a t -> 'a t
  (** Set intersection. The result is stored in [into] *)
