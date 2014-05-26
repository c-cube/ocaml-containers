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

(** {1 Continuation List} *)

type + 'a t = unit -> 
  [ `Nil
  | `Cons of 'a * 'a t
  ]

val nil : 'a t

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

val singleton : 'a -> 'a t

val is_empty : 'a t -> bool

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list
(** Gather all values into a list *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val to_seq : 'a t -> 'a sequence
val to_gen : 'a t -> 'a gen

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on values *)

val iter : ('a -> unit) -> 'a t -> unit

val length : 'a t -> int

val take : int -> 'a t -> 'a t

val drop : int -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val fmap : ('a -> 'b option) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val flatten : 'a t t -> 'a t

val range : int -> int -> int t

val (--) : int -> int -> int t
