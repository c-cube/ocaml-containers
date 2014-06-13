
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

(** {1 Options} *)

type 'a t = 'a option

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform the element inside, if any *)

val maybe : ('a -> 'b) -> 'b -> 'a t -> 'b
(** [maybe f x o] is [x] if [o] is [None], otherwise it's [f y] if [o = Some y] *)

val is_some : _ t -> bool

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val return : 'a -> 'a t
(** Monadic return *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of {!map} *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Flip version of {!>>=} *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val (<$>) : ('a -> 'b) -> 'a t -> 'b t

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on 0 or 1 elements *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on 0 or 1 elements *)

(** {2 Conversion and IO} *)

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t
(** Head of list, or [None] *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Buffer.t -> 'a -> unit

val to_gen : 'a t -> 'a gen
val to_seq : 'a t -> 'a sequence

val pp : 'a printer -> 'a t printer

