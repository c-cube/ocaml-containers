
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

(** {1 Basic String Utils}
Consider using KMP instead. *)

type t = string

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

val is_sub : sub:t -> int -> t -> int -> bool
(** [is_sub ~sub i s j] returns [true] iff [sub] is a substring of [s] starting
    at position [j] *)

val split : by:t -> t -> t list
(** split the given string along the given separator [by]. Should only
    be used with very small separators, otherwise use {!KMP}.
    @raise Failure if [by = ""] *)

val split_gen : by:t -> t -> t gen

val split_seq : by:t -> t -> t sequence

val find : ?start:int -> sub:t -> t -> int
(** Find [sub] in the string, returns its first index or -1.
    Should only be used with very small [sub] *)

val repeat : t -> int -> t
(** The same string, repeated n times *)

val prefix : pre:t -> t -> bool
(** [str_prefix ~pre s] returns [true] iff [pre] is a prefix of [s] *)

val to_gen : t -> char gen
val of_gen : char gen -> t

val to_seq : t -> char sequence
val of_seq : char sequence -> t

val pp : Buffer.t -> t -> unit
