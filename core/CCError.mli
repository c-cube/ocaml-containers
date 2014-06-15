
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

(** {1 Error Monad} *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type +'a t =
  [ `Ok of 'a
  | `Error of string
  ]

val return : 'a -> 'a t

val fail : string -> 'a t

val of_exn : exn -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val guard : (unit -> 'a) -> 'a t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val equal : 'a equal -> 'a t equal

val compare : 'a ord -> 'a t ord

(** {2 Collections} *)

val map_l : ('a -> 'b t) -> 'a list -> 'b list t

val fold_l : ('acc -> 'a -> 'acc t) -> 'acc -> 'a list -> 'acc t

val fold_seq : ('acc -> 'a -> 'acc t) -> 'acc -> 'a sequence -> 'acc t

(** {2 Conversions} *)

val to_opt : 'a t -> 'a option

val of_opt : 'a option -> 'a t

val to_seq : 'a t -> 'a sequence

(** {2 IO} *)

val pp : 'a printer -> 'a t printer

val print : 'a formatter -> 'a t formatter
