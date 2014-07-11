
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

val map2 : ('a -> 'b) -> (string -> string) -> 'a t -> 'b t
(** Same as {!map}, but also with a function that can transform
    the error message in case of failure *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val equal : 'a equal -> 'a t equal

val compare : 'a ord -> 'a t ord

val fold : success:('a -> 'b) -> failure:(string -> 'b) -> 'a t -> 'b
(** [fold ~success ~failure e] opens [e] and, if [e = `Ok x], returns
    [success x], otherwise [e = `Error s] and it returns [failure s]. *)

(** {2 Wrappers} *)

val guard : (unit -> 'a) -> 'a t
(** [guard f] runs [f ()] and returns its result wrapped in [`Ok]. If
    [f ()] raises some exception [e], then it fails with [`Error msg]
    where [msg] is some printing of [e] (see {!register_printer}). *)

val wrap1 : ('a -> 'b) -> 'a -> 'b t
(** Same as {!guard} but gives the function one argument. *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t
(** Same as {!guard} but gives the function two arguments. *)

val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd t

(** {2 APplicative} *)

val pure : 'a -> 'a t

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

(** {2 Collections} *)

val map_l : ('a -> 'b t) -> 'a list -> 'b list t

val fold_l : ('b -> 'a -> 'b t) -> 'b -> 'a list -> 'b t

val fold_seq : ('b -> 'a -> 'b t) -> 'b -> 'a sequence -> 'b t

(** {2 Misc} *)

val choose : 'a t list -> 'a t
(** [choose l] selects a member of [l] that is a [`Ok _] value,
    or returns [`Error msg] otherwise, where [msg] is obtained by
    combining the error messages of all elements of [l] *)

val retry : int -> (unit -> 'a t) -> 'a t
(** [retry n f] calls [f] at most [n] times, returning the first result
    of [f ()] that doesn't fail. If [f] fails [n] times, [retry n f] fails. *)

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t

  val retry_m : int -> (unit -> 'a t M.t) -> 'a t M.t
end

(** {2 Conversions} *)

val to_opt : 'a t -> 'a option

val of_opt : 'a option -> 'a t

val to_seq : 'a t -> 'a sequence

(** {2 IO} *)

val pp : 'a printer -> 'a t printer

val print : 'a formatter -> 'a t formatter

(** {2 Global Exception Printers}

One can register exception printers here, so they will be used by {!guard},
{!wrap1}, etc. The printers should succeed (print) on exceptions they
can deal with, and re-raise the exception otherwise. For instance
if I register a printer for [Not_found], it could look like:
  
{[CCError.register_printer
    (fun buf exn -> match exn with
      | Not_found -> Buffer.add_string buf "Not_found"
      | _ -> raise exn
    );;
]}
This way a printer that doesn't know how to deal with an exception will
let other printers do it. *)

val register_printer : exn printer -> unit
