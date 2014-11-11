
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

(** {1 Error Monad}

The variant is polymorphic in the error type since 0.5 *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type (+'good, +'bad) t =
  [ `Ok of 'good
  | `Error of 'bad
  ]

val return : 'a -> ('a,'err) t
(** Successfully return a value *)

val fail : 'err -> ('a,'err) t
(** Fail with an error *)

val of_exn : exn -> ('a, string) t
(** [of_exn e] uses {!Printexc} to print the exception as a string *)

val fail_printf : ('a, Buffer.t, unit, ('a,string) t) format4 -> 'a
(** [fail_printf format] uses [format] to obtain an error message
    and then returns [`Error msg]
    @since 0.3.3 *)

val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
(** Map on success *)

val map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t
(** Map on error.
    @since 0.5 *)

val map2 : ('a -> 'b) -> ('err -> 'err) -> ('a, 'err) t -> ('b, 'err) t
(** Same as {!map}, but also with a function that can transform
    the error message in case of failure *)

val iter : ('a -> unit) -> ('a, _) t -> unit
(** Apply the function only in case of `Ok *)

val get_exn : ('a, _) t -> 'a
(** Extract the value [x] from [`Ok x], fails otherwise.
    You should be careful with this function, and favor other combinators
    whenever possible.
    @raise Invalid_argument if the value is an error. *)

val flat_map : ('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t

val (>|=) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t

val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

val equal : ?err:'err equal -> 'a equal -> ('a, 'err) t equal

val compare : ?err:'err ord -> 'a ord -> ('a, 'err) t ord

val fold : success:('a -> 'b) -> failure:('err -> 'b) -> ('a, 'err) t -> 'b
(** [fold ~success ~failure e] opens [e] and, if [e = `Ok x], returns
    [success x], otherwise [e = `Error s] and it returns [failure s]. *)

(** {2 Wrappers}

The functions {!guard}, {!wrap1}, {!wrap2} and {!wrap3} now return
exceptions in case of failure, @since 0.5 *)

val guard : (unit -> 'a) -> ('a, exn) t
(** [guard f] runs [f ()] and returns its result wrapped in [`Ok]. If
    [f ()] raises some exception [e], then it fails with [`Error e] *)

val guard_str : (unit -> 'a) -> ('a, string) t
(** Same as {!guard} but uses {!of_exn} to print the exception.
    See {!register_printer} *)

val wrap1 : ('a -> 'b) -> 'a -> ('b, exn) t
(** Same as {!guard} but gives the function one argument. *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
(** Same as {!guard} but gives the function two arguments. *)

val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t

(** {2 Applicative} *)

val pure : 'a -> ('a, 'err) t

val (<*>) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t

(** {2 Collections} *)

val map_l : ('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t

val fold_l : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a list -> ('b, 'err) t

val fold_seq : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a sequence -> ('b, 'err) t

(** {2 Misc} *)

val choose : ('a, 'err) t list -> ('a, 'err list) t
(** [choose l] selects a member of [l] that is a [`Ok _] value,
    or returns [`Error l] otherwise, where [l] is the list of errors. *)

val retry : int -> (unit -> ('a, 'err) t) -> ('a, 'err list) t
(** [retry n f] calls [f] at most [n] times, returning the first result
    of [f ()] that doesn't fail. If [f] fails [n] times, [retry n f] fails
    with the list of successive errors. *)

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : ('a M.t, 'err) t -> ('a, 'err) t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> ('a, 'err) t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> ('a, 'err) t -> ('b, 'err) t M.t

  val retry_m : int -> (unit -> ('a, 'err) t M.t) -> ('a, 'err list) t M.t
end

(** {2 Conversions} *)

val to_opt : ('a, _) t -> 'a option

val of_opt : 'a option -> ('a, string) t

val to_seq : ('a, _) t -> 'a sequence

(** {2 IO} *)

val pp : 'a printer -> ('a, string) t printer

val print : 'a formatter -> ('a, string) t formatter

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
