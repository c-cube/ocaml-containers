
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Error Monad}

    Uses the new "result" type from OCaml 4.03.

    @since 0.16 *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type (+'good, +'bad) t = ('good, 'bad) Result.result =
  | Ok of 'good
  | Error of 'bad

val return : 'a -> ('a, 'err) t
(** Successfully return a value *)

val fail : 'err -> ('a, 'err) t
(** Fail with an error *)

val of_exn : exn -> ('a, string) t
(** [of_exn e] uses {!Printexc} to print the exception as a string *)

val of_exn_trace : exn -> ('a, string) t
(** [of_exn_trace e] is similar to [of_exn e], but it adds the stacktrace
    to the error message.

    Remember to call [Printexc.record_backtrace true] and compile with the
    debug flag for this to work. *)

val fail_printf : ('a, Buffer.t, unit, ('a, string) t) format4 -> 'a
(** [fail_printf format] uses [format] to obtain an error message
    and then returns [Error msg] *)

val fail_fprintf : ('a, Format.formatter, unit, ('a, string) t) format4 -> 'a
(** [fail_printf format] uses [format] to obtain an error message
    and then returns [Error msg] *)

val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
(** Map on success *)

val map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t
(** Map on the error variant *)

val map2 : ('a -> 'b) -> ('err1 -> 'err2) -> ('a, 'err1) t -> ('b, 'err2) t
(** Same as {!map}, but also with a function that can transform
    the error message in case of failure *)

val iter : ('a -> unit) -> ('a, _) t -> unit
(** Apply the function only in case of Ok *)

exception Get_error

val get_exn : ('a, _) t -> 'a
(** Extract the value [x] from [Ok x], fails otherwise.
    You should be careful with this function, and favor other combinators
    whenever possible.
    @raise Get_error if the value is an error. *)

val get_or : ('a, _) t -> default:'a -> 'a
(** [get_or e ~default] returns [x] if [e = Ok x], [default] otherwise *)

val map_or : ('a -> 'b) ->  ('a, 'b) t -> default:'b -> 'b
(** [map_or f e ~default] returns [f x] if [e = Ok x], [default] otherwise *)

val catch : ('a, 'err) t -> ok:('a -> 'b) -> err:('err -> 'b) -> 'b
(** [catch e ~ok ~err] calls either [ok] or [err] depending on
    the value of [e]. *)

val flat_map : ('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t

val (>|=) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t

val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

val equal : ?err:'err equal -> 'a equal -> ('a, 'err) t equal

val compare : ?err:'err ord -> 'a ord -> ('a, 'err) t ord

val fold : ok:('a -> 'b) -> error:('err -> 'b) -> ('a, 'err) t -> 'b
(** [fold ~ok ~error e] opens [e] and, if [e = Ok x], returns
    [ok x], otherwise [e = Error s] and it returns [error s]. *)

(** {2 Wrappers} *)

val guard : (unit -> 'a) -> ('a, exn) t
(** [guard f] runs [f ()] and returns its result wrapped in [Ok]. If
    [f ()] raises some exception [e], then it fails with [Error e] *)

val guard_str : (unit -> 'a) -> ('a, string) t
(** Same as {!guard} but uses {!of_exn} to print the exception. *)

val guard_str_trace : (unit -> 'a) -> ('a, string) t
(** Same as {!guard_str} but uses {!of_exn_trace} instead of {!of_exn} so
    that the stack trace is printed. *)

val wrap1 : ('a -> 'b) -> 'a -> ('b, exn) t
(** Same as {!guard} but gives the function one argument. *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
(** Same as {!guard} but gives the function two arguments. *)

val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t
(** Same as {!guard} but gives the function three arguments. *)

(** {2 Applicative} *)

val pure : 'a -> ('a, 'err) t
(** Synonym of {!return} *)

val (<*>) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
(** [a <*> b] evaluates [a] and [b], and, in case of success, returns
    [Ok (a b)]. Otherwise, it fails, and the error of [a] is chosen
    over the error of [b] if both fail. *)

val join : (('a, 'err) t, 'err) t -> ('a, 'err) t
(** [join t], in case of success, returns [Ok o] from [Ok (Ok o)]. Otherwise,
    it fails with [Error e] where [e] is the unwrapped error of [t]. *)

val both : ('a, 'err) t  -> ('b, 'err) t -> (('a * 'b), 'err) t
(** [both a b], in case of success, returns [Ok (o, o')] with the ok values
    of [a] and [b]. Otherwise, it fails, and the error of [a] is chosen over the
    error of [b] if both fail. *)

(** {2 Infix} *)

module Infix : sig
  val (>|=) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
  val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val (<*>) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
end

(** {2 Collections} *)

val map_l : ('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t

val fold_l : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a list -> ('b, 'err) t

val fold_seq : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a sequence -> ('b, 'err) t

(** {2 Misc} *)

val choose : ('a, 'err) t list -> ('a, 'err list) t
(** [choose l] selects a member of [l] that is a [Ok _] value,
    or returns [Error l] otherwise, where [l] is the list of errors. *)

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

type ('a, 'b) error = [`Ok of 'a | `Error of 'b]

val of_err : ('a, 'b) error -> ('a, 'b) t
(** @since 0.17 *)

val to_err : ('a, 'b) t -> ('a, 'b) error
(** @since 0.17 *)

(** {2 IO} *)

val pp : 'a printer -> ('a, string) t printer

val pp': 'a printer -> 'e printer -> ('a, 'e) t printer
(** Printer that is generic on the error type
    @since 0.19 *)

val print : 'a formatter -> ('a, string) t formatter

val print' : 'a formatter -> 'e formatter -> ('a, 'e) t formatter
(** Printer that is generic on the error type
    @since 0.19 *)

