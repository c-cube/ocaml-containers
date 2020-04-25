(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Error Monad}

    Uses the new "result" type from OCaml 4.03.

    @since 0.16 *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type nonrec (+'good, +'bad) result = ('good, 'bad) result =
  | Ok of 'good
  | Error of 'bad

type (+'good, +'bad) t = ('good, 'bad) result =
  | Ok of 'good
  | Error of 'bad

val return : 'a -> ('a, 'err) t
(** Successfully return a value. *)

val fail : 'err -> ('a, 'err) t
(** Fail with an error. *)

val of_exn : exn -> ('a, string) t
(** [of_exn e] uses {!Printexc} to print the exception as a string. *)

val of_exn_trace : exn -> ('a, string) t
(** [of_exn_trace e] is similar to [of_exn e], but it adds the stacktrace
    to the error message.

    Remember to call [Printexc.record_backtrace true] and compile with the
    debug flag for this to work. *)

val fail_printf : ('a, Buffer.t, unit, ('b, string) t) format4 -> 'a
(** [fail_printf format] uses [format] to obtain an error message
    and then returns [Error msg]. *)

val fail_fprintf : ('a, Format.formatter, unit, ('b, string) t) format4 -> 'a
(** [fail_fprintf format] uses [format] to obtain an error message
    and then returns [Error msg]. *)

val add_ctx : string -> ('a, string) t -> ('a, string) t
(** [add_ctx msg] leaves [Ok x] untouched, but transforms
    [Error s] into [Error s'] where [s'] contains the additional
    context given by [msg].
    @since 1.2 *)

val add_ctxf : ('a, Format.formatter, unit, ('b, string) t -> ('b, string) t) format4 -> 'a
(** [add_ctxf format_message] is similar to {!add_ctx} but with
    {!Format} for printing the message (eagerly).
    Example:
    {[
      add_ctxf "message(number %d, foo: %B)" 42 true (Error "error)"
    ]}
    @since 1.2 *)

val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
(** Map on success. *)

val map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t
(** Map on the error variant. *)

val map2 : ('a -> 'b) -> ('err1 -> 'err2) -> ('a, 'err1) t -> ('b, 'err2) t
(** Like {!map}, but also with a function that can transform
    the error message in case of failure. *)

val iter : ('a -> unit) -> ('a, _) t -> unit
(** Apply the function only in case of [Ok]. *)

val iter_err : ('err -> unit) -> (_, 'err) t -> unit
(** Apply the function in case of [Error].
    @since 2.4 *)

exception Get_error

val get_exn : ('a, _) t -> 'a
(** Extract the value [x] from [Ok x], fails otherwise.
    You should be careful with this function, and favor other combinators
    whenever possible.
    @raise Get_error if the value is an error. *)

val get_or : ('a, _) t -> default:'a -> 'a
(** [get_or e ~default] returns [x] if [e = Ok x], [default] otherwise. *)

val get_or_failwith : ('a, string) t -> 'a
(** [get_or_failwith e] returns [x] if [e = Ok x], fails otherwise.
    @raise Failure with [msg] if [e = Error msg].
    @since 2.4 *)

val get_lazy : ('b -> 'a) -> ('a, 'b) t -> 'a
(** [get_lazy default_fn x] unwraps [x], but if [x = Error e] it returns [default_fr e] instead.
    @since NEXT_RELEASE *)

val map_or : ('a -> 'b) ->  ('a, 'c) t -> default:'b -> 'b
(** [map_or f e ~default] returns [f x] if [e = Ok x], [default] otherwise. *)

val catch : ('a, 'err) t -> ok:('a -> 'b) -> err:('err -> 'b) -> 'b
(** [catch e ~ok ~err] calls either [ok] or [err] depending on
    the value of [e]. *)

val flat_map : ('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t

val (>|=) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t

val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
(** Monadic composition. [e >>= f] proceeds as [f x] if [e] is [Ok x]
    or returns [e] if [e] is an [Error]. *)

val equal : err:'err equal -> 'a equal -> ('a, 'err) t equal

val compare : err:'err ord -> 'a ord -> ('a, 'err) t ord

val fold : ok:('a -> 'b) -> error:('err -> 'b) -> ('a, 'err) t -> 'b
(** [fold ~ok ~error e] opens [e] and, if [e = Ok x], returns
    [ok x], otherwise [e = Error s] and it returns [error s]. *)

val fold_ok : ('a -> 'b -> 'a) -> 'a -> ('b, _) t -> 'a
(** [fold_ok f acc r] will compute [f acc x] if [r=Ok x],
    and return [acc] otherwise, as if the result were a mere option.
    @since 1.2 *)

val is_ok : ('a, 'err) t -> bool
(** Return true if [Ok].
    @since 1.0 *)

val is_error : ('a, 'err) t -> bool
(** Return true if [Error].
    @since 1.0 *)

(** {2 Wrappers} *)

val guard : (unit -> 'a) -> ('a, exn) t
(** [guard f] runs [f ()] and returns its result wrapped in [Ok]. If
    [f ()] raises some exception [e], then it fails with [Error e]. *)

val guard_str : (unit -> 'a) -> ('a, string) t
(** Like {!guard} but uses {!of_exn} to print the exception. *)

val guard_str_trace : (unit -> 'a) -> ('a, string) t
(** Like {!guard_str} but uses {!of_exn_trace} instead of {!of_exn} so
    that the stack trace is printed. *)

val wrap1 : ('a -> 'b) -> 'a -> ('b, exn) t
(** Like {!guard} but gives the function one argument. *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
(** Like {!guard} but gives the function two arguments. *)

val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t
(** Like {!guard} but gives the function three arguments. *)

(** {2 Applicative} *)

val pure : 'a -> ('a, 'err) t
(** Synonym of {!return}. *)

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
  (** Monadic composition. [e >>= f] proceeds as [f x] if [e] is [Ok x]
      or returns [e] if [e] is an [Error]. *)

  val (<*>) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
  (** [a <*> b] evaluates [a] and [b], and, in case of success, returns
      [Ok (a b)]. Otherwise, it fails, and the error of [a] is chosen
      over the error of [b] if both fail. *)

  (** Let operators on OCaml >= 4.08.0, nothing otherwise
      @since 2.8 *)
  include CCShimsMkLet_.S2 with type ('a,'e) t_let2 := ('a,'e) result
end

(** Let operators on OCaml >= 4.08.0, nothing otherwise
    @since 2.8 *)
include CCShimsMkLet_.S2 with type ('a,'e) t_let2 := ('a,'e) result


(** {2 Collections} *)

val flatten_l : ('a, 'err) t list -> ('a list, 'err) t
(** Same as [map_l id]: returns [Ok [x1;…;xn]] if [l=[Ok x1; …; Ok xn]],
    or the first error otherwise.
    @since 2.7
*)

val map_l : ('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t
(** [map_l f [a1; ...; an]] applies the function [f] to [a1, ..., an] , and, in case of
    success for every element, returns the list of [Ok]-value.
    Otherwise, it fails and returns the first error encountered. Tail-recursive.*)

val fold_l : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a list -> ('b, 'err) t

val fold_iter : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a iter -> ('b, 'err) t
(** @since 3.0 *)

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
  (** Monadic [return]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic [bind]. *)
end

module Traverse(M : MONAD) : sig
  val sequence_m : ('a M.t, 'err) t -> ('a, 'err) t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> ('a, 'err) t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> ('a, 'err) t -> ('b, 'err) t M.t

  val retry_m : int -> (unit -> ('a, 'err) t M.t) -> ('a, 'err list) t M.t
end

(** {2 Conversions} *)

val to_opt : ('a, _) t -> 'a option
(** Convert a result to an option. *)

val of_opt : 'a option -> ('a, string) t
(** Convert an option to a result. *)

val to_iter : ('a, _) t -> 'a iter
(** @since 2.8 *)

val to_std_seq : ('a, _) t -> 'a Seq.t
(** @since 2.8 *)

type ('a, 'b) error = [`Ok of 'a | `Error of 'b]

val of_err : ('a, 'b) error -> ('a, 'b) t
(** @since 0.17 *)

val to_err : ('a, 'b) t -> ('a, 'b) error
(** @since 0.17 *)

(** {2 IO} *)

val pp : 'a printer -> ('a, string) t printer

val pp': 'a printer -> 'e printer -> ('a, 'e) t printer
(** Printer that is generic on the error type. *)
