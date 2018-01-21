
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Handling S-expressions} *)

type 'a or_error = ('a, string) Result.result
type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(** {2 Basics} *)

type t = [
  | `Atom of string
  | `List of t list
]
type sexp = t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val atom : string -> t  
(** Build an atom directly from a string. *)

val of_int : int -> t
val of_bool : bool -> t
val of_list : t list -> t
val of_rev_list : t list -> t  
(** Reverse the list. *)
val of_float : float -> t
val of_unit : t
val of_pair : t * t -> t
val of_triple : t * t * t -> t
val of_quad : t * t * t * t -> t

val of_variant : string -> t list -> t
(** [of_variant name args] is used to encode algebraic variants
    into a S-expr. For instance [of_variant "some" [of_int 1]]
    represents the value [Some 1]. *)

val of_field : string -> t -> t
(** Used to represent one record field. *)

val of_record : (string * t) list -> t
(** Represent a record by its named fields. *)

(** {2 Printing} *)

val to_buf : Buffer.t -> t -> unit

val to_string : t -> string

val to_file : string -> t -> unit

val to_file_seq : string -> t sequence -> unit
(** Print the given sequence of expressions to a file. *)

val to_chan : out_channel -> t -> unit

val pp : Format.formatter -> t -> unit
(** Pretty-printer nice on human eyes (including indentation). *)

val pp_noindent : Format.formatter -> t -> unit
(** Raw, direct printing as compact as possible. *)

(** {2 Parsing} *)

(** A parser of ['a] can return [Yield x] when it parsed a value,
    or [Fail e] when a parse error was encountered, or
    [End] if the input was empty. *)
type 'a parse_result =
  | Yield of 'a
  | Fail of string
  | End

module Decoder : sig
  type t
  (** Decoder *)

  val of_lexbuf : Lexing.lexbuf -> t

  val next : t -> sexp parse_result
  (** Parse the next S-expression or return an error if the input isn't
      long enough or isn't a proper S-expression. *)
end

val parse_string : string -> t or_error
(** Parse a string. *)

val parse_chan : in_channel -> t or_error
(** Parse a S-expression from the given channel. Can read more data than
    necessary, so don't use this if you need finer-grained control (e.g.
    to read something else {b after} the S-exp). *)

val parse_chan_gen : in_channel -> t or_error gen
(** Parse a channel into a generator of S-expressions. *)

val parse_chan_list : in_channel -> t list or_error

val parse_file : string -> t or_error
(** Open the file and read a S-exp from it. *)

val parse_file_list : string -> t list or_error
(** Open the file and read a S-exp from it. *)
