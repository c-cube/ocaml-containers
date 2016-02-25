(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 Applicative Parser Combinators}

    Example: basic S-expr parser

{[
  open Containers_string.App_parse;;

  type sexp = Atom of string | List of sexp list;;

  let mkatom a = Atom a;;
  let mklist l = List l;;

  let ident_char = alpha_num <+> any_of "|!;$#@%&-_/=*.:~+[]<>'" ;;
  let ident = many1 ident_char >|= str_of_l ;;
  let atom = (ident <+> quoted) >|= mkatom ;;

  let sexp = fix (fun sexp ->
    white >>
      (atom <+>
       ((char '(' >> many sexp << char ')') >|= mklist)
      )
  );;

  Str.parse_exn "(a (b c d) e)" sexp;;

]}

@deprecated CCParse is more expressive and stable

{b status: deprecated}
@since 0.10
*)

type ('a,'b) result = [`Error of 'b | `Ok of 'a]

type 'a t
(** Parser that yields an error or a value of type 'a *)

(** {6 Combinators} *)

val return : 'a -> 'a t
(** Parser that succeeds with the given value *)

val pure : 'a -> 'a t
(** Synonym to {!return} *)

val junk : unit t
(** Skip next char *)

val fail : string -> 'a t
(** [fail msg] fails with the given error message *)

val failf : ('a, unit, string, 'b t) format4 -> 'a

val app : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map the parsed value *)

val int : int t
(** Parse an integer *)

val float : float t
(** Parse a floating point number *)

val bool : bool t
(** Parse "true" or "false" *)

val char : char -> char t
(** [char c] parses [c] and [c] only *)

val any_of : string -> char t
(** Parse any of the chars present in the given string *)

val alpha_lower : char t

val alpha_upper : char t

val alpha : char t

val symbols : char t
(** Symbols, such as "!-=_"... *)

val num : char t

val alpha_num : char t

val word : string t
(** [word] parses any identifier not starting with an integer and
    not containing any whitespace nor delimiter
    TODO: specify *)

val quoted : string t
(** Quoted string, following OCaml conventions *)

val str_of_l : char list -> string
(** Helper to build strings from lists of chars *)

val spaces : unit t
(** Parse a sequence of ['\t'] and [' '] *)

val spaces1 : unit t
(** Same as {!spaces} but requires at least one space *)

val white : unit t
(** Parse a sequence of ['\t'], ['\n'] and [' '] *)

val white1 : unit t

val eof : unit t
(** Matches the end of input, fails otherwise *)

val many : ?sep:unit t -> 'a t -> 'a list t
(** 0 or more parsed elements of the given type.
    @param sep separator between elements of the list (for instance, {!space}) *)

val many1 : ?sep:unit t -> 'a t -> 'a list t
(** Same as {!many}, but needs at least one element *)

val skip : _ t -> unit t
(** Skip 0 or more instances of the given parser *)

val skip1 : _ t -> unit t

val opt : 'a t -> 'a option t
(** [opt x] tries to parse [x], and returns [None] otherwise *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f p] parses the same as [p], but fails if the returned value
    does not satisfy [f] *)


(* TODO: complement operator any_but (all but \, for instance) *)
(* TODO: a "if-then-else" combinator (assuming the test has a
   set of possible first chars) *)

val switch_c : ?default:'a t -> (char * 'a t) list -> 'a t
(** [switch_c l] matches the next char and uses the corresponding parser.
    Fails if the next char is not in the list, unless default is defined.
    @param default parser to use if no char matches
    @raise Invalid_argument if some char occurs several times in [l] *)

val switch_s : (string * 'a t) list -> 'a t
(** [switch_s l] attempts to match matches any of the strings in [l].
    If one of those strings matches, the corresponding parser
    is used from now on.
    @raise Invalid_argument if some string is a prefix of another string,
          or is empty, or if the list is empty *)

val choice : 'a t list -> 'a t
(** [choice l] chooses between the parsers, unambiguously
    @raise Invalid_argument if the list is empty, or if some parsers
    overlap, making the choice ambiguous *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] makes a fixpoint *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix version of {!map} *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Synonym to {!app} *)

  val (>>) : _ t -> 'a t -> 'a t
  (** [a >> b] parses [a], ignores its result, then parses [b] *)

  val (<<) : 'a t -> _ t -> 'a t
  (** [a << b] parses [a], then [b], and discards [b] to return [a] *)

  val (<+>) : 'a t -> 'a t -> 'a t
  (** [a <+> b] is [choice [a;b]], a binary choice *)

  val (<::>) : 'a t -> 'a list t -> 'a list t
  (** [a <::> b] is [app (fun x l -> x::l) a b] *)
end

include module type of Infix

(** {2 Signatures} *)

(** {6 Parsing} *)

type error = {
  line: int;
  col: int;
  msg: string;
}

val string_of_error : error -> string

exception Error of error

module type S = sig
  type source
  (** Source of characters *)

  val parse : source -> 'a t -> ('a, error) result
  (** Parse the given source using the parser, and returns the parsed value. *)

  val parse': source -> 'a t -> ('a, string) result
  (** Same as {!parse}, but returns a user-friendly string in case of failure *)

  val parse_exn : source -> 'a t -> 'a
  (** Unsafe version of {!parse}.
      @raise Error if parsing fails *)
end

(** {2 Parse} *)

module type INPUT = sig
  type t

  val read : t -> Bytes.t -> int -> int -> int
end

module Make(I : INPUT) : S with type source = I.t

(** {2 Low-level interface} *)

val print : Format.formatter -> _ t -> unit
(** Print a parser structure, for debug purpose *)

type token =
  | Yield of char
  | EOF

module type READER = sig
  type t
  type source (* underlying source *)

  val create : source -> t
  val peek : t -> token  (* peek; do not consume *)
  val next : t -> token  (* read and consume *)
  val junk : t -> unit   (* consume last token, obtained with junk *)
  val line : t -> int
  val col : t -> int
end

module MakeFromReader(R : READER) : S with type source = R.source

(** {2 Defaults} *)

module Str : S with type source = string

module Chan : S with type source = in_channel
