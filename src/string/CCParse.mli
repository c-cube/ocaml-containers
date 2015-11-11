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

(**
{1 Very Simple Parser Combinators}

{b status} still a bit unstable, the type {!'a t} might still change.

Examples:

{6 parse recursive structures}

{[
open Containers_string.Parse;;

type tree = L of int | N of tree * tree;;

let mk_leaf x = L x
let mk_node x y = N(x,y)

let ptree = fix @@ fun self ->
  skip_space *>
  ( (char '(' *> (pure mk_node <*> self <*> self) <* char ')')
    <|>
    (U.int >|= mk_leaf) )
;;

parse_string_exn "(1 (2 3))" ptree;;
parse_string_exn "((1 2) (3 (4 5)))" ptree;;

]}

{6 Parse a list of words}

{[
open Containers_string.Parse;;
let p = U.list ~sep:"," U.word;;
parse_string_exn "[abc , de, hello ,world  ]" p;;
]}

{6 Stress Test}
This makes a list of 100_000 integers, prints it and parses it back.

{[
let p = CCParse.(U.list ~sep:"," U.int);;

let l = CCList.(1 -- 100_000);;
let l_printed =
  CCFormat.to_string (CCList.print ~sep:"," ~start:"[" ~stop:"]" CCInt.print) l;;

let l' = CCParse.parse_string_exn ~p l_printed;;

assert (l=l');;
]}

@since 0.11
*)

type 'a or_error = [`Ok of 'a | `Error of string]

type line_num = int (** @since 0.13 *)
type col_num = int (** @since 0.13 *)

exception ParseError of line_num * col_num * (unit -> string)
(** position * message

 This type changed at 0.13 *)

(** {2 Input} *)

(** @since 0.13 *)
module MemoTbl : sig
  type t
  val create: int -> t (** New memoization table *)
end

type input = {
  is_done : unit -> bool; (** End of input? *)
  cur : unit -> char;  (** Current char *)
  next : unit -> char;
    (** Returns current char;
        if not {!is_done}, move to next char,
        otherwise throw ParseError *)

  pos : unit -> int;   (** Current pos *)
  lnum : unit -> line_num; (** Line number @since 0.13 *)
  cnum : unit -> col_num;  (** Column number @since 0.13 *)
  memo : MemoTbl.t; (** Memoization table, if any *)
  backtrack : int -> unit;  (** Restore to previous pos *)
  sub : int -> int -> string; (** [sub pos len] extracts slice from [pos] with [len] *)
}
(** The type of input, which must allow for backtracking somehow.
    This type is {b unstable} and its details might change. *)

val input_of_string : string -> input
(** Parse the string *)

val input_of_chan : ?size:int -> in_channel -> input
(** [input_of_chan ic] reads lazily the content of [ic] as parsing goes.
    All content that is read is saved to an internal buffer for backtracking.
    @param size number of bytes read at once from [ic]
    @since 0.13 *)

(** {2 Combinators} *)

type 'a t = input -> ok:('a -> unit) -> err:(exn -> unit) -> unit
(** Takes the input and two continuations:
    {ul
      {- [ok] to call with the result when it's done}
      {- [err] to call when the parser met an error}
    }
    The type definition changed since 0.14 to avoid stack overflows
    @raise ParseError in case of failure *)

val return : 'a -> 'a t
(** Always succeeds, without consuming its input *)

val pure : 'a -> 'a t
(** Synonym to {!return} *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Map *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative *)

val (<* ) : 'a t -> _ t -> 'a t
(** [a <* b] parses [a] into [x], parses [b] and ignores its result,
    and returns [x] *)

val ( *>) : _ t -> 'a t -> 'a t
(** [a *> b] parses [a], then parses [b] into [x], and returns [x]. The
    results of [a] is ignored. *)

val fail : string -> 'a t
(** [fail msg] fails with the given message. It can trigger a backtrack *)

val eoi : unit t
(** Expect the end of input, fails otherwise *)

val nop : unit t
(** Succeed with [()] *)

val char : char -> char t
(** [char c] parses the char [c] and nothing else *)

val char_if : (char -> bool) -> char t
(** [char_if f] parses a character [c] if [f c = true] *)

val chars_if : (char -> bool) -> string t
(** [chars_if f] parses a string of chars that satisfy [f] *)

val chars1_if : (char -> bool) -> string t
(** Same as {!chars_if}, but only non-empty strings *)

val endline : char t
(** Parses '\n' *)

val space : char t
(** Tab or space *)

val white : char t
(** Tab or space or newline *)

val skip_chars : (char -> bool) -> unit t
(** Skip 0 or more chars satisfying the predicate *)

val skip_space : unit t
(** Skip ' ' and '\t' *)

val skip_white : unit t
(** Skip ' ' and '\t' and '\n' *)

val is_alpha : char -> bool
(** Is the char a letter? *)

val is_num : char -> bool
(** Is the char a digit? *)

val is_alpha_num : char -> bool

val is_space : char -> bool
(** True on ' ' and '\t' *)

val is_white : char -> bool
(** True on ' ' and '\t' and '\n'
    @since 0.13 *)

val (~~~) : (char -> bool) -> char -> bool
(** Negation on predicates *)

val (|||) : (char -> bool) -> (char -> bool) -> char -> bool
(** Disjunction on predicates *)

val (&&&) : (char -> bool) -> (char -> bool) -> char -> bool
(** Conjunction on predicates *)

val (<|>) : 'a t -> 'a t -> 'a t
(** [a <|> b] tries to parse [a], and if [a] fails, backtracks and tries
    to parse [b]. Therefore, it succeeds if either succeeds *)

val string : string -> string t
(** [string s] parses exactly the string [s], and nothing else *)

val many : 'a t -> 'a list t
(** [many p] parses a list of [p], eagerly (as long as possible) *)

val many1 : 'a t -> 'a list t
(** parses a non empty list *)

val skip : _ t -> unit t
(** [skip p] parses [p] and ignores its result *)

val sep : by:_ t -> 'a t -> 'a list t
(** [sep ~by p] parses a list of [p] separated by [by] *)

val sep1 : by:_ t -> 'a t -> 'a list t
(** [sep1 ~by p] parses a non empty list of [p], separated by [by] *)

val fix : ('a t -> 'a t) -> 'a t
(** Fixpoint combinator *)

val memo : 'a t -> 'a t
(** Memoize the parser. [memo p] will behave like [p], but when called
    in a state (read: position in input) it has already processed, [memo p]
    returns a result directly. The implementation uses an underlying
    hashtable.
    This can be costly in memory, but improve the run time a lot if there
    is a lot of backtracking involving [p].

    This function is not thread-safe.
    @since 0.13 *)

val fix_memo : ('a t -> 'a t) -> 'a t
(** Same as {!fix}, but the fixpoint is memoized.
    @since 0.13 *)

(** {2 Parse}

  Those functions have a label [~p] on the parser, since 0.14.
*)

val parse : input:input -> p:'a t -> 'a or_error
(** [parse ~input p] applies [p] on the input, and returns [`Ok x] if
    [p] succeeds with [x], or [`Error s] otherwise *)

val parse_exn : input:input -> p:'a t -> 'a
(** @raise ParseError if it fails *)

val parse_string : string -> p:'a t -> 'a or_error
(** Specialization of {!parse} for string inputs *)

val parse_string_exn : string -> p:'a t -> 'a
(** @raise ParseError if it fails *)

val parse_file : ?size:int -> file:string -> p:'a t -> 'a or_error
(** [parse_file ~file p] parses [file] with [p] by opening the file
    and using {!input_of_chan}.
    @param size size of chunks read from file
    @since 0.13 *)

val parse_file_exn : ?size:int -> file:string -> p:'a t -> 'a
(** Unsafe version of {!parse_file}
    @since 0.13 *)

(** {2 Utils} *)

module U : sig
  val list : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a list t
  (** [list p] parses a list of [p], with the OCaml conventions for
      start token "[", stop token "]" and separator ";".
      Whitespace between items are skipped *)

  val int : int t

  val word : string t
  (** non empty string of alpha num, start with alpha *)

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val pair : ?start:string -> ?stop:string -> ?sep:string ->
             'a t -> 'b t -> ('a * 'b) t
  (** Parse a pair using OCaml whitespace conventions.
      The default is "(a, b)".
      @since 0.14 *)

  val triple : ?start:string -> ?stop:string -> ?sep:string ->
               'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Parse a triple using OCaml whitespace conventions.
      The default is "(a, b, c)".
      @since 0.14 *)
end
