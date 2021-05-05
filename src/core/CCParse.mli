
(* This file is free software. See file "license" for more details. *)

(** {1 Very Simple Parser Combinators}

    These combinators can be used to write very simple parsers, for example
    to extract data from a line-oriented file.

    {[
      open CCParse;;

      type tree = L of int | N of tree * tree;;

      let mk_leaf x = L x
      let mk_node x y = N(x,y)

      let ptree = fix @@ fun self ->
        skip_space *>
          ( (try_ (char '(') *> (pure mk_node <*> self <*> self) <* char ')')
            <|>
              (U.int >|= mk_leaf) )
      ;;

      parse_string_exn ptree "(1 (2 3))" ;;
      parse_string_exn ptree "((1 2) (3 (4 5)))" ;;

    ]}

    {4 Parse a list of words}

    {[
      open Containers.Parse;;
      let p = U.list ~sep:"," U.word;;
      parse_string_exn p "[abc , de, hello ,world  ]";;
    ]}

    {4 Stress Test}
    This makes a list of 100_000 integers, prints it and parses it back.

    {[
      let p = CCParse.(U.list ~sep:"," U.int);;

      let l = CCList.(1 -- 100_000);;
      let l_printed =
        CCFormat.(to_string (within "[" "]" (list ~sep:(return ",@,") int))) l;;

      let l' = CCParse.parse_string_exn p l_printed;;

      assert (l=l');;
    ]}

*)

(** {2 Errors}
    @since NEXT_RELEASE *)
module Error : sig
  type t
  (** A parse error.
      @since NEXT_RELEASE *)

  val line_and_column : t -> int * int
  (** Line and column numbers of the error position. *)

  val msg : t -> string

  val to_string : t -> string
  (** Prints the error *)

  val pp : Format.formatter -> t -> unit
  (** Pretty prints the error *)
end

type 'a or_error = ('a, Error.t) result
(* TODO: use [('a, error) result] instead, with easy conversion to [('a, string) result]  *)

exception ParseError of Error.t

(** {2 Input} *)

type position
(* TODO: make a module Position:  sig type t val line : t -> int val col : t -> int *)

(** {2 Combinators} *)

type 'a t
(** The abstract type of parsers that return a value of type ['a] (or fail).

    @raise ParseError in case of failure.
    @since NEXT_RELEASE the type is private.
*)

val return : 'a -> 'a t
(** Always succeeds, without consuming its input. *)

val pure : 'a -> 'a t
(** Synonym to {!return}. *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Map. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind.
    [p >>= f] results in a new parser which behaves as [p] then,
    in case of success, applies [f] to the result. *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative. *)

val (<* ) : 'a t -> _ t -> 'a t
(** [a <* b] parses [a] into [x], parses [b] and ignores its result,
    and returns [x]. *)

val ( *>) : _ t -> 'a t -> 'a t
(** [a *> b] parses [a], then parses [b] into [x], and returns [x]. The
    results of [a] is ignored. *)

val (|||) : 'a t -> 'b t -> ('a * 'b) t
(** [a ||| b] parses [a], then [b], then returns the pair of their results.
    @since NEXT_RELEASE *)

val fail : string -> 'a t
(** [fail msg] fails with the given message. It can trigger a backtrack. *)

val failf: ('a, unit, string, 'b t) format4 -> 'a
(** [Format.sprintf] version of {!fail}. *)

val parsing : string -> 'a t -> 'a t
(** [parsing s p] behaves the same as [p], with the information that
    we are parsing [s], if [p] fails. *)

val eoi : unit t
(** Expect the end of input, fails otherwise. *)

val nop : unit t
(** Succeed with [()]. *)

val any_char : char t
(** [any_char] parses any character.
    It still fails if the end of input was reached.
    @since NEXT_RELEASE *)

val any_chars : int -> string t
(** [any_chars len] parses exactly [len] characters from the input.
    @since NEXT_RELEASE *)

val char : char -> char t
(** [char c] parses the character [c] and nothing else. *)

val char_if : ?descr:string -> (char -> bool) -> char t
(** [char_if f] parses a character [c] if [f c = true].
    @param descr describes what kind of character was expected *)

val chars_if : (char -> bool) -> string t
(** [chars_if f] parses a string of chars that satisfy [f]. *)

val chars1_if : ?descr:string -> (char -> bool) -> string t
(** Like {!chars_if}, but only non-empty strings.
    @param descr describes what kind of character was expected *)

val endline : char t
(** Parse '\n'. *)

val space : char t
(** Tab or space. *)

val white : char t
(** Tab or space or newline. *)

val skip_chars : (char -> bool) -> unit t
(** Skip 0 or more chars satisfying the predicate. *)

val skip_space : unit t
(** Skip ' ' and '\t'. *)

val skip_white : unit t
(** Skip ' ' and '\t' and '\n'. *)

val is_alpha : char -> bool
(** Is the char a letter? *)

val is_num : char -> bool
(** Is the char a digit? *)

val is_alpha_num : char -> bool
(** Is the char a letter or a digit? *)

val is_space : char -> bool
(** True on ' ' and '\t'. *)

val is_white : char -> bool
(** True on ' ' and '\t' and '\n'. *)

val (<|>) : 'a t -> 'a t -> 'a t
(** [a <|> b] tries to parse [a], and if [a] fails without
    consuming any input, backtracks and tries
    to parse [b], otherwise it fails as [a].
    See {!try_} to ensure [a] does not consume anything (but it is best
    to avoid wrapping large parsers with {!try_}). *)

val (<?>) : 'a t -> string -> 'a t
(** [a <?> msg] behaves like [a], but if [a] fails,
    [a <? msg] fails with [msg] instead.
    Useful as the last choice in a series of [<|>]. For example:
    [a <|> b <|> c <?> "expected one of a, b, c"]. *)

val suspend : (unit -> 'a t) -> 'a t
(** [suspend f] is  the same as [f ()], but evaluates [f ()] only
    when needed. *)

val string : string -> string t
(** [string s] parses exactly the string [s], and nothing else. *)

val exact : string -> string t
(** Alias to {!string}.
    @since NEXT_RELEASE *)

val many : 'a t -> 'a list t
(** [many p] parses [p] repeatedly, until [p] fails, and
    collects the results into a list. *)

val optional : _ t -> unit t
(** [optional p] tries to parse [p], and return [()] whether it
    succeeded or failed. Cannot fail.
    @since NEXT_RELEASE *)

val try_ : 'a t -> 'a option t
(** [try_ p] tries to parse using [p], and return [Some x] if [p]
    succeeded with [x]. Otherwise it returns [None]. This cannot fail.
    @since NEXT_RELEASE *)

val many_until : until:_ t -> 'a t -> 'a list t
(** [many_until ~until p] parses as many [p] as it can until
    the [until] parser successfully returns.
    If [p] fails before that then [many_until ~until p] fails as well.
    Typically [until] can be a closing ')' or another termination condition.
    @since NEXT_RELEASE *)

val try_or : 'a t -> f:('a -> 'b t) -> else_:'b t -> 'b t
(** [try_or p1 ~f p2] attempts to parse [x] using [p1],
    and then becomes [f x].
    If [p1] fails, then it becomes [p2].
    @since NEXT_RELEASE
*)

val or_ : 'a t -> 'a t -> 'a t
(** [or_ p1 p2] tries to parse [p1], and if it fails, tries [p2]
    from the same position.
    @since NEXT_RELEASE *)

val many1 : 'a t -> 'a list t
(** [many1 p] is like [many p] excepts it fails if the
    list is empty (i.e. it needs [p] to succeed at least once). *)

val skip : _ t -> unit t
(** [skip p] parses zero or more times [p] and ignores its result. *)

val sep : by:_ t -> 'a t -> 'a list t
(** [sep ~by p] parses a list of [p] separated by [by]. *)

(* TODO: lookahead? *)

val sep_until: until:_ t -> by:_ t -> 'a t -> 'a list t
(** Same as {!sep} but stop when [until] parses successfully.
    @since NEXT_RELEASE *)

val sep1 : by:_ t -> 'a t -> 'a list t
(** [sep1 ~by p] parses a non empty list of [p], separated by [by]. *)

val line : string t
(** Parse a line, '\n' excluded.
    @since NEXT_RELEASE *)

val each_line : 'a t -> 'a list t
(** [each_line p] runs [p] on each line of the input.
    @since NEXT_RELEASE *)

val fix : ('a t -> 'a t) -> 'a t
(** Fixpoint combinator. *)

val memo : 'a t -> 'a t
(** Memoize the parser. [memo p] will behave like [p], but when called
    in a state (read: position in input) it has already processed, [memo p]
    returns a result directly. The implementation uses an underlying
    hashtable.
    This can be costly in memory, but improve the run time a lot if there
    is a lot of backtracking involving [p].

    This function is not thread-safe. *)

val fix_memo : ('a t -> 'a t) -> 'a t
(** Like {!fix}, but the fixpoint is memoized. *)

(** {2 Parse}

    Those functions have a label [~p] on the parser, since 0.14.
*)

val stringify_result : 'a or_error -> ('a, string) result
(** Turn a {!Error.t}-oriented result into a more basic string result.
    @since NEXT_RELEASE *)

val parse_string : 'a t -> string -> ('a, string) result
(** Parse a string using the parser. *)

val parse_string_e : 'a t -> string -> 'a or_error
(** Version of {!parse_string} that returns a more detailed error. *)

val parse_string_exn : 'a t -> string -> 'a
(**  @raise ParseError if it fails. *)

val parse_file : 'a t -> string -> ('a, string) result
(** [parse_file p filename] parses file named [filename] with [p]
    by opening the file and reading it whole. *)

val parse_file_e : 'a t -> string -> 'a or_error
(** Version of {!parse_file} that returns a more detailed error. *)

val parse_file_exn : 'a t -> string -> 'a
(** Same as {!parse_file}, but
    @raise ParseError if it fails. *)

(** {2 Infix} *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Map. [p >|= f] parses an item [x] using [p],
      and returns [f x]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind.
      [p >>= f] results in a new parser which behaves as [p] then,
      in case of success, applies [f] to the result. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative. *)

  val (<* ) : 'a t -> _ t -> 'a t
  (** [a <* b] parses [a] into [x], parses [b] and ignores its result,
      and returns [x]. *)

  val ( *>) : _ t -> 'a t -> 'a t
  (** [a *> b] parses [a], then parses [b] into [x], and returns [x]. The
      result of [a] is ignored. *)

  val (<|>) : 'a t -> 'a t -> 'a t
  (** [a <|> b] tries to parse [a], and if [a] fails, it backtracks and tries
      to parse [b].
      Alias to {!or_} *)

  val (|||) : 'a t -> 'b t -> ('a * 'b) t
  (** [a ||| b] parses [a], then [b], then returns the pair of their results.
      @since NEXT_RELEASE *)

  val (<?>) : 'a t -> string -> 'a t
  (** [a <?> msg] behaves like [a], but if [a] fails,
      it fails with [msg] instead. Useful as the last choice in a series of
      [<|>]: [a <|> b <|> c <?> "expected a|b|c"]. *)

end

(** {2 Utils}

    This is useful to parse OCaml-like values in a simple way.
    All the parsers are whitespace-insensitive (they skip whitespace). *)
module U : sig
  val list : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a list t
  (** [list p] parses a list of [p], with the OCaml conventions for
      start token "[", stop token "]" and separator ";".
      Whitespace between items are skipped. *)

  (* TODO: parse option? *)
  (* TODO: split on whitespace? *)

  val int : int t
  (** Parse an int in decimal representation. *)

  val hexa_int : int t
  (** Parse an int int hexadecimal format. Accepts an optional [0x] prefix,
      and ignores capitalization.
      @since NEXT_RELEASE *)

  val word : string t
  (** Non empty string of alpha num, start with alpha. *)

  (* TODO: boolean literal *)
  (* TODO: quoted string *)

  val pair : ?start:string -> ?stop:string -> ?sep:string ->
    'a t -> 'b t -> ('a * 'b) t
  (** Parse a pair using OCaml syntactic conventions.
      The default is "(a, b)". *)

  val triple : ?start:string -> ?stop:string -> ?sep:string ->
    'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Parse a triple using OCaml syntactic conventions.
      The default is "(a, b, c)". *)
end

(** Let operators on OCaml >= 4.08.0, nothing otherwise
    @since 2.8 *)
include CCShimsMkLet_.S with type 'a t_let := 'a t
