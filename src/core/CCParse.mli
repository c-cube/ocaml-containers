
(* This file is free software. See file "license" for more details. *)

(** {1 Very Simple Parser Combinators}

    These combinators can be used to write very simple parsers, for example
    to extract data from a line-oriented file, or as a replacement to {!Scanf}.

    {2 A few examples}

    Some more advanced example(s) can be found in the [/examples] directory.

    {4 Parse a tree}

    {[
      open CCParse;;

      type tree = L of int | N of tree * tree;;

      let mk_leaf x = L x
      let mk_node x y = N(x,y)

      let ptree = fix @@ fun self ->
        skip_space *>
          ( (char '(' *> (pure mk_node <*> self <*> self) <* char ')')
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

    {2 Stability guarantees}

    Some functions are marked "experimental" and are still subject to change.

*)

type position
(** A position in the input. Typically it'll point at the {b beginning} of
    an error location. *)

(** {2 Positions in input}

    @since 3.6 *)
module Position : sig
  type t = position

  val line : t -> int
  (** Line number *)

  val column : t -> int
  (** Column number *)

  val line_and_column : t -> int * int
  (** Line and column number *)

  val pp : Format.formatter -> t -> unit
  (** Unspecified pretty-printed version of the position. *)
end

(** {2 Errors}
    @since 3.6 *)
module Error : sig
  type t
  (** A parse error.
      @since 3.6 *)

  val position : t -> position
  (** Returns position of the error *)

  val line_and_column : t -> int * int
  (** Line and column numbers of the error position. *)

  val msg : t -> string

  val to_string : t -> string
  (** Prints the error *)

  val pp : Format.formatter -> t -> unit
  (** Pretty prints the error *)
end

type +'a or_error = ('a, Error.t) result
(** ['a or_error] is either [Ok x] for some result [x : 'a],
    or an error {!Error.t}.

    See {!stringify_result} and {!Error.to_string} to print the
    error message. *)

exception ParseError of Error.t

(** {2 Input} *)

(** {2 Combinators} *)

type 'a t
(** The abstract type of parsers that return a value of type ['a] (or fail).

    @raise ParseError in case of failure.
    @since 3.6 the type is private.
*)

val return : 'a -> 'a t
(** Always succeeds, without consuming its input. *)

val pure : 'a -> 'a t
(** Synonym to {!return}. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val bind : ('a -> 'b t) -> 'a t -> 'b t
(** [bind f p] results in a new parser which behaves as [p] then,
    in case of success, applies [f] to the result.
    @since 3.6
*)

val ap : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative.
    @since 3.6 *)

val eoi : unit t
(** Expect the end of input, fails otherwise. *)

val empty : unit t
(** Succeed with [()].
    @since 3.6 *)

val fail : string -> 'a t
(** [fail msg] fails with the given message. It can trigger a backtrack. *)

val failf: ('a, unit, string, 'b t) format4 -> 'a
(** [Format.sprintf] version of {!fail}. *)

val fail_lazy : (unit -> string) -> 'a t
(** Like {!fail}, but only produce an error message on demand.
    @since 3.6 *)

val parsing : string -> 'a t -> 'a t
(** [parsing s p] behaves the same as [p], with the information that
    we are parsing [s], if [p] fails.
    The message [s] is added to the error, it does not replace it,
    not does the location change (the error still points to
    the same location as in [p]). *)

val set_error_message : string -> 'a t -> 'a t
(** [set_error_message msg p] behaves like [p], but if [p] fails,
    [set_error_message msg p] fails with [msg] instead and at the current
    position. The internal error message of [p] is just discarded.
    @since 3.6 *)

val with_pos : 'a t -> ('a * position) t
(** [with_pos p] behaves like [p], but returns the (starting) position
    along with [p]'s result.

    {b EXPERIMENTAL}
    @since 3.6 *)

val any_char : char t
(** [any_char] parses any character.
    It still fails if the end of input was reached.
    @since 3.6 *)

val any_char_n : int -> string t
(** [any_char_n len] parses exactly [len] characters from the input.
    Fails if the input doesn't contain at least [len] chars.
    @since 3.6 *)

val char : char -> char t
(** [char c] parses the character [c] and nothing else. *)

type slice
(** A slice of the input, as returned by some combinators such
    as {!split_1} or {!split_list} or {!take}.

    The idea is that one can use some parsers to cut the input into slices,
    e.g. split into lines, or split a line into fields (think CSV or TSV).
    Then a variety of parsers can be used on each slice to extract data from
    it using {!recurse}.

    Slices contain enough information to make it possible
    for [recurse slice p] to report failures (if [p] fails) using locations
    from the original input, not relative to the slice.
    Therefore, even after splitting the input into lines using, say, {!each_line},
    a failure to parse the 500th line will be reported at line 500 and
    not at line 1.

    {b EXPERIMENTAL}
    @since 3.6 *)

(** Functions on slices.
    @since 3.6 *)
module Slice : sig
  type t = slice

  val is_empty : t -> bool
  (** Is the slice empty? *)

  val length : t -> int
  (** Length of the slice *)

  val to_string : t -> string
  (** Convert the slice into a string.
      Linear time and memory in [length slice] *)
end

val recurse : slice -> 'a t -> 'a t
(** [recurse slice p] parses the [slice]
    (most likely obtained via another combinator, such as {!split_1}
    or {!split_n}), using [p].

    The slice contains a position which is used to relocate error
    messages to their position in the whole input, not just relative to
    the slice.

    {b EXPERIMENTAL}
    @since 3.6 *)

val set_current_slice : slice -> unit t
(** [set_current_slice slice] replaces the parser's state with [slice].

    {b EXPERIMENTAL}
    @since 3.6 *)

val chars_fold :
  f:('acc -> char ->
     [`Continue of 'acc | `Consume_and_stop of 'acc | `Stop of 'acc | `Fail of string]) ->
  'acc ->
  ('acc * slice) t
(** [chars_fold f acc0] folds over characters of the input.
    Each char [c] is passed, along with the current accumulator, to [f];
    [f] can either:

    - stop, by returning [`Stop acc]. In this case the final accumulator [acc]
      is returned, and [c] is not consumed.
    - consume char and stop, by returning [`Consume_and_stop acc].
    - fail, by returning [`Fail msg]. In this case the parser fails
      with the given message.
    - continue, by returning [`Continue acc]. The parser continues to the
      next char with the new accumulator.

    This is a generalization of of {!chars_if} that allows one to transform
    characters on the fly, skip some, handle escape sequences, etc.
    It can also be useful as a base component for a lexer.

   @return a pair of the final accumular, and the slice matched by the fold.
   @since 3.6 *)

val chars_fold_transduce :
  f:('acc -> char ->
     [ `Continue of 'acc | `Yield of 'acc * char
     | `Consume_and_stop | `Stop | `Fail of string]) ->
  'acc ->
  ('acc * string) t
(** Same as {!char_fold} but with the following differences:

    - returns a string along with the accumulator, rather than the slice
      of all the characters accepted by [`Continue _].
      The string is built from characters returned by [`Yield].
    - new case [`Yield (acc, c)] adds [c] to the returned string
      and continues parsing with [acc].

    @since 3.6 *)

val take : int -> slice t
(** [take len] parses exactly [len] characters from the input.
    Fails if the input doesn't contain at least [len] chars.
    @since 3.6 *)

val take_if : (char -> bool) -> slice t
(** [take_if f] takes characters as long as they satisfy the predicate [f].
    @since 3.6 *)

val take1_if : ?descr:string -> (char -> bool) -> slice t
(** [take1_if f] takes characters as long as they satisfy the predicate [f].
    Fails if no character satisfies [f].
    @param descr describes what kind of character was expected, in case of error
    @since 3.6 *)

val char_if : ?descr:string -> (char -> bool) -> char t
(** [char_if f] parses a character [c] if [f c = true].
    Fails if  the next char does not satisfy [f].
    @param descr describes what kind of character was expected, in case of error *)

val chars_if : (char -> bool) -> string t
(** [chars_if f] parses a string of chars that satisfy [f].
    Cannot fail. *)

val chars1_if : ?descr:string -> (char -> bool) -> string t
(** Like {!chars_if}, but accepts only non-empty strings.
    [chars1_if p] fails if the string accepted by [chars_if p] is empty.
    [chars1_if p] is equivalent to [take1_if p >|= Slice.to_string].
    @param descr describes what kind of character was expected, in case of error *)

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

val suspend : (unit -> 'a t) -> 'a t
(** [suspend f] is  the same as [f ()], but evaluates [f ()] only
    when needed. *)

val string : string -> string t
(** [string s] parses exactly the string [s], and nothing else. *)

val exact : string -> string t
(** Alias to {!string}.
    @since 3.6 *)

val many : 'a t -> 'a list t
(** [many p] parses [p] repeatedly, until [p] fails, and
    collects the results into a list. *)

val optional : _ t -> unit t
(** [optional p] tries to parse [p], and return [()] whether it
    succeeded or failed. Cannot fail itself.
    It consumes input if [p] succeeded (as much as [p] consumed), but
    consumes not input if [p] failed.
    @since 3.6 *)

val try_ : 'a t -> 'a t
[@@deprecated "plays no role anymore, just replace [try foo] with [foo]"]
(** [try_ p] is just like [p] (it used to play a role in backtracking
    semantics but no more).

    @deprecated since 3.6 it can just be removed. See {!try_opt} if you want
    to detect failure. *)

val try_opt : 'a t -> 'a option t
(** [try_opt p] tries to parse using [p], and return [Some x] if [p]
    succeeded with [x] (and consumes what [p] consumed).
    Otherwise it returns [None] and consumes nothing. This cannot fail.
    @since 3.6 *)

val many_until : until:_ t -> 'a t -> 'a list t
(** [many_until ~until p] parses as many [p] as it can until
    the [until] parser successfully returns.
    If [p] fails before that then [many_until ~until p] fails as well.
    Typically [until] can be a closing ')' or another termination condition,
    and what is consumed by [until] is also consumed by [many_until ~until p].

    {b EXPERIMENTAL}

    @since 3.6 *)

val try_or : 'a t -> f:('a -> 'b t) -> else_:'b t -> 'b t
(** [try_or p1 ~f ~else_:p2] attempts to parse [x] using [p1],
    and then becomes [f x].
    If [p1] fails, then it becomes [p2]. This can be useful if [f] is expensive
    but only ever works if [p1] matches (e.g. after an opening parenthesis
    or some sort of prefix).
    @since 3.6
*)

val try_or_l :
  ?msg:string ->
  ?else_:'a t ->
  (unit t * 'a t) list ->
  'a t
(** [try_or_l ?else_ l] tries each pair [(test, p)] in order.
    If the n-th [test] succeeds, then [try_or_l l] behaves like n-th [p],
    whether [p] fails or not.
    If they all fail, and [else_] is defined, then it behaves like [else_].
    If all fail, and [else_] is [None], then it fails as well.

    This is a performance optimization compared to {!(<|>)}. We commit to a
    branch if the test succeeds, without backtracking at all.

    See {!lookahead_ignore} for a convenient way of writing the test conditions.

    @param msg error message if all options fail

    {b EXPERIMENTAL}
    @since 3.6 *)

val or_ : 'a t -> 'a t -> 'a t
(** [or_ p1 p2] tries to parse [p1], and if it fails, tries [p2]
    from the same position.
    @since 3.6 *)

val both : 'a t -> 'b t -> ('a * 'b) t
(** [both a b] parses [a], then [b], then returns the pair of their results.
    @since 3.6 *)

val many1 : 'a t -> 'a list t
(** [many1 p] is like [many p] excepts it fails if the
    list is empty (i.e. it needs [p] to succeed at least once). *)

val skip : _ t -> unit t
(** [skip p] parses zero or more times [p] and ignores its result.
    It is eager, meaning it will continue as long as [p] succeeds.
    As soon as [p] fails, [skip p] stops consuming any input. *)

val sep : by:_ t -> 'a t -> 'a list t
(** [sep ~by p] parses a list of [p] separated by [by]. *)

val sep_until: until:_ t -> by:_ t -> 'a t -> 'a list t
(** Same as {!sep} but stop when [until] parses successfully.
    @since 3.6 *)

val sep1 : by:_ t -> 'a t -> 'a list t
(** [sep1 ~by p] parses a non empty list of [p], separated by [by]. *)

val lookahead : 'a t -> 'a t
(** [lookahead p] behaves like [p], except it doesn't consume any input.

    {b EXPERIMENTAL}
    @since 3.6 *)

val lookahead_ignore : 'a t -> unit t
(** [lookahead_ignore p] tries to parse input with [p],
    and succeeds if [p] succeeds. However it doesn't consume any input
    and returns [()], so in effect its only use-case is to detect
    whether [p] succeeds, e.g. in {!try_or_l}.

    {b EXPERIMENTAL}
    @since 3.6 *)

val fix : ('a t -> 'a t) -> 'a t
(** Fixpoint combinator. *)

val line : slice t
(** Parse a line, ['\n'] excluded, and position the cursor after the ['\n'].
    @since 3.6 *)

val line_str : string t
(** [line_str] is [line >|= Slice.to_string].
    It parses the next line and turns the slice into a string.
    The state points to the character immediately after the ['\n'] character.
    @since 3.6 *)

val each_line : 'a t -> 'a list t
(** [each_line p] runs [p] on each line of the input.
    {b EXPERIMENTAL}
    @since 3.6 *)

val split_1 : on_char:char -> (slice * slice option) t
(** [split_1 ~on_char] looks for [on_char] in the input, and returns a
    pair [sl1, sl2], where:

    - [sl1] is the slice of the input the precedes the first occurrence
      of [on_char], or the whole input if [on_char] cannot be found.
      It does not contain [on_char].
    - [sl2] is the slice that comes after [on_char],
      or [None] if [on_char] couldn't be found. It doesn't contain the first
      occurrence of [on_char] (if any).

    The parser is now positioned at the end of the input.

    {b EXPERIMENTAL}
    @since 3.6 *)

val split_list : on_char:char -> slice list t
(** [split_list ~on_char] splits the input on all occurrences of [on_char],
    returning a list of slices.

    {b EXPERIMENTAL}
    @since 3.6 *)

val split_list_at_most : on_char:char -> int -> slice list t
(** [split_list_at_most ~on_char n] applies [split_1 ~on_char] at most
    [n] times, to get a list of [n+1] elements.
    The last element might contain [on_char]. This is useful to limit the
    amount of work done by {!split_list}.

    {b EXPERIMENTAL}
    @since 3.6 *)


val split_2 : on_char:char -> (slice * slice) t
(** [split_2 ~on_char] splits the input into exactly 2 fields,
    and fails if the split yields less or more than 2 items.
    {b EXPERIMENTAL}
    @since 3.6 *)

val split_3 : on_char:char -> (slice * slice * slice) t
(** See {!split_2}
    {b EXPERIMENTAL}
    @since 3.6 *)

val split_4 : on_char:char -> (slice * slice * slice * slice) t
(** See {!split_2}
    {b EXPERIMENTAL}
    @since 3.6 *)

val each_split : on_char:char -> 'a t -> 'a list t
(** [split_list_map ~on_char p] uses [split_list ~on_char] to split
    the input, then parses each chunk of the input thus obtained using [p].

    The difference with [sep ~by:(char on_char) p] is that
    [sep] calls [p] first, and only tries to find [on_char] after [p] returns.
    While it is more flexible, this technique also means [p] has to be careful
    not to consume [on_char] by error.

    A useful specialization of this is {!each_line}, which is
    basically [each_split ~on_char:'\n' p].

    {b EXPERIMENTAL}
    @since 3.6 *)

val all : slice t
(** [all] returns all the unconsumed input as a slice, and consumes it.
    Use {!Slice.to_string} to turn it into a string.

    Note that [lookahead all] can be used to {i peek} at the rest of the input
    without consuming anything.

    @since 3.6 *)

val all_str : string t
(** [all_str] accepts all the remaining chars and extracts them into a
    string. Similar to {!all} but with a string.

    {b EXPERIMENTAL}
    @since 3.6 *)

(* TODO
val trim : slice t
(** [trim] is like {!all}, but removes whitespace on the left and right.
   {b EXPERIMENTAL}
    @since 3.6 *)
 *)

val memo : 'a t -> 'a t
(** Memoize the parser. [memo p] will behave like [p], but when called
    in a state (read: position in input) it has already processed, [memo p]
    returns a result directly. The implementation uses an underlying
    hashtable.
    This can be costly in memory, but improve the run time a lot if there
    is a lot of backtracking involving [p].

    Do not call {!memo} inside other functions, especially with {!(>>=)},
    {!map}, etc. being so prevalent. Instead the correct way to use it
    is in a toplevel definition:

    {[
      let my_expensive_parser = memo (foo *> bar >>= fun i -> …)
    ]}

    This function is not thread-safe. *)

val fix_memo : ('a t -> 'a t) -> 'a t
(** Like {!fix}, but the fixpoint is memoized. *)

(** {2 Infix} *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Alias to {!map}. [p >|= f] parses an item [x] using [p],
      and returns [f x]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Alias to {!bind}.
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
  (** Alias to {!or_}.

      [a <|> b] tries to parse [a], and if [a] fails without
      consuming any input, backtracks and tries
      to parse [b], otherwise it fails as [a].
      See {!try_} to ensure [a] does not consume anything (but it is best
      to avoid wrapping large parsers with {!try_}). *)

  val (<?>) : 'a t -> string -> 'a t
  (** [a <?> msg] behaves like [a], but if [a] fails,
      [a <?> msg] fails with [msg] instead.
      Useful as the last choice in a series of [<|>]. For example:
      [a <|> b <|> c <?> "expected one of a, b, c"]. *)

  val (|||) : 'a t -> 'b t -> ('a * 'b) t
  (** Alias to {!both}.
      [a ||| b] parses [a], then [b], then returns the pair of their results.
      @since 3.6 *)

  (** Let operators on OCaml >= 4.08.0, nothing otherwise
      @since 2.8 *)
  include CCShimsMkLet_.S with type 'a t_let := 'a t
end

include module type of Infix

(** {2 Parse input} *)

val stringify_result : 'a or_error -> ('a, string) result
(** Turn a {!Error.t}-oriented result into a more basic string result.
    @since 3.6 *)

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


(** {2 Utils}

    This is useful to parse OCaml-like values in a simple way.
    All the parsers are whitespace-insensitive (they skip whitespace). *)
module U : sig
  val list : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a list t
  (** [list p] parses a list of [p], with the OCaml conventions for
      start token "\[", stop token "\]" and separator ";".
      Whitespace between items are skipped. *)

  (* TODO: parse option? *)
  (* TODO: split on whitespace? *)

  val int : int t
  (** Parse an int in decimal representation. *)

  val in_paren : 'a t -> 'a t
  (** [in_paren p] parses an opening "(",[p] , and then ")".
      @since 3.6 *)

  val in_parens_opt : 'a t -> 'a t
  (** [in_parens_opt p] parses [p] in an arbitrary number of nested
      parenthesis (possibly 0).
      @since 3.6 *)

  val option : 'a t -> 'a option t
  (** [option p] parses "Some <x>" into [Some x]  if [p] parses "<x>" into [x],
      and parses "None" into [None].
      @since 3.6 *)

  val hexa_int : int t
  (** Parse an int int hexadecimal format. Accepts an optional [0x] prefix,
      and ignores capitalization.
      @since 3.6 *)

  val word : string t
  (** Non empty string of alpha num, start with alpha. *)

  val bool : bool t
  (** Accepts "true" or "false"
      @since 3.6 *)

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

(** Debugging utils.
    {b EXPERIMENTAL}
    @since 3.6 *)
module Debug_ : sig
  val trace_fail : string -> 'a t -> 'a t
  (** [trace_fail name p] behaves like [p], but prints the error message of [p]
      on stderr whenever [p] fails.
      @param name used as a prefix of all trace messages. *)

  val trace_success : string -> print:('a -> string) -> 'a t -> 'a t
  (** [trace_success name ~print p] behaves like [p], but
      prints successful runs of [p] using [print]. *)

  val trace_success_or_fail : string -> print:('a -> string) -> 'a t -> 'a t
      (** Trace both error or success *)
end
