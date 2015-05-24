
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

@since NEXT_RELEASE
*)

type 'a or_error = [`Ok of 'a | `Error of string]
exception ParseError of int * string (** position * message *)

(** {2 Input} *)

type input = {
  is_done : unit -> bool; (** End of input? *)
  cur : unit -> char;  (** Current char *)
  next : unit -> char; (** if not {!is_done}, move to next char *)
  pos : unit -> int;   (** Current pos *)
  backtrack : int -> unit;  (** Restore to previous pos *)
  sub : int -> int -> string; (** [sub pos len] extracts slice from [pos] with [len] *)
}

val input_of_string : string -> input

(** {2 Combinators} *)

type 'a t = input -> 'a (** @raise ParseError in case of failure *)

val return : 'a -> 'a t
val pure : 'a -> 'a t (** synonym to {!return} *)
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
val (<* ) : 'a t -> _ t -> 'a t
val ( *>) : _ t -> 'a t -> 'a t

val fail : string -> 'a t
val eoi : unit t (** end of string *)
val nop : unit t (** do nothing *)

val char : char -> char t
val char_if : (char -> bool) -> char t
val chars_if : (char -> bool) -> string t
val chars1_if : (char -> bool) -> string t (** non empty *)
val endline : char t
val space : char t (** tab or space *)
val white : char t (** tab or space or newline *)

val skip_chars : (char -> bool) -> unit t (** Skip 0 or more chars *)
val skip_space : unit t
val skip_white : unit t

val is_alpha : char -> bool
val is_num : char -> bool
val is_alpha_num : char -> bool
val is_space : char -> bool
val (~~~) : (char -> bool) -> char -> bool
val (|||) : (char -> bool) -> (char -> bool) -> char -> bool
val (&&&) : (char -> bool) -> (char -> bool) -> char -> bool

val (<|>) : 'a t -> 'a t -> 'a t  (* succeeds if either succeeds *)

val string : string -> string t

val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t (** non empty *)
val skip : _ t -> unit t

val sep : by:_ t -> 'a t -> 'a list t
val sep1 : by:_ t -> 'a t -> 'a list t (** non empty *)

val fix : ('a t -> 'a t) -> 'a t
(** Fixpoint combinator *)

(** {2 Parse} *)

val parse : input:input -> 'a t -> 'a or_error
val parse_exn : input:input -> 'a t -> 'a (** @raise ParseError if it fails *)

val parse_string : string -> 'a t -> 'a or_error
val parse_string_exn : string -> 'a t -> 'a (** @raise ParseError if it fails *)


(** {2 Utils} *)

module U : sig
  val list : ?start:string -> ?stop:string -> ?sep:string -> 'a t -> 'a list t
  val int : int t
  val word : string t (** alpha num, start with alpha *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
end
