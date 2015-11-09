(*
copyright (c) 2013, simon cruanes
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


(** {1 Levenshtein distance}

We take inspiration from
http://blog.notdot.net/2010/07/Damn-Cool-Algorithms-Levenshtein-Automata
for the main algorithm and ideas. However some parts are adapted *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(** {2 Abstraction over Strings}
Due to the existence of several encodings and string representations we
abstract over the type of strings. A string is a finite array of characters
(8-bits char, unicode runes, etc.) which provides a length operation
and a function to access the n-th character. *)

module type STRING = sig
  type char_
  type t

  val of_list : char_ list -> t
  val get : t -> int -> char_
  val length : t -> int
  val compare_char : char_ -> char_ -> int
end

(** {2 Continuation list}

This data structure is used to represent a list of result that is
evaluated only as far as the user wants. If the user only wants a few elements,
she doesn't pay for the remaining ones.

In particular, when matching a string against a (big) set of indexed
strings, we return a continuation list so that, even if there are many results,
only those actually asked for are evaluated. *)

type 'a klist =
  unit -> [
  | `Nil
  | `Cons of 'a * 'a klist
  ]

val klist_to_list : 'a klist -> 'a list
  (** Helper for short lists. *)

(** {2 Signature}

The signature for a given string representation provides 3 main things:

- a [edit_distance] function to compute the edit distance between strings
- an [automaton] type that is built from a string [s] and a maximum distance [n],
  and only accepts the strings [s'] such that [edit_distance s s' <= n].
- an [Index] module that can be used to map many strings to values, like
  a regular string map, but for which retrieval is fuzzy (for a given
  maximal distance).

A possible use of the index could be:
{[

let words = CCIO.with_in "/usr/share/dict/words"
  (fun i -> CCIO.read_all i |> CCString.Split.list_cpy ~by:"\n");;

let words = List.map (fun s->s,s) words;;
let idx = CCLevenshtein.Index.of_list words;;

CCLevenshtein.Index.retrieve ~limit:1 idx "hell" |> CCLevenshtein.klist_to_list;;
]}
*)

module type S = sig
  type char_
  type string_

  (** {6 Edit Distance} *)

  val edit_distance : string_ -> string_ -> int
  (** Edition distance between two strings. This satisfies the classical
     distance axioms: it is always positive, symmetric, and satisfies
     the formula [distance a b + distance b c >= distance a c] *)

  (** {6 Automaton}
  An automaton, built from a string [s] and a limit [n], that accepts
  every string that is at distance at most [n] from [s]. *)

  type automaton
  (** Levenshtein automaton *)

  val of_string : limit:int -> string_ -> automaton
  (** Build an automaton from a string, with a maximal distance [limit].
      The automaton will accept strings whose {!edit_distance} to the
      parameter is at most [limit]. *)

  val of_list : limit:int -> char_ list -> automaton
  (** Build an automaton from a list, with a maximal distance [limit] *)

  val debug_print : (out_channel -> char_ -> unit) ->
                    out_channel -> automaton -> unit
  (** Output the automaton's structure on the given channel. *)

  val match_with : automaton -> string_ -> bool
  (** [match_with a s] matches the string [s] against [a], and returns
      [true] if the distance from [s] to the word represented by [a] is smaller
      than the limit used to build [a] *)

  (** {6 Index for one-to-many matching} *)

  module Index : sig
    type 'b t
    (** Index that maps strings to values of type 'b. Internally it is
       based on a trie. A string can only map to one value. *)

    val empty : 'b t
    (** Empty index *)

    val is_empty : _ t -> bool

    val add : 'b t -> string_ -> 'b -> 'b t
    (** Add a pair string/value to the index. If a value was already present
       for this string it is replaced. *)

    val cardinal : _ t -> int
    (** Number of bindings *)

    val remove : 'b t -> string_ -> 'b t
    (** Remove a string (and its associated value, if any) from the index. *)

    val retrieve : limit:int -> 'b t -> string_ -> 'b klist
    (** Lazy list of objects associated to strings close to the query string *)

    val of_list : (string_ * 'b) list -> 'b t
    (** Build an index from a list of pairs of strings and values *)

    val to_list : 'b t -> (string_ * 'b) list
    (** Extract a list of pairs from an index *)

    val add_seq : 'a t -> (string_ * 'a) sequence -> 'a t
    (** @since 0.14 *)

    val of_seq : (string_ * 'a) sequence -> 'a t
    (** @since 0.14 *)

    val to_seq : 'a t -> (string_ * 'a) sequence
    (** @since 0.14 *)

    val add_gen : 'a t -> (string_ * 'a) gen -> 'a t
    (** @since 0.14 *)

    val of_gen : (string_ * 'a) gen -> 'a t
    (** @since 0.14 *)

    val to_gen : 'a t -> (string_ * 'a) gen
    (** @since 0.14 *)

    val fold : ('a -> string_ -> 'b -> 'a) -> 'a -> 'b t -> 'a
    (** Fold over the stored pairs string/value *)

    val iter : (string_ -> 'b -> unit) -> 'b t -> unit
    (** Iterate on the pairs *)

    val to_klist : 'b t -> (string_ * 'b) klist
    (** Conversion to an iterator *)
  end
end

(** {2 Functor} *)

module Make(Str : STRING) : S
  with type string_ = Str.t
  and type char_ = Str.char_

(** {2 Default instance: string} *)

include S with type char_ = char and type string_ = string

val debug_print : out_channel -> automaton -> unit
