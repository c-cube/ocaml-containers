
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


(** {1 Levenshtein distance} *)

(** {2 Automaton} *)

type 'a automaton
  (** Levenshtein automaton for characters of type 'a *)

val of_array : ?compare:('a -> 'a -> int) -> limit:int -> 'a array -> 'a automaton
  (** Build an automaton from an array, with a maximal distance [limit] *)

val of_list : ?compare:('a -> 'a -> int) -> limit:int -> 'a list -> 'a automaton
  (** Build an automaton from a list, with a maximal distance [limit] *)

val of_string : limit:int -> string -> char automaton
  (** Automaton for the special case of strings *)

type match_result =
  | TooFar
  | Distance of int

val match_with : 'a automaton -> 'a array -> match_result
  (** [match_with a s] matches the string [s] against [a], and returns
      the distance from [s] to the word represented by [a] *)

val match_with_string : char automaton -> string -> match_result
  (** Specialized version of {!match_with} for strings *)
