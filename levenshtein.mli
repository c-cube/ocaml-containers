
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

(** {2 Automaton} *)

type 'a automaton
  (** Levenshtein automaton for characters of type 'a *)

val of_array : ?compare:('a -> 'a -> int) -> limit:int -> 'a array -> 'a automaton
  (** Build an automaton from an array, with a maximal distance [limit] *)

val of_list : ?compare:('a -> 'a -> int) -> limit:int -> 'a list -> 'a automaton
  (** Build an automaton from a list, with a maximal distance [limit] *)

val of_string : limit:int -> string -> char automaton
  (** Automaton for the special case of strings *)

val debug_print : out_channel -> char automaton -> unit
  (** Output the automaton on the given channel. Only for string automata. *)

val match_with : 'a automaton -> 'a array -> bool
  (** [match_with a s] matches the string [s] against [a], and returns
      [true] if the distance from [s] to the word represented by [a] is smaller
      than the limit used to build [a] *)

val match_with_string : char automaton -> string -> bool
  (** Specialized version of {!match_with} for strings *)

(** {6 Index for one-to-many matching} *)

(** Continuation list *)
type 'a klist =
  [
  | `Nil
  | `Cons of 'a * (unit -> 'a klist)
  ]

val klist_to_list : 'a klist -> 'a list
  (** Helper. *)

module Index(X : Map.OrderedType) : sig
  type key = X.t

  type 'b t
    (** Index that maps [key] strings to values of type 'b. Internally it is
       based on a trie. *)

  val empty : 'b t
    (** Empty index *)

  val is_empty : _ t -> bool

  val add : 'b t -> key array -> 'b -> 'b t
    (** Add a char array to the index. If a value was already present
       for this array it is replaced. *)

  val remove : 'b t -> key array -> 'b -> 'b t
    (** Remove a char array from the index. *)

  val retrieve : limit:int -> 'b t -> key array -> 'b klist
    (** Lazy list of objects associated to strings close to
        the query string *)

  val of_list : (key array * 'b) list -> 'b t

  val to_list : 'b t -> (key array * 'b) list

  (* TODO sequence/iteration functions *)
end

(** Specific case for strings *)
module StrIndex : sig
  include module type of Index(Char)

  val add_string : 'b t -> string -> 'b -> 'b t
    (** Add a string to a char index *)

  val remove_string : 'b t -> string -> 'b -> 'b t
    (** Remove a string from a char index *)

  val retrieve_string : limit:int -> 'b t -> string -> 'b klist

  val of_str_list : (string * 'b) list -> 'b t

  val to_str_list : 'b t -> (string * 'b) list
end
