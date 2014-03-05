
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

(** {2 Abstraction over Strings} *)

module type STRING = sig
  type char_
  type t

  val of_list : char_ list -> t
  val get : t -> int -> char_
  val length : t -> int
  val compare_char : char_ -> char_ -> int
end

(** {2 Continuation list} *)
type 'a klist =
  [
  | `Nil
  | `Cons of 'a * (unit -> 'a klist)
  ]

val klist_to_list : 'a klist -> 'a list
  (** Helper. *)

(** {2 Signature} *)

module type S = sig
  type char_
  type string_

  (** {6 Automaton} *)

  type automaton
    (** Levenshtein automaton *)

  val of_string : limit:int -> string_ -> automaton
    (** Build an automaton from an array, with a maximal distance [limit] *)

  val of_list : limit:int -> char_ list -> automaton
    (** Build an automaton from a list, with a maximal distance [limit] *)

  val debug_print : (out_channel -> char_ -> unit) ->
                    out_channel -> automaton -> unit
    (** Output the automaton on the given channel. *)

  val match_with : automaton -> string_ -> bool
    (** [match_with a s] matches the string [s] against [a], and returns
        [true] if the distance from [s] to the word represented by [a] is smaller
        than the limit used to build [a] *)

  (** {6 Index for one-to-many matching} *)

  module Index : sig
    type 'b t
      (** Index that maps strings to values of type 'b. Internally it is
         based on a trie. *)

    val empty : 'b t
      (** Empty index *)

    val is_empty : _ t -> bool

    val add : 'b t -> string_ -> 'b -> 'b t
      (** Add a char array to the index. If a value was already present
         for this array it is replaced. *)

    val remove : 'b t -> string_ -> 'b -> 'b t
      (** Remove a string from the index. *)

    val retrieve : limit:int -> 'b t -> string_ -> 'b klist
      (** Lazy list of objects associated to strings close to the query string *)

    val of_list : (string_ * 'b) list -> 'b t

    val to_list : 'b t -> (string_ * 'b) list

    (* TODO sequence/iteration functions *)
  end
end

module Make(Str : STRING) : S
  with type string_ = Str.t
  and type char_ = Str.char_

include S with type char_ = char and type string_ = string

val debug_print : out_channel -> automaton -> unit
