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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
  damages (including, but not limited to, procurement of substitute goods or
  services; loss of use, data, or profits; or business interruption) however
  caused and on any theory of liability, whether in contract, strict liability,
  or tort (including negligence or otherwise) arising in any way out of the use
  of this software, even if advised of the possibility of such damage.
*)

(** {1 Abstract set/relation} *)

type 'a sequence = ('a -> unit) -> unit

type 'a t

val empty : 'a t
  (** Empty set *)

val mem : 'a t -> 'a -> bool
  (** [mem set x] returns true iff [x] belongs to the set *)

val iter : 'a t -> ('a -> unit) -> unit
  (** Iterate on the set elements **)

val fold : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
  (** Fold on the set *)

val cardinal : _ t -> int
  (** Number of elements *)

val singleton : ?eq:('a -> 'a -> bool) -> 'a -> 'a t
  (** Single-element set *)

val mk_generic : ?cardinal:(unit -> int) ->
                 mem:('a -> bool) ->
                 iter:(('a -> unit) -> unit) -> 'a t
  (** CCGeneric constructor. Takes a membership function and an iteration
      function, and possibly a cardinal function (supposed to return
      the number of elements) *)

val of_hashtbl : ('a, _) Hashtbl.t -> 'a t
  (** Set composed of the keys of this hashtable. The cardinal is computed
      using the number of bindings, so keys with multiple bindings will
      entail errors in {!cardinal} !*)

val filter : 'a t -> ('a -> bool) -> 'a t
  (** Filter the set *)

val union : 'a t -> 'a t -> 'a t

val intersection : 'a t -> 'a t -> 'a t

val product : 'a t -> 'b t -> ('a * 'b) t
  (** Cartesian product *)

val to_seq : 'a t -> 'a sequence

val to_list : 'a t -> 'a list

(** {2 Set builders} *)

(** A set builder is a value that serves to build a set, element by element.
    Several implementations can be provided, but the two operations that
    must be present are:

    - add an element to the builder
    - extract the set composed of all elements added so far
*)

type 'a builder

val mk_builder : add:('a -> unit) -> get:(unit -> 'a t) -> 'a builder
  (** CCGeneric set builder *)

val builder_hash : ?size:int ->
                   ?eq:('a -> 'a -> bool) ->
                   ?hash:('a -> int) -> unit -> 'a builder
  (** Builds a set from a Hashtable. [size] is the initial size *)

val builder_cmp : ?cmp:('a -> 'a -> int) -> unit -> 'a builder

val of_seq_builder : builder:'a builder -> 'a sequence -> 'a t
  (** Uses the given builder to construct a set from a sequence of elements *)

val of_seq_hash : ?eq:('a -> 'a -> bool) -> ?hash:('a -> int) -> 'a sequence -> 'a t
  (** Construction of a set from a sequence of hashable elements *)

val of_seq_cmp : ?cmp:('a -> 'a -> int) -> 'a sequence -> 'a t
  (** Construction of a set from a sequence of comparable elements *)

val of_list : 'a list -> 'a t
  (** Helper that uses default hash function and equality to build a set *)

val map : ?builder:'b builder -> 'a t -> f:('a -> 'b) -> 'b t
  (** Eager map from a set to another set. The result is built immediately
      using a set builder *)

val hash_join : ?eq:('key -> 'key -> bool) ->
           ?size:int ->
           ?hash:('key -> int) ->
           ?builder:'res builder ->
           project1:('a -> 'key) ->
           project2:('b -> 'key) ->
           merge:('a -> 'b -> 'res) ->
           'a t -> 'b t -> 'res t
  (** Relational join between two sets. The two sets are joined on
      the 'key type, and rows are merged into 'res.
      This takes at least three functions
      in addition to optional parameters:

      - [project1] extracts keys from rows of the first set
      - [project2] extracts keys from rows of the second set
      - [merge] merges rows that have the same key together
  *)

(** {2 Functorial interfaces} *)

module MakeHash(X : Hashtbl.HashedType) : sig
  type elt = X.t
    (** Elements of the set are hashable *)

  val of_seq : ?size:int -> elt sequence -> elt t
    (** Build a set from a sequence *)
end


module MakeSet(S : Set.S) : sig
  type elt = S.elt

  val of_seq : ?init:S.t -> elt sequence -> elt t
    (** Build a set from a sequence *)

  val of_set : S.t -> elt t
    (** Explicit conversion from a tree set *)

  val to_set : elt t -> S.t
    (** Conversion to a set (linear time) *)
end
