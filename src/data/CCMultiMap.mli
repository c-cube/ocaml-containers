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

(** {1 Multimap} *)

type 'a sequence = ('a -> unit) -> unit

module type S = sig
  type key
  type value
  type t

  val empty : t
    (** Empty multimap *)

  val is_empty : t -> bool
    (** Empty multimap? *)

  val add : t -> key -> value -> t
    (** Add a key/value binding *)

  val remove : t -> key -> value -> t
    (** Remove the binding *)

  val remove_all : t -> key -> t
    (** Remove the key from the map *)

  val mem : t -> key -> bool
    (** Is there a binding for this key? *)

  val find : t -> key -> value list
    (** List of values for this key *)

  val find_iter : t -> key -> (value -> unit) -> unit
    (** Iterate on bindings for this key *)

  val count : t -> key -> int
    (** Number of bindings for this key *)

  val iter : t -> (key -> value -> unit) -> unit
    (** Iterate on all key/value *)

  val fold : t -> 'a -> ('a -> key -> value -> 'a) -> 'a
    (** Fold on all key/value *)

  val size : t -> int
    (** Number of keys *)

  val union : t -> t -> t
    (** Union of multimaps *)

  val inter : t -> t -> t
    (** Intersection of multimaps *)

  val diff : t -> t -> t
    (** Difference of maps, ie bindings of the first that are not
        in the second *)

  val equal : t -> t -> bool
    (** Same multimap *)

  val compare : t -> t -> int
    (** Total order on multimaps *)

  val submap : t -> t -> bool
    (** [submap m1 m2] is true iff all bindings of [m1] are also in [m2] *)

  val to_seq : t -> (key * value) sequence

  val of_seq : ?init:t -> (key * value) sequence -> t

  val keys : t -> key sequence

  val values : t -> value sequence
    (** Some values may occur several times *)
end

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(K : OrderedType)(V : OrderedType) : S with type key = K.t and type value = V.t

(** {2 Two-Way Multimap}
Represents n-to-n mappings between two types. Each element from the "left"
is mapped to several right values, and conversely.

@since 0.3.3 *)

module type BIDIR = sig
  type t
  type left
  type right

  val empty : t

  val is_empty : t -> bool

  val add : t -> left -> right -> t
  (** Add a binding (left,right) *)

  val remove : t -> left -> right -> t
  (** Remove a specific binding *)

  val cardinal_left : t -> int
  (** Number of distinct left keys *)

  val cardinal_right : t -> int
  (** Number of distinct right keys *)

  val remove_left : t -> left -> t
  (** Remove all bindings for the left key *)

  val remove_right : t -> right -> t
  (** Remove all bindings for the right key *)

  val mem_left : t -> left -> bool
  (** Is the left key present in at least one pair? *)

  val mem_right : t -> right -> bool
  (** Is the right key present in at least one pair? *)

  val find_left : t -> left -> right sequence
  (** Find all bindings for this given left-key *)

  val find_right : t -> right -> left sequence
  (** Find all bindings for this given right-key *)

  val find1_left : t -> left -> right option
  (** like {!find_left} but returns at most one value *)

  val find1_right : t -> right -> left option
  (** like {!find_right} but returns at most one value *)

  val fold : ('a -> left -> right -> 'a) -> 'a -> t -> 'a
  (** Fold on pairs *)

  val pairs : t -> (left * right) sequence
  (** Iterate on pairs *)

  val add_pairs : t -> (left * right) sequence -> t
  (** Add pairs *)

  val seq_left : t -> left sequence
  val seq_right : t -> right sequence
end

module MakeBidir(L : OrderedType)(R : OrderedType) : BIDIR
  with type left = L.t and type right = R.t
