
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

(** {1 Vantage-Point Tree}

A data structure used to store and retrieve elements that belong to a
metric space. References are:

  {{: https://github.com/UnixJunkie/vantage_point_tree_from_codepad/blob/master/vp_tree.ml} François Berenger's implementation}
  {{: http://en.wikipedia.org/wiki/Vantage-point_tree} Wikipedia}
*)

module type METRIC_SPACE = sig
  type t
    (** Elements of the metric space *)

  type num
    (** Numeric type used for distances *)

  val distance : t -> t -> num
    (** Distance between two points. It must satisfy the following invariants:

        - [distance x y = distance y x]
        - [distance x y <= distance x z + distance z y]
        - [distance x y >= 0]
        - [distance x y = 0] if and only if [x] and [y] are the same point.
    *)

  val add : num -> num -> num
    (** Addition of two distances (associative, commutative) *)

  val compare : num -> num -> int
    (** Total ordering on the numeric type used for distances; must
        be compatible with {!add} *)
end

(** {2 Utils} *)

type 'a sequence = ('a -> unit) -> unit

module LazyList : sig
  type +'a t =
    | Nil
    | Cons of 'a * 'a t Lazy.t

  val take : int -> 'a t -> 'a t

  val to_list : 'a t -> 'a list

  val iter : 'a t -> ('a -> unit) -> unit
end

(** {2 VPTree functorial interface} *)

module type S = sig
  module Space : METRIC_SPACE

  type key = Space.t

  type +'a t
    (** VPTree that maps to data of type 'a *)

  val member : _ t -> key -> bool
    (** Is the given key, member of the tree? *)

  val find_closest : 'a t -> key -> (key * 'a * int) LazyList.t
    (** [find_closest tree key] finds the points of [tree]
        that are the closest to [key], in increasing order. Enumerates all
        the points eventually, if the whole list is explored.
        Each tuple also contains the distance to the [key]. *)

  val of_array : (key * 'a) array -> 'a t
    (** Build a VPTree from an array of bindings. The array is modified in
        place (values are swapped).
        If a key occurs several times in the sequence, one of the value
        it maps to is chosen arbitrarily. *)

  val of_list : (key * 'a) list -> 'a t
    (** Build a VPTree from a list *)

  val of_seq : (key * 'a) sequence -> 'a t

  val of_lazy_list : (key * 'a) LazyList.t -> 'a t

  val size : _ t -> int
    (** Find the number of key/value pairs in the tree (linear time) *)
end

module Make(Space : METRIC_SPACE) : S with module Space = Space
