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

(** {1 Multiset} *)

type 'a sequence = ('a -> unit) -> unit

module type S = sig
  type elt
  type t

  val empty : t

  val is_empty : t -> bool

  val mem : t -> elt -> bool

  val count : t -> elt -> int

  val singleton : elt -> t

  val add : t -> elt -> t

  val remove : t -> elt -> t

  val add_mult : t -> elt -> int -> t
  (** [add_mult set x n] adds [n] occurrences of [x] to [set]
      @raise Invalid_argument if [n < 0]
      @since 0.6 *)

  val remove_mult : t -> elt -> int -> t
  (** [remove_mult set x n] removes at most [n] occurrences of [x] from [set]
      @raise Invalid_argument if [n < 0]
      @since 0.6 *)

  val update : t -> elt -> (int -> int) -> t
  (** [update set x f] calls [f n] where [n] is the current multiplicity
      of [x] in [set] ([0] to indicate its absence); the result of [f n]
      is the new multiplicity of [x].
      @raise Invalid_argument if [f n < 0]
      @since 0.6 *)

  val min : t -> elt
  (** Minimal element w.r.t the total ordering on elements *)

  val max : t -> elt
  (** Maximal element w.r.t the total ordering on elements *)

  val union : t -> t -> t
  (** [union a b] contains as many occurrences of an element [x]
      as [count a x + count b x]. *)

  val meet : t -> t -> t
  (** [meet a b] is a multiset such that
      [count (meet a b) x = max (count a x) (count b x)] *)

  val intersection : t -> t -> t
  (** [intersection a b] is a multiset such that
      [count (intersection a b) x = min (count a x) (count b x)] *)

  val diff : t -> t -> t
  (** MultiSet difference.
      [count (diff a b) x = max (count a x - count b x) 0] *)

  val contains : t -> t -> bool
  (** [contains a x = (count m x > 0)] *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val cardinal : t -> int
  (** Number of distinct elements *)

  val iter : t -> (int -> elt -> unit) -> unit

  val fold : t -> 'b -> ('b -> int -> elt -> 'b) -> 'b

  val of_list : elt list -> t

  val to_list : t -> elt list

  val to_seq : t -> elt sequence

  val of_seq : elt sequence -> t
end

module Make(O : Set.OrderedType) : S with type elt = O.t
