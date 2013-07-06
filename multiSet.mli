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

module type S = sig
  type elt
  type t

  val empty : t

  val mem : t -> elt -> bool

  val count : t -> elt -> int

  val singleton : elt -> t

  val add : t -> elt -> t

  val remove : t -> elt -> t

  val min : t -> elt

  val max : t -> elt

  val union : t -> t -> t

  val intersection : t -> t -> t

  val diff : t -> t -> t

  val contains : t -> t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val cardinal : t -> int
    (** Number of distinct elements *)

  val iter : t -> (int -> elt -> unit) -> unit

  val fold : t -> 'b -> ('b -> int -> elt -> 'b) -> 'b

  val of_list : elt list -> t

  val to_list : t -> elt list
end

module Make(O : Set.OrderedType) : S with type elt = O.t
