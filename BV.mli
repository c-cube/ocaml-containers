
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

(** {2 Imperative Bitvectors} *)

type t

val create : size:int -> bool -> t
  (** Create a bitvector of given size, with given default value *)

val copy : t -> t
  (** Copy of bitvector *)

val resize : t -> size:int -> bool -> t
  (** [resize bv ~size default] resizes [bv] to the given size. If the
      new size is bigger than the old one, new values are set to [default]. *)

val cardinal : t -> int
  (** Number of bits set *)

val length : t -> int
  (** Length of underlying array *)

val is_empty : t -> bool
  (** Any bit set? *)

val set : t -> int -> unit
  (** Set i-th bit *)

val get : t -> int -> bool
  (** Is the i-th bit true? *)

val reset : t -> int -> unit
  (** Set i-th bit to 0 *)

val flip : t -> int -> unit
  (** Flip i-th bit *)

val clear : t -> unit
  (** Set every bit to 0 *)

val iter : t -> (int -> bool -> unit) -> unit
  (** Iterate on all bits *)

val iter_true : t -> (int -> unit) -> unit
  (** Iterate on bits set to 1 *)

val to_list : t -> int list
  (** List of indexes that are true *)

val of_list : int list -> t
  (** From a list of true bits *)

val union_into : into:t -> t -> unit
  (** [union ~into bv] sets [into] to the union of itself and [bv].
      [into] must have at least as long as [bv]. *)

val inter_into : into:t -> t -> unit
  (** [union ~into bv] sets [into] to the intersection of itself and [bv] *)

val union : t -> t -> t
  (** [union bv1 bv2] returns the union of the two sets *)

val inter : t -> t -> t

val select : t -> 'a array -> ('a * int) list
  (** [select arr bv] selects the elements of [arr] whose index
      correspond to a true bit in [bv]. The elements are paired to their
      index in [arr]. If [bv] is too short, elements of [arr] with too high
      an index cannot be returned. *)
