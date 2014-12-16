(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Imperative Union-Find structure} *)

(** This structure operates on arbitrary objects as long as they are
    hashable. It maps keys to values (values belong to a monoid,
    if they are not needed, unit makes for a simple implementation)
    and each equivalence class' representative maps to
    the monoid merge of all the class' elements values.
    One also can iterate on the representative elements. *)

(** We need to be able to hash and compare keys, and values need to form
    a monoid *)
module type PAIR = sig
  type key
  type value

  val hash : key -> int
  val equal : key -> key -> bool

  val merge : value -> value -> value   (** Should be associative commutative *)
  val zero : value  (** Neutral element of {!merge} *)
end

(** Build a union-find module from a key/value specification *)
module Make(P : PAIR) : sig
  type key = P.key
    (** Elements that can be compared *)

  type value = P.value
    (** Values associated with elements *)

  type t
    (** The union-find imperative structure itself *)

  val create : key list -> t
    (** Create a union-find for the given elements. Elements are mapped
        to zero by default. *)

  val mem : t -> key -> bool
    (** Does the key belong to the UF? *)

  val find : t -> key -> key
    (** Finds the representative of this key's equivalence class.
        @raise Not_found if the key does not belong to the UF *)

  val find_value : t -> key -> value
    (** Find value for the given element. The value is the monoid
        merge of all values associated to [key]'s equivalence class.
        @raise Not_found if [mem uf key] is false. *)

  val union : t -> key -> key -> unit
    (** Merge two elements (and their equivalence classes) *)

  val add : t -> key -> value -> unit
    (** Add the given value to the key's class (monoid). It modifies the value
        by merging it with [value]. If the key does not belong
        to the union-find, it is added. *)

  val iter : t -> (key -> value -> unit) -> unit
    (** Iterate on representative and their value *)
end
