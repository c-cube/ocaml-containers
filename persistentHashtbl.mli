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

(** {1 Persistent hash-table on top of OCaml's hashtables} *)

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Signature of such a hashtable} *)

module type S = sig
  type key
  type 'a t

  val create : int -> 'a t
    (** Create a new hashtable *)

  val is_empty : 'a t -> bool
    (** Is the table empty? *)

  val find : 'a t -> key -> 'a
    (** Find the value for this key, or raise Not_found *)

  val mem : 'a t -> key -> bool
    (** Is the key bound? *)

  val length : 'a t -> int
    (** Number of bindings *)

  val replace : 'a t -> key -> 'a -> 'a t
    (** Add the binding to the table, returning a new table. This erases
        the current binding for [key], if any. *)

  val remove : 'a t -> key -> 'a t
    (** Remove the key *)

  val copy : 'a t -> 'a t
    (** Fresh copy of the table; the underlying structure is not shared
        anymore, so using both tables alternatively will be efficient *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
    (** Iterate over bindings *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold over bindings *)

  val of_seq : ?init:'a t -> (key * 'a) Sequence.t -> 'a t
    (** Add (replace) bindings from the sequence to the table *)

  val of_list : ?init:'a t -> (key * 'a) list -> 'a t

  val to_seq : 'a t -> (key * 'a) Sequence.t
    (** Sequence of the bindings of the table *)

  val to_list : 'a t -> (key * 'a) list
end

(** {2 Implementation} *)

module Make(H : HashedType) : S with type key = H.t

