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

(** Open addressing hashtable, with linear probing. *)

type 'a sequence = ('a -> unit) -> unit

module type S =
  sig
    type key

    type 'a t

    val create : ?max_load:float -> int -> 'a t
      (** Create a hashtable.  [max_load] is (number of items / size of table).
          Must be in {v ]0, 1[ v} *)

    val copy : 'a t -> 'a t

    val clear : 'a t -> unit
      (** Clear the content of the hashtable *)

    val find : 'a t -> key -> 'a
      (** Find the value for this key, or raise Not_found *)

    val replace : 'a t -> key -> 'a -> unit
      (** Add/replace the binding for this key. O(1) amortized. *)

    val remove : 'a t -> key -> unit
      (** Remove the binding for this key, if any *)

    val length : 'a t -> int
      (** Number of bindings in the table *)

    val mem : 'a t -> key -> bool
      (** Is the key present in the hashtable? *)

    val iter : (key -> 'a -> unit) -> 'a t -> unit
      (** Iterate on bindings *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      (** Fold on bindings *)

    val to_seq : 'a t -> (key * 'a) sequence

    val of_seq : 'a t -> (key * 'a) sequence -> unit

    val stats : 'a t -> int * int * int * int * int * int
      (** Cf Weak.S *)
  end

(** Create a hashtable *)
module Make(H : Hashtbl.HashedType) : S with type key = H.t

(** The hashconsing part has the very bad property that it may introduce
    memory leak, because the hashtable is not weak. Be warned. *)

(** Hashconsed type *)
module type HashconsedType =
  sig
    include Hashtbl.HashedType
    val tag : int -> t -> t
  end

(** Create a hashconsing module *)
module Hashcons(H : HashconsedType) :
  sig
    type t = H.t

    val hashcons : t -> t

    val iter : (t -> unit) -> unit

    val stats : unit -> int * int * int * int * int * int
  end
