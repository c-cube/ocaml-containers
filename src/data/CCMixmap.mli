(*
copyright (c) 2013-2014, simon cruanes
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

(** {1 Maps with Heterogeneous Values}

{b status: experimental}

@since 0.9 *)

type 'a injection
(** An accessor for values of type 'a in any map. Values put
    in the map using a key can only be retrieved using this
    very same key. *)

val create_inj : unit -> 'a injection
(** Return a value that works for a given type of values.  This function is
    normally called once for each type of value.  Several keys may be
    created for the same type, but a value set with a given setter can only be
    retrieved with the matching getter.  The same key can be reused
    across multiple maps (although not in a thread-safe way). *)

module type S = sig
  type key

  type t
  (** A map containing values of different types, indexed by {!key}. *)

  val empty : t
  (** Empty map *)

  val get : inj:'a injection -> t  -> key -> 'a option
  (** Get the value corresponding to this key, if it exists and
      belongs to the same key *)

  val add : inj:'a injection -> t -> key -> 'a -> t
  (** Bind the key to the value, using [inj] *)

  val find : inj:'a injection -> t -> key -> 'a
  (** Find the value for the given key, which must be of the right type.
      @raise Not_found if either the key is not found, or if its value
        doesn't belong to the right type *)

  val cardinal : t -> int
  (** Number of bindings *)

  val remove : t -> key -> t
  (** Remove the binding for this key *)

  val mem : inj:_ injection-> t -> key -> bool
  (** Is the given key in the map, with the right type? *)

  val iter_keys : f:(key -> unit) -> t -> unit
  (** Iterate on the keys of this map *)

  val fold_keys : f:('a -> key -> 'a) -> x:'a -> t -> 'a
  (** Fold over the keys *)

  (** {2 Iterators} *)

  type 'a sequence = ('a -> unit) -> unit

  val keys_seq : t -> key sequence
  (** All the keys *)

  val bindings_of : inj:'a injection -> t -> (key * 'a) sequence
  (** All the bindings that come from the corresponding injection *)

  type value =
    | Value : ('a injection -> 'a option) -> value

  val bindings : t -> (key * value) sequence
  (** Iterate on all bindings *)
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORD) : S with type key = X.t
