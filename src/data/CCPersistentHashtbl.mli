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

(** {1 Persistent hash-table on top of OCaml's hashtables}

Almost as efficient as the regular Hashtbl type, but with a persistent
interface (rewinding changes to get back in the past history). This is
mostly useful for backtracking-like uses, or forward uses (never using
old values).

This module is not thread-safe. *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit
type 'a equal = 'a -> 'a -> bool

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Signature of such a hashtable} *)

module type S = sig
  type key
  type 'a t

  val empty : unit -> 'a t
  (** Empty table. The table will be allocated at the first binding *)

  val create : int -> 'a t
  (** Create a new hashtable, with the given initial capacity *)

  val is_empty : 'a t -> bool
  (** Is the table empty? *)

  val find : 'a t -> key -> 'a
  (** Find the value for this key, or fails
      @raise Not_found if the key is not present in the table *)

  val get_exn : key -> 'a t -> 'a
  (** Synonym to {!find} with flipped arguments *)

  val get : key -> 'a t -> 'a option
  (** Safe version of !{get_exn} *)

  val mem : 'a t -> key -> bool
  (** Is the key bound? *)

  val length : _ t -> int
  (** Number of bindings *)

  val add : 'a t -> key -> 'a -> 'a t
  (** Add the binding to the table, returning a new table. The old binding
      for this key, if it exists, is shadowed and will be restored upon
      [remove tbl k].
      @since 0.14 *)

  val replace : 'a t -> key -> 'a -> 'a t
  (** Add the binding to the table, returning a new table. This erases
      the current binding for [key], if any. *)

  val update : 'a t -> key -> ('a option -> 'a option) -> 'a t
  (** [update tbl key f] calls [f None] if [key] doesn't belong in [tbl],
      [f (Some v)] if [key -> v] otherwise; If [f] returns [None] then
      [key] is removed, else it returns [Some v'] and [key -> v'] is added. *)

  val remove : 'a t -> key -> 'a t
  (** Remove the key *)

  val copy : 'a t -> 'a t
  (** Fresh copy of the table; the underlying structure is not shared
      anymore, so using both tables alternatively will be efficient *)

  val merge : (key -> 'a option -> 'a option -> 'a option) ->
              'a t -> 'a t -> 'a t
  (** Merge two tables together into a new table. The function's argument
      correspond to values associated with the key (if present); if the
      function returns [None] the key will not appear in the result. *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
  (** Iterate over bindings *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over bindings *)

  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map all values *)

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  (** {3 Conversions} *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** Add (replace) bindings from the sequence to the table *)

  val of_list : (key * 'a) list -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t

  val add_list : 'a t -> (key  * 'a) list -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence
  (** Sequence of the bindings of the table *)

  val to_list : 'a t -> (key * 'a) list

  (** {3 Misc} *)

  val equal : 'a equal -> 'a t equal

  val pp : key printer -> 'a printer -> 'a t printer

  val print : key formatter -> 'a formatter -> 'a t formatter

  val stats : _ t -> Hashtbl.statistics
  (** Statistics on the internal table.
      @since 0.14 *)
end

(** {2 Implementation} *)

module Make(H : HashedType) : S with type key = H.t
