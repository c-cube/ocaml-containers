
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


(** {1 Extension to the standard Hashtbl}

@since 0.4 *)

type 'a sequence = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

val get : ('a,'b) Hashtbl.t -> 'a -> 'b option
(** Safe version of {!Hashtbl.find} *)

val keys : ('a,'b) Hashtbl.t -> 'a sequence
(** Iterate on keys (similar order as {!Hashtbl.iter}) *)

val values : ('a,'b) Hashtbl.t -> 'b sequence
(** Iterate on values in the table *)

val keys_list : ('a, 'b) Hashtbl.t -> 'a list
(** [keys t] is the list of keys in [t].
    @since 0.8 *)

val values_list : ('a, 'b) Hashtbl.t -> 'b list
(** [values t] is the list of values in [t].
    @since 0.8 *)

val map_list : ('a -> 'b -> 'c) -> ('a, 'b) Hashtbl.t -> 'c list
(** Map on a hashtable's items, collect into a list *)

val to_seq : ('a,'b) Hashtbl.t -> ('a * 'b) sequence
(** Iterate on bindings in the table *)

val of_seq : ('a * 'b) sequence -> ('a,'b) Hashtbl.t
(** From the given bindings, added in order *)

val to_list : ('a,'b) Hashtbl.t -> ('a * 'b) list
(** List of bindings (order unspecified)  *)

val of_list : ('a * 'b) list -> ('a,'b) Hashtbl.t
(** From the given list of bindings, added in order *)

val print : 'a printer -> 'b printer -> ('a, 'b) Hashtbl.t printer
(** Printer for table
    @since 0.13 *)

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** Safe version of {!Hashtbl.find} *)

  val keys : 'a t -> key sequence
  (** Iterate on keys (similar order as {!Hashtbl.iter}) *)

  val values : 'a t -> 'a sequence
  (** Iterate on values in the table *)

  val keys_list : ('a, 'b) Hashtbl.t -> 'a list
  (** [keys t] is the list of keys in [t].
      @since 0.8 *)

  val values_list : ('a, 'b) Hashtbl.t -> 'b list
  (** [values t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Iterate on values in the table *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** From the given bindings, added in order *)

  val to_list : 'a t -> (key * 'a) list
  (** List of bindings (order unspecified)  *)

  val of_list : (key * 'a) list -> 'a t
  (** From the given list of bindings, added in order *)

  val print : key printer -> 'a printer -> 'a t printer
  (** Printer for tables
      @since 0.13 *)
end

module Make(X : Hashtbl.HashedType) :
  S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t

(** {2 Default Table}

A table with a default element for keys that were never added. *)

module type DEFAULT = sig
  type key

  type 'a t
  (** A hashtable for keys of type [key] and values of type ['a] *)

  val create : ?size:int -> 'a -> 'a t
  (** [create d] makes a new table that maps every key to [d] by default.
      @param size optional size of the initial table *)

  val create_with : ?size:int -> (key -> 'a) -> 'a t
  (** Similar to [create d] but here [d] is a function called to obtain a
      new default value for each distinct key. Useful if the default
      value is stateful. *)

  val get : 'a t -> key -> 'a
  (** Unfailing retrieval (possibly returns the default value). This will
      modify the table if the key wasn't present. *)

  val set : 'a t -> key -> 'a -> unit
  (** Replace the current binding for this key *)

  val remove : 'a t -> key -> unit
  (** Remove the binding for this key. If [get tbl k] is called later, the
      default value for the table will be returned *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Pairs of [(elem, value)] for all elements on which [get] was called *)
end

module MakeDefault(X : Hashtbl.HashedType) : DEFAULT with type key = X.t

(** {2 Count occurrences using a Hashtbl} *)

module type COUNTER = sig
  type elt
  (** Elements that are to be counted *)

  type t

  val create : int -> t
  (** A counter maps elements to natural numbers (the number of times this
      element occurred) *)

  val incr : t -> elt -> unit
  (** Increment the counter for the given element *)

  val incr_by : t -> int -> elt -> unit
  (** Add several occurrences at once *)

  val get : t -> elt -> int
  (** Number of occurrences for this element *)

  val add_seq : t -> elt sequence -> unit
  (** Increment each element of the sequence *)

  val of_seq : elt sequence -> t
  (** [of_seq s] is the same as [add_seq (create ())] *)
end

module MakeCounter(X : Hashtbl.HashedType) : COUNTER with type elt = X.t
