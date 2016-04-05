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

(** {1 Prefix Tree} *)

type 'a sequence = ('a -> unit) -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Signatures} *)

(** {6 A Composite Word}

Words are made of characters, who belong to a total order *)

module type WORD = sig
  type t
  type char_

  val compare : char_ -> char_ -> int
  val to_seq : t -> char_ sequence
  val of_list : char_ list -> t
end

module type S = sig
  type char_
  type key

  type 'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val add : key -> 'a -> 'a t -> 'a t
  (** Add a binding to the trie (possibly erasing the previous one) *)

  val remove : key -> 'a t -> 'a t
  (** Remove the key, if present *)

  val find : key -> 'a t -> 'a option
  (** Find the value associated with the key, if any *)

  val find_exn : key -> 'a t -> 'a
  (** Same as {!find} but can fail.
      @raise Not_found if the key is not present *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** Update the binding for the given key. The function is given
      [None] if the key is absent, or [Some v] if [key] is bound to [v];
      if it returns [None] the key is removed, otherwise it
      returns [Some y] and [key] becomes bound to [y] *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on key/value bindings. Will use {!WORD.of_list} to rebuild keys. *)

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map values, giving both key and value. Will use {!WORD.of_list} to rebuild keys. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map values, giving only the value. *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Same as {!fold}, but for effectful functions *)

  val fold_values : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** More efficient version of {!fold}, that doesn't keep keys *)

  val iter_values : ('a -> unit) -> 'a t -> unit

  val merge : ('a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  (** Merge two tries together. The function is used in
      case of conflicts, when a key belongs to both tries *)

  val size : _ t -> int
  (** Number of bindings *)

  (** {6 Conversions} *)

  val to_list : 'a t -> (key * 'a) list

  val of_list : (key * 'a) list -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence

  val of_seq : (key * 'a) sequence -> 'a t

  val to_seq_values : 'a t -> 'a sequence

  val to_tree : 'a t -> [`Char of char_ | `Val of 'a | `Switch] ktree

  (** {6 Ranges} *)

  val above : key -> 'a t -> (key * 'a) sequence
  (** All bindings whose key is bigger or equal to the given key *)

  val below : key -> 'a t -> (key * 'a) sequence
  (** All bindings whose key is smaller or equal to the given key *)

  (**/**)
  val check_invariants: _ t -> bool
  (**/**)
end

(** {2 Implementation} *)

module Make(W : WORD) : S with type key = W.t and type char_ = W.char_

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MakeArray(X : ORDERED) : S with type key = X.t array and type char_ = X.t

module MakeList(X : ORDERED) : S with type key = X.t list and type char_ = X.t

module String : S with type key = string and type char_ = char
