
(* This file is free software, part of containers. See file "license" for more details. *)

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

  val longest_prefix : key -> 'a t -> key
  (** [longest_prefix k m] finds the longest prefix of [k] that leads to
      at least one path in [m] (it does not mean that the prefix is bound to
      a value.

      Example: if [m] has keys "abc0" and "abcd", then [longest_prefix "abc2" m]
      will return "abc"

      @since 0.17 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** Update the binding for the given key. The function is given
      [None] if the key is absent, or [Some v] if [key] is bound to [v];
      if it returns [None] the key is removed, otherwise it
      returns [Some y] and [key] becomes bound to [y] *)

  val fold : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on key/value bindings. Will use {!WORD.of_list} to rebuild keys. *)

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map values, giving both key and value. Will use {!WORD.of_list} to rebuild keys.
      @since 0.17 *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map values, giving only the value.
      @since 0.17  *)

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
  (** All bindings whose key is bigger or equal to the given key, in
      ascending order *)

  val below : key -> 'a t -> (key * 'a) sequence
  (** All bindings whose key is smaller or equal to the given key,
      in decreasing order *)

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
