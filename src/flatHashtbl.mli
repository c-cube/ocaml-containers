(** Open addressing hashtable, with linear probing. *)

module type S =
  sig
    type key

    type 'a t

    val create : ?max_load:float -> int -> 'a t
      (** Create a hashtable.  [max_load] is (number of items / size of table).
          Must be in ]0, 1[ *)

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

    val stats : 'a t -> int * int * int * int * int * int
      (** Cf Weak.S *)
  end

(** Create a hashtable *)
module Make(H : Hashtbl.HashedType) : S with type key = H.t

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
