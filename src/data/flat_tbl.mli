(** {1 Open addressing hashtable, with linear probing.} *)

type 'a iter = ('a -> unit) -> unit

module type S = sig
  type key

  type 'a t

  val create : int -> 'a t
  (** Create a hashtable. *)

  val copy : 'a t -> 'a t

  val clear : 'a t -> unit
  (** Clear the content of the hashtable *)

  val find : 'a t -> key -> 'a
  (** Find the value for this key, or
      @raise Not_found if not present *)

  val find_opt : 'a t -> key -> 'a option
  (** Find the value for this key *)

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

  val to_iter : 'a t -> (key * 'a) iter

  val add_iter : 'a t -> (key * 'a) iter -> unit

  val of_iter : (key * 'a) iter -> 'a t

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> unit

  val of_list : (key * 'a) list -> 'a t

  val stats : 'a t -> int * int * int * int * int * int
  (** Cf Weak.S *)

  (**/**)
  val _pp_dib : _ t -> string
  (**/**)
end

(** Create a hashtable *)
module Make(H : Hashtbl.HashedType) : S with type key = H.t
