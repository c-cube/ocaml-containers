(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Tries}

  Trie indexed by the hash of the keys, where the branching factor is fixed.
  The goal is to have a quite efficient functional structure with fast
  update and access {b if} the hash function is good.
  The trie is not binary, to improve cache locality and decrease depth.

  Preliminary benchmarks (see the "tbl" section of benchmarks) tend to show
  that this type is quite efficient for small data sets.

  {b status: unstable}

  @since 0.13
*)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(** {2 Transient Identifiers} *)
module Transient : sig
  type t
  (** Identifiers for transient modifications. A transient modification
      is uniquely identified by a [Transient.t]. Once [Transient.freeze r]
      is called, [r] cannot be used to modify the structure again. *)

  val create : unit -> t
  (** Create a new, active ID *)

  val equal : t -> t -> bool
  (** Equality between IDs *)

  val frozen : t -> bool
  (** [frozen i] returns [true] if [freeze i] was called before. In this case,
      the ID cannot be used for modifications again. *)

  val active : t -> bool
  (** [active i] is [not (frozen i)] *)

  val freeze : t -> unit
  (** [freeze i] makes [i] unusable for new modifications. The values
      created with [i] will now be immutable. *)

  val with_ : (t -> 'a) -> 'a
  (** [Transient.with_ f] creates a transient ID [i], calls [f i],
      freezes the ID [i] and returns the result of [f i]. *)

  exception Frozen
  (** Raised when a frozen ID is used *)
end

(** {2 Signature} *)
module type S = sig
  type key

  type 'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val singleton : key -> 'a -> 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val mem : key -> _ t -> bool

  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if key not present *)

  val remove : key -> 'a t -> 'a t
  (** Remove the key, if present. *)

  val update : key -> f:('a option -> 'a option) -> 'a t -> 'a t
  (** [update k ~f m] calls [f (Some v)] if [get k m = Some v], [f None]
      otherwise. Then, if [f] returns [Some v'] it binds [k] to [v'],
      if [f] returns [None] it removes [k] *)

  val add_mut : id:Transient.t -> key -> 'a -> 'a t -> 'a t
  (** [add_mut ~id k v m] behaves like [add k v m], except it will mutate
      in place whenever possible. Changes done with an [id] might affect all
      versions of the structure obtained with the same [id] (but not
      other versions).
      @raise Transient.Frozen if [id] is frozen *)

  val remove_mut : id:Transient.t -> key -> 'a t -> 'a t
  (** Same as {!remove}, but modifies in place whenever possible
      @raise Transient.Frozen if [id] is frozen *)

  val update_mut : id:Transient.t -> key -> f:('a option -> 'a option) -> 'a t -> 'a t
  (** Same as {!update} but with mutability
      @raise Transient.Frozen if [id] is frozen *)

  val cardinal : _ t -> int

  val choose : 'a t -> (key * 'a) option

  val choose_exn : 'a t -> key * 'a
  (** @raise Not_found if not pair was found *)

  val iter : f:(key -> 'a -> unit) -> 'a t -> unit

  val fold : f:('b -> key -> 'a -> 'b) -> x:'b -> 'a t -> 'b

  (** {6 Conversions} *)

  val to_list : 'a t -> (key * 'a) list

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val add_list_mut : id:Transient.t -> 'a t -> (key * 'a) list -> 'a t
  (** @raise Frozen if the ID is frozen *)

  val of_list : (key * 'a) list -> 'a t

  val add_seq : 'a t -> (key * 'a) sequence -> 'a t

  val add_seq_mut : id:Transient.t -> 'a t -> (key * 'a) sequence -> 'a t
  (** @raise Frozen if the ID is frozen *)

  val of_seq : (key * 'a) sequence -> 'a t

  val to_seq : 'a t -> (key * 'a) sequence

  val add_gen : 'a t -> (key * 'a) gen -> 'a t

  val add_gen_mut : id:Transient.t -> 'a t -> (key * 'a) gen -> 'a t
  (** @raise Frozen if the ID is frozen *)

  val of_gen : (key * 'a) gen -> 'a t

  val to_gen : 'a t -> (key * 'a) gen

  (** {6 IO} *)

  val print : key printer -> 'a printer -> 'a t printer

  val as_tree : 'a t -> [`L of int * (key * 'a) list | `N ] ktree
  (** For debugging purpose: explore the structure of the tree,
      with [`L (h,l)] being a leaf (with shared hash [h])
      and [`N] an inner node *)
end

(** {2 Type for keys} *)
module type KEY = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(** {2 Functors} *)
module Make(K : KEY) : S with type key = K.t

(**/**)
val popcount : int -> int
(**/**)
