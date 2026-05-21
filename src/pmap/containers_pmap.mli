(** Persistent hash trie with functional semantics.
   Branching factor 16 (4 bits per level), but otherwise inspired
   from CCHashtrie.

   @since NEXT_RELEASE *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit

module type KEY = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

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

  val update : key -> f:('a option -> 'a option) -> 'a t -> 'a t
  (** [update k ~f m] calls [f (Some v)] if [get k m = Some v],
      [f None] otherwise. Then, if [f] returns [Some v'] it binds [k] to [v'],
      if [f] returns [None] it removes [k] *)

  val cardinal : _ t -> int
  val choose : 'a t -> (key * 'a) option

  val choose_exn : 'a t -> key * 'a
  (** @raise Not_found if no pair was found *)

  val iter : f:(key -> 'a -> unit) -> 'a t -> unit
  val fold : f:('b -> key -> 'a -> 'b) -> x:'b -> 'a t -> 'b
  val to_list : 'a t -> (key * 'a) list
  val add_list : 'a t -> (key * 'a) list -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val add_iter : 'a t -> (key * 'a) iter -> 'a t
  val of_iter : (key * 'a) iter -> 'a t
  val to_iter : 'a t -> (key * 'a) iter
  val add_gen : 'a t -> (key * 'a) gen -> 'a t
  val of_gen : (key * 'a) gen -> 'a t
  val to_gen : 'a t -> (key * 'a) gen
  val pp : key printer -> 'a printer -> 'a t printer
end

module Make (Key : KEY) : S with type key = Key.t
