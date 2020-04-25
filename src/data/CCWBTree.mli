(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Weight-Balanced Tree}

    {b status: experimental}

    @since 0.13 *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type KEY = sig
  include ORD
  val weight : t -> int
end

(** {2 Signature} *)

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : _ t -> bool

  val singleton : key -> 'a -> 'a t

  val mem : key -> _ t -> bool

  val get : key -> 'a t -> 'a option

  val get_exn : key -> 'a t -> 'a
  (** @raise Not_found if the key is not present. *)

  val nth : int -> 'a t -> (key * 'a) option
  (** [nth i m] returns the [i]-th [key, value] in the ascending
      order. Complexity is [O(log (cardinal m))]. *)

  val nth_exn : int -> 'a t -> key * 'a
  (** @raise Not_found if the index is invalid. *)

  val get_rank : key -> 'a t -> [`At of int | `After of int | `First]
  (** [get_rank k m] looks for the rank of [k] in [m], i.e. the index
      of [k] in the sorted list of bindings of [m].
      [let (`At n) = get_rank k m in nth_exn n m = get m k] should hold.
      @since 1.4 *)

  val add : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [get k m = Some v], [f None]
      otherwise. Then, if [f] returns [Some v'] it binds [k] to [v'],
      if [f] returns [None] it removes [k]. *)

  val cardinal : _ t -> int

  val weight : _ t -> int

  val fold : f:('b -> key -> 'a -> 'b) -> x:'b -> 'a t -> 'b

  val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
  (** Map values, giving both key and value. Will use {!WORD.of_list} to rebuild keys.
      @since 0.17
  *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** Map values, giving only the value.
      @since 0.17
  *)

  val iter : f:(key -> 'a -> unit) -> 'a t -> unit

  val split : key -> 'a t -> 'a t * 'a option * 'a t
  (** [split k t] returns [l, o, r] where [l] is the part of the map
      with keys smaller than [k], [r] has keys bigger than [k],
      and [o = Some v] if [k, v] belonged to the map. *)

  val merge : f:(key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  (** Like {!Map.S.merge}. *)

  val extract_min : 'a t -> key * 'a * 'a t
  (** [extract_min m] returns [k, v, m'] where [k,v] is the pair with the
      smallest key in [m], and [m'] does not contain [k].
      @raise Not_found if the map is empty. *)

  val extract_max : 'a t -> key * 'a * 'a t
  (** [extract_max m] returns [k, v, m'] where [k,v] is the pair with the
      highest key in [m], and [m'] does not contain [k].
      @raise Not_found if the map is empty. *)

  val choose : 'a t -> (key * 'a) option

  val choose_exn : 'a t -> key * 'a
  (** @raise Not_found if the tree is empty. *)

  val random_choose : Random.State.t -> 'a t -> key * 'a
  (** Randomly choose a (key,value) pair within the tree, using weights
      as probability weights.
      @raise Not_found if the tree is empty. *)

  val add_list : 'a t -> (key * 'a) list -> 'a t

  val of_list : (key * 'a) list -> 'a t

  val to_list : 'a t -> (key * 'a) list

  val add_iter : 'a t -> (key * 'a) iter -> 'a t

  val of_iter : (key * 'a) iter -> 'a t

  val to_iter : 'a t -> (key * 'a) iter

  val add_gen : 'a t -> (key * 'a) gen -> 'a t

  val of_gen : (key * 'a) gen -> 'a t

  val to_gen : 'a t -> (key * 'a) gen

  val pp : key printer -> 'a printer -> 'a t printer
  (** Renamed from [val print].
      @since 2.0 *)

  (**/**)
  val node_ : key -> 'a -> 'a t -> 'a t -> 'a t
  val balanced : _ t -> bool
  (**/**)
end

(** {2 Functor} *)

module Make(X : ORD) : S with type key = X.t

module MakeFull(X : KEY) : S with type key = X.t
(** Use the custom [X.weight] function *)
