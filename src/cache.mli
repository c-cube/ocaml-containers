(** An imperative cache of fixed size for memoization of pairs *)

module type S =
  sig 
    type key

    type 'a t

    (** create a cache with given size *)
    val create : int -> (key -> key -> 'a) -> 'a t

    (** find a value in the cache *)
    val lookup : 'a t -> key -> key -> 'a

    (** clear the cache from its content *)
    val clear : 'a t -> unit
  end

module type CachedType =
  sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
  end

(** functorial implementation *)
module Make(CType : CachedType) : S with type key = CType.t
