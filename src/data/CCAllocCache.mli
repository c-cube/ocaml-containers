
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Cache for Allocations}

  Be very careful not to use-after-free or double-free.

  {b NOT THREAD SAFE}
  {b status: experimental}

  @since 0.15

*)

module Arr : sig
  type 'a t
  (** Cache for 'a arrays *)

  val create: ?buck_size:int -> int -> 'a t
  (** [create n] makes a new cache of arrays up to length [n]
      @param buck_size number of arrays cached for each array length
      @param n maximum size of arrays put in cache *)

  val make : 'a t -> int -> 'a -> 'a array
  (** [make cache i x] is like [Array.make i x],
      but might return a cached array instead of allocating one.
      {b NOTE}: if the array is already allocated then it
        will NOT be filled with [x] *)

  val free : 'a t -> 'a array -> unit
  (** Return array to the cache. The array's elements will not be GC'd *)

  val with_ : 'a t -> int -> 'a -> f:('a array -> 'b) -> 'b
  (** Combines {!make} and {!free} *)
end
