
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Maps with Heterogeneous Values}

    {b status: experimental}

    {[
      module M = CCMixmap.Make(CCInt)

      let inj_int = CCMixmap.create_inj()
      let inj_str = CCMixmap.create_inj()
      let inj_list_int = CCMixmap.create_inj()

      let m =
        M.empty
        |> M.add ~inj:inj_int 1 1
        |> M.add ~inj:inj_str 2 "2"
        |> M.add ~inj:inj_list_int 3 [3;3;3]

      assert (M.get ~inj:inj_int 1 m = Some 1)
      assert (M.get ~inj:inj_str 1 m = None)
      assert (M.get ~inj:inj_str 2 m = Some "2")
      assert (M.get ~inj:inj_int 2 m = None)
      assert (M.get ~inj:inj_list_int 3 m = Some [3;3;3])
      assert (M.get ~inj:inj_str 3 m = None)
    ]}

    @since 0.9
    @since 0.16 change of API, the map is last argument to
      make piping with [|>] easier. *)

type 'a injection
(** An accessor for values of type 'a in any map. Values put
    in the map using a key can only be retrieved using this
    very same key. *)

val create_inj : unit -> 'a injection
(** Return a value that works for a given type of values.  This function is
    normally called once for each type of value.  Several keys may be
    created for the same type, but a value set with a given setter can only be
    retrieved with the matching getter.  The same key can be reused
    across multiple maps (although not in a thread-safe way). *)

module type S = sig
  type key

  type t
  (** A map containing values of different types, indexed by {!key}. *)

  val empty : t
  (** Empty map *)

  val get : inj:'a injection -> key -> t -> 'a option
  (** Get the value corresponding to this key, if it exists and
      belongs to the same key *)

  val add : inj:'a injection -> key -> 'a -> t -> t
  (** Bind the key to the value, using [inj] *)

  val find : inj:'a injection -> key -> t -> 'a
  (** Find the value for the given key, which must be of the right type.
      @raise Not_found if either the key is not found, or if its value
        doesn't belong to the right type *)

  val cardinal : t -> int
  (** Number of bindings *)

  val remove : key -> t -> t
  (** Remove the binding for this key *)

  val mem : inj:_ injection-> key -> t -> bool
  (** Is the given key in the map, with the right type? *)

  val iter_keys : f:(key -> unit) -> t -> unit
  (** Iterate on the keys of this map *)

  val fold_keys : f:('a -> key -> 'a) -> x:'a -> t -> 'a
  (** Fold over the keys *)

  (** {2 Iterators} *)

  type 'a sequence = ('a -> unit) -> unit

  val keys_seq : t -> key sequence
  (** All the keys *)

  val bindings_of : inj:'a injection -> t -> (key * 'a) sequence
  (** All the bindings that come from the corresponding injection *)

  type value =
    | Value : ('a injection -> 'a option) -> value

  val bindings : t -> (key * value) sequence
  (** Iterate on all bindings *)
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORD) : S with type key = X.t
