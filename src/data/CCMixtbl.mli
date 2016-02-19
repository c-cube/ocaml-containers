
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash Table with Heterogeneous Keys}

From https://github.com/mjambon/mixtbl (thanks to him).
Example:

{[
let inj_int = CCMixtbl.create_inj () ;;

let tbl = CCMixtbl.create 10 ;;

OUnit.assert_equal None (CCMixtbl.get ~inj:inj_int tbl "a");;

CCMixtbl.set inj_int tbl "a" 1;;

OUnit.assert_equal (Some 1) (CCMixtbl.get ~inj:inj_int tbl "a");;

let inj_string = CCMixtbl.create_inj () ;;

CCMixtbl.set inj_string tbl "b" "Hello";

OUnit.assert_equal (Some "Hello") (CCMixtbl.get inj_string tbl "b");;
OUnit.assert_equal None (CCMixtbl.get inj_string tbl "a");;
OUnit.assert_equal (Some 1) (CCMixtbl.get inj_int tbl "a");;
CCMixtbl.set inj_string tbl "a" "Bye";;

OUnit.assert_equal None (CCMixtbl.get inj_int tbl "a");;
OUnit.assert_equal (Some "Bye") (CCMixtbl.get inj_string tbl "a");;
]}

@since 0.6 *)

type 'a t
(** A hash table containing values of different types.
    The type parameter ['a] represents the type of the keys. *)

type 'b injection
(** An accessor for values of type 'b in any table. Values put
    in the table using a key can only be retrieved using this
    very same key. *)

val create : int -> 'a t
(** [create n] creates a hash table of initial size [n]. *)

val create_inj : unit -> 'b injection
(** Return a value that works for a given type of values.  This function is
    normally called once for each type of value.  Several keys may be
    created for the same type, but a value set with a given setter can only be
    retrieved with the matching getter.  The same key can be reused
    across multiple tables (although not in a thread-safe way). *)

val get : inj:'b injection -> 'a t  -> 'a -> 'b option
(** Get the value corresponding to this key, if it exists and
    belongs to the same key *)

val set : inj:'b injection -> 'a t -> 'a -> 'b -> unit
(** Bind the key to the value, using [inj] *)

val find : inj:'b injection -> 'a t -> 'a -> 'b
(** Find the value for the given key, which must be of the right type.
    @raise Not_found if either the key is not found, or if its value
      doesn't belong to the right type *)

val length : 'a t -> int
(** Number of bindings *)

val clear : 'a t -> unit
(** Clear content of the hashtable *)

val remove : 'a t -> 'a -> unit
(** Remove the binding for this key *)

val copy : 'a t -> 'a t
(** Copy of the table *)

val mem : inj:_ injection-> 'a t -> 'a -> bool
(** Is the given key in the table, with the right type? *)

val iter_keys : 'a t -> ('a -> unit) -> unit
(** Iterate on the keys of this table *)

val fold_keys : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
(** Fold over the keys *)

(** {2 Iterators} *)

type 'a sequence = ('a -> unit) -> unit

val keys_seq : 'a t -> 'a sequence
(** All the keys *)

val bindings_of : inj:'b injection -> 'a t -> ('a * 'b) sequence
(** All the bindings that come from the corresponding injection *)

type value =
  | Value : ('b injection -> 'b option) -> value

val bindings : 'a t -> ('a *  value) sequence
(** Iterate on all bindings *)
