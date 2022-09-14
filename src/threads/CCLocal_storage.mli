(** Thread/Domain local storage

    This allows the creation of global state that is per-domain or per-thread.

    {b status} experimental

    @since NEXT_RELEASE
*)

type 'a t

val create : unit -> 'a t
(** Create new storage *)

val get : 'a t -> 'a option
(** Get the content for this thread, if any. *)

val get_exn : 'a t -> 'a
(** Same as {!get}, but fails if no data was associated to this thread.
    @raise Not_found if the data is not there. *)

val get_or : default:'a -> 'a t -> 'a
(** Same as {!get} but returns [default] if no data is associated
    to this thread. *)

val set : 'a t -> 'a -> unit
(** Set content for this thread. *)

val set_get : 'a t -> 'a -> 'a option
(** Set content for this thread, and return the old value. *)

val n_entries : _ t -> int
(** Number of entries in the map currently.

    Be aware that some threads might
    have exited without cleaning up behind them. See {!with_} for
    scope-protected modification of the variable that will cleanup
    properly (like {!Fun.protect}).
*)

val with_ : 'a t -> 'a -> (unit -> 'b) -> 'b
(** [with_ var x f] sets [var] to [x] for this thread, calls [f()], and
    then restores the old value of [var] for this thread. *)
