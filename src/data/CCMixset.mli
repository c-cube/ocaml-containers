
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Set of Heterogeneous Values}

    {[
      let k1 : int key = newkey () in
      let k2 : int key = newkey () in
      let k3 : string key = newkey () in
      let set =
        empty
        |> set ~key:k1 1
        |> set ~key:k2 2
        |> set ~key:k3 "3"
      in
      assert (get ~key:k1 set = Some 1);
      assert (get ~key:k2 set = Some 2);
      assert (get ~key:k3 set = Some "3");
      ()
    ]}

    @since 0.11 *)

type t
(** A set of values of heterogeneous types *)

type 'a key
(** A unique "key" to access a value of type ['a] in a [set] *)

val newkey : unit -> 'a key
(** [newkey ()] creates a new unique key that can be used to access
    a ['a] value in a set. Each key created with [newkey] is distinct
    from any other key, even if they have the same type.

    Not thread-safe. *)

val empty : t
(** Empty set *)

val set : key:'a key -> 'a -> t -> t
(** [set ~key v set] maps [key] to [v] in [set]. It means that
    for every [set], [get ~key (set ~key v set) = Some v]. *)

val get : key:'a key -> t -> 'a option
(** [get ~key set] obtains the value for [key] in [set], if any. *)

val get_exn : key:'a key -> t -> 'a
(** Same as {!get}, but can fail
    @raise Not_found if the key is not present *)

val cardinal : t -> int
(** Number of mappings *)
