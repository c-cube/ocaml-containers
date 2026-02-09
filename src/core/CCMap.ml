(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map} *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** [get k m] returns [Some v] if the current binding of [k] in [m] is [v],
      or [None] if the key [k] is not present.
      Safe version of {!find}. *)

  val get_or : key -> 'a t -> default:'a -> 'a
  (** [get_or k m ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [m]).
      @since 0.16 *)

  val merge_safe :
    f:(key -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option) ->
    'a t ->
    'b t ->
    'c t
  (** [merge_safe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val add_seq_with :
    f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) Seq.t -> 'a t
  (** [add_seq ~f m l] adds the given seq [l] of bindings to the map [m],
      using [f] to combine values that have the same key.
      @since 3.3 *)

  val of_seq_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) Seq.t -> 'a t
  (** [of_seq_with ~f l] builds a map from the given seq [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the seq than [v2].
      @since 3.3 *)

  val add_iter : 'a t -> (key * 'a) iter -> 'a t
  (** [add_iter m iter] adds the given [iter] of bindings to the map [m].
      Like {!add_list}.
      @since 2.8 *)

  val add_iter_with :
    f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) iter -> 'a t
  (** [add_iter ~f m l] adds the given iter [l] of bindings to the map [m],
      using [f] to combine values that have the same key.
      @since 3.3 *)

  val of_iter : (key * 'a) iter -> 'a t
  (** [of_iter iter] builds a map from the given [iter] of bindings.
      Like {!of_list}.
      @since 2.8 *)

  val of_iter_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) iter -> 'a t
  (** [of_iter_with ~f l] builds a map from the given iter [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the iter than [v2].
      @since 3.3 *)

  val to_iter : 'a t -> (key * 'a) iter
  (** [to_iter m] iterates on the whole map [m], creating an [iter] of bindings.
      Like {!to_list}.
      @since 2.8 *)

  val of_list : (key * 'a) list -> 'a t
  (** [of_list l] builds a map from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, only its last binding
      will be present in the result. *)

  val of_list_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) list -> 'a t
  (** [of_list_with ~f l] builds a map from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, all its bindings are combined using the
      function [f], with [f key v1 v2] being called with [v1] occurring
      later in the list than [v2].
      @since 3.3 *)

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** [add_list m l] adds the given list [l] of bindings to the map [m].
      @since 0.14 *)

  val add_list_with :
    f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) list -> 'a t
  (** [add_list ~f m l] adds the given list [l] of bindings to the map [m],
      using [f] to combine values that have the same key.
      @since 3.3 *)

  val keys : _ t -> key iter
  (** [keys m] iterates on the keys of [m] only, creating an [iter] of keys.
      @since 0.15 *)

  val values : 'a t -> 'a iter
  (** [values m] iterates on the values of [m] only, creating an [iter] of values.
      @since 0.15 *)

  val to_list : 'a t -> (key * 'a) list
  (** [to_list m] builds a list of the bindings of the given map [m].
      The order is unspecified. *)

  val pp :
    ?pp_start:unit printer ->
    ?pp_stop:unit printer ->
    ?pp_arrow:unit printer ->
    ?pp_sep:unit printer ->
    key printer ->
    'a printer ->
    'a t printer
  (** [pp ?pp_start ?pp_stop ?pp_arrow ?pp_sep pp_key pp_v m] pretty-prints the
      contents of the map. *)
end

module Make (O : Map.OrderedType) = struct
  module M = Map.Make (O)

  (* backport functions from recent stdlib.
     they will be shadowed by inclusion of [S] if present. *)

  (* === include M.
     This will shadow some values depending on OCaml's current version
     === *)

  include M

  let get = find_opt
  let get_or k m ~default = try find k m with Not_found -> default

  let merge_safe ~f a b =
    merge
      (fun k v1 v2 ->
        match v1, v2 with
        | None, None -> assert false
        | Some v1, None -> f k (`Left v1)
        | None, Some v2 -> f k (`Right v2)
        | Some v1, Some v2 -> f k (`Both (v1, v2)))
      a b

  let add_seq_with ~f m s =
    let combine k v = function
      | None -> Some v
      | Some v0 -> Some (f k v v0)
    in
    Seq.fold_left (fun m (k, v) -> update k (combine k v) m) m s

  let of_seq_with ~f s = add_seq_with ~f empty s

  let add_iter m s =
    let m = ref m in
    s (fun (k, v) -> m := add k v !m);
    !m

  let add_iter_with ~f m s =
    let combine k v = function
      | None -> Some v
      | Some v0 -> Some (f k v v0)
    in
    let m = ref m in
    s (fun (k, v) -> m := update k (combine k v) !m);
    !m

  let of_iter s = add_iter empty s
  let of_iter_with ~f s = add_iter_with ~f empty s
  let to_iter m yield = iter (fun k v -> yield (k, v)) m
  let keys m yield = iter (fun k _ -> yield k) m
  let values m yield = iter (fun _ v -> yield v) m
  let add_list m l = List.fold_left (fun m (k, v) -> add k v m) m l

  let add_list_with ~f m l =
    let combine k v = function
      | None -> Some v
      | Some v0 -> Some (f k v v0)
    in
    List.fold_left (fun m (k, v) -> update k (combine k v) m) m l

  [@@@iflt 5.1]

  let of_list l = add_list empty l

  [@@@endif]

  let of_list_with ~f l = add_list_with ~f empty l

  [@@@iflt 5.1]

  let to_list m = fold (fun k v acc -> (k, v) :: acc) m []

  [@@@endif]

  let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
      ?(pp_arrow = fun fmt () -> Format.fprintf fmt "@ -> ")
      ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_k pp_v fmt m =
    pp_start fmt ();
    let first = ref true in
    iter
      (fun k v ->
        if !first then
          first := false
        else
          pp_sep fmt ();
        pp_k fmt k;
        pp_arrow fmt ();
        pp_v fmt v)
      m;
    pp_stop fmt ()
end
