
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map} *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find}. *)

  val get_or : key -> 'a t -> default:'a -> 'a
  (** [get_or k m ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [m]).
      @since 0.16 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val choose_opt : 'a t -> (key * 'a) option
  (** Safe version of {!choose}.
      @since 1.5 *)

  val min_binding_opt : 'a t -> (key * 'a) option
  (** Safe version of {!min_binding}.
      @since 1.5 *)

  val max_binding_opt : 'a t -> (key * 'a) option
  (** Safe version of {!max_binding}.
      @since 1.5 *)

  val find_opt : key -> 'a t -> 'a option
  (** Safe version of {!find}.
      @since 1.5 *)

  val find_first : (key -> bool) -> 'a t -> key * 'a
  (** Find smallest binding satisfying the monotonic predicate.
      See {!Map.S.find_first}.
      @since 1.5 *)

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  (** Safe version of {!find_first}.
      @since 1.5 *)

  val merge_safe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [merge_safe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val of_iter : (key * 'a) iter -> 'a t
  (** Like {!of_list}.
      @since 2.8 *)

  val add_std_seq : 'a t -> (key * 'a) Seq.t -> 'a t
  (** Like {!add_list}.
      @since 2.8 *)

  val of_std_seq : (key * 'a) Seq.t -> 'a t
  (** Like {!of_list}.
      @since 2.8 *)

  val add_iter : 'a t -> (key * 'a) iter -> 'a t
  (** Like {!add_list}.
      @since 2.8 *)

  val of_iter : (key * 'a) iter -> 'a t
  (** Like {!of_list}.
      @since 2.8 *)

  val to_iter : 'a t -> (key * 'a) iter
  (** Like {!to_list}.
      @since 2.8 *)

  val of_list : (key * 'a) list -> 'a t
  (** Build a map from the given list of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, only its last binding
      will be present in the result. *)

  val add_list : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

  val keys : _ t -> key iter
  (** Iterate on keys only.
      @since 0.15 *)

  val values : 'a t -> 'a iter
  (** Iterate on values only.
      @since 0.15 *)

  val to_list : 'a t -> (key * 'a) list

  val pp :
    ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
    key printer -> 'a printer -> 'a t printer
end

module Make(O : Map.OrderedType) = struct
  module M = Map.Make(O)

  (* backport functions from recent stdlib.
     they will be shadowed by inclusion of [S] if present. *)

  let union f a b =
    M.merge
      (fun k v1 v2 -> match v1, v2 with
         | None, None -> assert false
         | None, (Some _ as r) -> r
         | Some _ as r, None -> r
         | Some v1, Some v2 -> f k v1 v2)
      a b

  let choose_opt m =
    try Some (M.choose m)
    with Not_found -> None

  let find_opt k m =
    try Some (M.find k m)
    with Not_found -> None

  let max_binding_opt m =
    try Some (M.max_binding m)
    with Not_found -> None

  let min_binding_opt m =
    try Some (M.min_binding m)
    with Not_found -> None

  exception Find_binding_exit

  let find_first_opt f m =
    let res = ref None in
    try
      M.iter
        (fun k v ->
           if f k then (
             res := Some (k,v);
             raise Find_binding_exit
           ))
        m;
      None
    with Find_binding_exit ->
      !res

  let find_first f m = match find_first_opt f m with
    | None -> raise Not_found
    | Some (k,v) -> k, v

  (* linear time, must traverse the whole map… *)
  let find_last_opt f m =
    let res = ref None in
    M.iter
      (fun k v -> if f k then res := Some (k,v))
      m;
    !res

  let find_last f m = match find_last_opt f m with
    | None -> raise Not_found
    | Some (k,v) -> k, v

  include M

  let get = find_opt

  let get_or k m ~default =
    try find k m
    with Not_found -> default

  let update k f m =
    let x =
      try f (Some (find k m))
      with Not_found -> f None
    in
    match x with
      | None -> remove k m
      | Some v' -> add k v' m

  let merge_safe ~f a b =
    merge
      (fun k v1 v2 -> match v1, v2 with
         | None, None -> assert false
         | Some v1, None -> f k (`Left v1)
         | None, Some v2 -> f k (`Right v2)
         | Some v1, Some v2 -> f k (`Both (v1,v2)))
      a b

  let add_std_seq m s =
    let m = ref m in
    Seq.iter (fun (k,v) -> m := add k v !m) s;
    !m

  let of_std_seq s = add_std_seq empty s

  let add_iter m s =
    let m = ref m in
    s (fun (k,v) -> m := add k v !m);
    !m

  let of_iter s = add_iter empty s

  let to_iter m yield =
    iter (fun k v -> yield (k,v)) m

  let keys m yield =
    iter (fun k _ -> yield k) m

  let values m yield =
    iter (fun _ v -> yield v) m

  let add_list m l = List.fold_left (fun m (k,v) -> add k v m) m l

  let of_list l = add_list empty l

  let to_list m =
    fold (fun k v acc -> (k,v)::acc) m []

  let pp ?(start="") ?(stop="") ?(arrow="->") ?(sep=", ") pp_k pp_v fmt m =
    Format.pp_print_string fmt start;
    let first = ref true in
    iter
      (fun k v ->
         if !first then first := false
         else (
           Format.pp_print_string fmt sep;
           Format.pp_print_cut fmt ()
         );
         pp_k fmt k;
         Format.pp_print_string fmt arrow;
         pp_v fmt v)
      m;
    Format.pp_print_string fmt stop
end
