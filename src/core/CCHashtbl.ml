(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extension to the standard Hashtbl}  *)

type 'a iter = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

module Poly = struct
  let get tbl x = try Some (Hashtbl.find tbl x) with Not_found -> None
  let get_or tbl x ~default = try Hashtbl.find tbl x with Not_found -> default
  let keys tbl k = Hashtbl.iter (fun key _ -> k key) tbl
  let values tbl k = Hashtbl.iter (fun _ v -> k v) tbl
  let keys_list tbl = Hashtbl.fold (fun k _ a -> k :: a) tbl []
  let values_list tbl = Hashtbl.fold (fun _ v a -> v :: a) tbl []

  let add_list tbl k v =
    let l = try Hashtbl.find tbl k with Not_found -> [] in
    Hashtbl.replace tbl k (v :: l)

  let incr ?(by = 1) tbl x =
    let n = get_or tbl x ~default:0 in
    if n + by <= 0 then
      Hashtbl.remove tbl x
    else
      Hashtbl.replace tbl x (n + by)

  let decr ?(by = 1) tbl x =
    try
      let n = Hashtbl.find tbl x in
      if n - by <= 0 then
        Hashtbl.remove tbl x
      else
        Hashtbl.replace tbl x (n - by)
    with Not_found -> ()

  let map_list f h = Hashtbl.fold (fun x y acc -> f x y :: acc) h []
  let to_iter tbl k = Hashtbl.iter (fun key v -> k (key, v)) tbl
  let add_iter tbl i = i (fun (k, v) -> Hashtbl.add tbl k v)

  let add_iter_with ~f tbl i =
    i (fun (k, v) ->
        match Hashtbl.find tbl k with
        | exception Not_found -> Hashtbl.add tbl k v
        | v2 -> Hashtbl.replace tbl k (f k v v2))

  let add_seq tbl seq = Seq.iter (fun (k, v) -> Hashtbl.add tbl k v) seq

  let add_seq_with ~f tbl seq =
    Seq.iter
      (fun (k, v) ->
        match Hashtbl.find tbl k with
        | exception Not_found -> Hashtbl.add tbl k v
        | v2 -> Hashtbl.replace tbl k (f k v v2))
      seq

  (* helper for building hashtables by bulk mutation *)
  let[@inline] mk_tbl_ f x =
    let tbl = Hashtbl.create 32 in
    f tbl x;
    tbl

  let of_iter i = mk_tbl_ add_iter i
  let of_seq i = mk_tbl_ add_seq i
  let of_iter_with ~f i = mk_tbl_ (add_iter_with ~f) i
  let of_seq_with ~f i = mk_tbl_ (add_seq_with ~f) i
  let add_iter_count tbl i = i (fun k -> incr tbl k)
  let add_seq_count tbl seq = Seq.iter (fun k -> incr tbl k) seq
  let of_iter_count i = mk_tbl_ add_iter_count i
  let of_seq_count i = mk_tbl_ add_seq_count i
  let to_list tbl = Hashtbl.fold (fun k v l -> (k, v) :: l) tbl []

  let of_list l =
    let tbl = Hashtbl.create 32 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) l;
    tbl

  let of_list_with ~f l =
    let tbl = Hashtbl.create 32 in
    List.iter
      (fun (k, v) ->
        match Hashtbl.find tbl k with
        | exception Not_found -> Hashtbl.add tbl k v
        | v2 -> Hashtbl.replace tbl k (f k v v2))
      l;
    tbl

  let update tbl ~f ~k =
    let v = get tbl k in
    match v, f k v with
    | None, None -> ()
    | None, Some v' -> Hashtbl.add tbl k v'
    | Some _, Some v' -> Hashtbl.replace tbl k v'
    | Some _, None -> Hashtbl.remove tbl k

  let get_or_add tbl ~f ~k =
    try Hashtbl.find tbl k
    with Not_found ->
      let v = f k in
      Hashtbl.add tbl k v;
      v

  let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
      ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ")
      ?(pp_arrow = fun fmt () -> Format.fprintf fmt "@ -> ") pp_k pp_v fmt m =
    pp_start fmt ();
    let first = ref true in
    Hashtbl.iter
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

include Poly

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** [get tbl k] finds a binding for the key [k] if present,
      or returns [None] if no value is found.
      Safe version of {!Hashtbl.find}. *)

  val get_or : 'a t -> key -> default:'a -> 'a
  (** [get_or tbl k ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [tbl]).
      @since 0.16 *)

  val add_list : 'a list t -> key -> 'a -> unit
  (** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
      not bound, it becomes bound to [y].
      @since 0.16 *)

  val incr : ?by:int -> int t -> key -> unit
  (** [incr ?by tbl x] increments or initializes the counter associated with [x].
      If [get tbl x = None], then after update, [get tbl x = Some 1];
      otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
      @param by if specified, the int value is incremented by [by] rather than 1.
      @since 0.16 *)

  val decr : ?by:int -> int t -> key -> unit
  (** [decr ?by tbl x] is like {!incr} but subtract 1 (or the value of [by]).
      If the value reaches 0, the key is removed from the table.
      This does nothing if the key is not already present in the table.
      @since 0.16 *)

  val keys : 'a t -> key iter
  (**  [keys tbl f] iterates on keys (similar order as {!Hashtbl.iter}). *)

  val values : 'a t -> 'a iter
  (**  [values tbl f] iterates on values in the table. *)

  val keys_list : _ t -> key list
  (** [keys_list tbl] is the list of keys in [tbl].
      If the key is in the Hashtable multiple times, all occurrences will be returned.
      @since 0.8 *)

  val values_list : 'a t -> 'a list
  (** [values_list t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list. *)

  val to_iter : 'a t -> (key * 'a) iter
  (** Iterate on bindings in the table.
      @since 2.8 *)

  val add_iter : 'a t -> (key * 'a) iter -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 2.8 *)

  val add_iter_with :
    f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) iter -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      If a key occurs multiple times in the input, the values are combined
      using [f] in an unspecified order.
      @since 3.3 *)

  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      Renamed from [add_std_seq] since 3.0.
      @since 3.0 *)

  val add_seq_with :
    f:(key -> 'a -> 'a -> 'a) -> 'a t -> (key * 'a) Seq.t -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      If a key occurs multiple times in the input, the values are combined
      using [f] in an unspecified order.
      @since 3.3 *)

  val of_iter : (key * 'a) iter -> 'a t
  (** From the given bindings, added in order.
      @since 2.8 *)

  val of_iter_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) iter -> 'a t
  (** From the given bindings, added in order.
      If a key occurs multiple times in the input, the values are combined
      using [f] in an unspecified order.
      @since 3.3 *)

  val of_seq : (key * 'a) Seq.t -> 'a t
  (** From the given bindings, added in order.
      Renamed from [of_std_seq] since 3.0.
      @since 3.0 *)

  val of_seq_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) Seq.t -> 'a t
  (** From the given bindings, added in order.
      If a key occurs multiple times in the input, the values are combined
      using [f] in an unspecified order.
      @since 3.3 *)

  val add_iter_count : int t -> key iter -> unit
  (** [add_iter_count tbl i] increments the count of each element of [i]
      by calling {!incr}. This is useful for counting how many times each
      element of [i] occurs.
      @since 2.8 *)

  val add_seq_count : int t -> key Seq.t -> unit
  (** [add_seq_count tbl seq] increments the count of each element of [seq]
      by calling {!incr}. This is useful for counting how many times each
      element of [seq] occurs.
      Renamed from [of_std_seq_count] since 3.0.
      @since 3.0 *)

  val of_iter_count : key iter -> int t
  (** Like {!add_seq_count}, but allocates a new table and returns it.
      @since 2.8 *)

  val of_seq_count : key Seq.t -> int t
  (** Like {!add_seq_count}, but allocates a new table and returns it.
      Renamed from [of_std_seq_count] since 3.0.
      @since 3.0 *)

  val to_list : 'a t -> (key * 'a) list
  (** [to_list tbl] returns the list of (key,value) bindings (order unspecified). *)

  val of_list : (key * 'a) list -> 'a t
  (** [of_list l] builds a table from the given list [l] of bindings [k_i -> v_i],
      added in order using {!add}. If a key occurs several times,
      it will be added several times, and the visible binding
      will be the last one. *)

  val of_list_with : f:(key -> 'a -> 'a -> 'a) -> (key * 'a) list -> 'a t
  (** [of_list l] builds a table from the given list [l] of bindings [k_i -> v_i].
      If a key occurs multiple times in the input, the values are combined
      using [f] in an unspecified order.
      @since 3.3 *)

  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}.
      @since 0.14 *)

  val get_or_add : 'a t -> f:(key -> 'a) -> k:key -> 'a
  (** [get_or_add tbl ~k ~f] finds and returns the binding of [k]
      in [tbl], if it exists. If it does not exist, then [f k]
      is called to obtain a new binding [v]; [k -> v] is added
      to [tbl] and [v] is returned.
      @since 1.0 *)

  val pp :
    ?pp_start:unit printer ->
    ?pp_stop:unit printer ->
    ?pp_sep:unit printer ->
    ?pp_arrow:unit printer ->
    key printer ->
    'a printer ->
    'a t printer
  (** [pp ~pp_start ~pp_stop ~pp_sep ~pp arrow pp_k pp_v] returns a table printer
      given a [pp_k] printer
      for individual key and a [pp_v] printer for individual value.
      [pp_start] and [pp_stop] control the opening and closing delimiters,
      by default print nothing. [pp_sep] control the separator between binding.
      [pp_arrow] control the arrow between the key and value.
      Renamed from [print] since 2.0.
      @since 0.13 *)
end

module Make (X : Hashtbl.HashedType) :
  S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t = struct
  include Hashtbl.Make (X)

  let get tbl x = try Some (find tbl x) with Not_found -> None
  let get_or tbl x ~default = try find tbl x with Not_found -> default

  let incr ?(by = 1) tbl x =
    let n = get_or tbl x ~default:0 in
    if n + by <= 0 then
      remove tbl x
    else
      replace tbl x (n + by)

  let add_list tbl k v =
    let l = try find tbl k with Not_found -> [] in
    replace tbl k (v :: l)

  let decr ?(by = 1) tbl x =
    try
      let n = find tbl x in
      if n - by <= 0 then
        remove tbl x
      else
        replace tbl x (n - by)
    with Not_found -> ()

  let keys tbl k = iter (fun key _ -> k key) tbl
  let values tbl k = iter (fun _ v -> k v) tbl
  let keys_list tbl = fold (fun k _ a -> k :: a) tbl []
  let values_list tbl = fold (fun _ v a -> v :: a) tbl []
  let map_list f h = fold (fun x y acc -> f x y :: acc) h []

  let update tbl ~f ~k =
    let v = get tbl k in
    match v, f k v with
    | None, None -> ()
    | None, Some v' -> add tbl k v'
    | Some _, Some v' -> replace tbl k v'
    | Some _, None -> remove tbl k

  let get_or_add tbl ~f ~k =
    try find tbl k
    with Not_found ->
      let v = f k in
      add tbl k v;
      v

  let to_iter tbl k = iter (fun key v -> k (key, v)) tbl
  let add_iter tbl i = i (fun (k, v) -> add tbl k v)

  let add_iter_with ~f tbl i =
    i (fun (k, v) ->
        match find tbl k with
        | exception Not_found -> add tbl k v
        | v2 -> replace tbl k (f k v v2))

  let add_seq tbl seq = Seq.iter (fun (k, v) -> add tbl k v) seq

  let add_seq_with ~f tbl seq =
    Seq.iter
      (fun (k, v) ->
        match find tbl k with
        | exception Not_found -> add tbl k v
        | v2 -> replace tbl k (f k v v2))
      seq

  (* helper for building hashtables by bulk mutation *)
  let[@inline] mk_tbl_ f x =
    let tbl = create 32 in
    f tbl x;
    tbl

  let of_iter i = mk_tbl_ add_iter i
  let of_seq i = mk_tbl_ add_seq i
  let of_iter_with ~f i = mk_tbl_ (add_iter_with ~f) i
  let of_seq_with ~f i = mk_tbl_ (add_seq_with ~f) i
  let add_iter_count tbl i = i (fun k -> incr tbl k)
  let add_seq_count tbl seq = Seq.iter (fun k -> incr tbl k) seq

  let of_iter_count seq =
    let tbl = create 32 in
    add_iter_count tbl seq;
    tbl

  let of_seq_count i =
    let tbl = create 32 in
    add_seq_count tbl i;
    tbl

  let to_list tbl = fold (fun k v l -> (k, v) :: l) tbl []

  let of_list l =
    let tbl = create 32 in
    List.iter (fun (k, v) -> add tbl k v) l;
    tbl

  let of_list_with ~f l =
    let tbl = create 32 in
    List.iter
      (fun (k, v) ->
        match find tbl k with
        | exception Not_found -> add tbl k v
        | v2 -> replace tbl k (f k v v2))
      l;
    tbl

  let pp ?(pp_start = fun _ () -> ()) ?(pp_stop = fun _ () -> ())
      ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ")
      ?(pp_arrow = fun fmt () -> Format.fprintf fmt "@ -> ") pp_k pp_v fmt m =
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
