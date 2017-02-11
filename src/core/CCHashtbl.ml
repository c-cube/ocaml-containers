
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extension to the standard Hashtbl}  *)

type 'a sequence = ('a -> unit) -> unit
type 'a eq = 'a -> 'a -> bool
type 'a hash = 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Polymorphic tables} *)

module Poly = struct
  let get tbl x =
    try Some (Hashtbl.find tbl x)
    with Not_found -> None

  let get_or tbl x ~default =
    try Hashtbl.find tbl x
    with Not_found -> default

  (*$=
    "c" (let tbl = of_list [1,"a"; 2,"b"] in get_or tbl 3 ~default:"c")
    "b" (let tbl = of_list [1,"a"; 2,"b"] in get_or tbl 2 ~default:"c")
  *)

  let keys tbl k = Hashtbl.iter (fun key _ -> k key) tbl

  let values tbl k = Hashtbl.iter (fun _ v -> k v) tbl

  let keys_list tbl = Hashtbl.fold (fun k _ a -> k::a) tbl []
  let values_list tbl = Hashtbl.fold (fun _ v a -> v::a) tbl []

  let add_list tbl k v =
    let l = try Hashtbl.find tbl k with Not_found -> [] in
    Hashtbl.replace tbl k (v::l)

  let incr ?(by=1) tbl x =
    let n = get_or tbl x ~default:0 in
    if n+by <= 0
    then Hashtbl.remove tbl x
    else Hashtbl.replace tbl x (n+by)

  let decr ?(by=1) tbl x =
    try
      let n = Hashtbl.find tbl x in
      if n-by <= 0
      then Hashtbl.remove tbl x
      else Hashtbl.replace tbl x (n-by)
    with Not_found -> ()

  let map_list f h =
    Hashtbl.fold
      (fun x y acc -> f x y :: acc)
      h []

  (*$T
    of_list [1,"a"; 2,"b"] |> map_list (fun x y -> string_of_int x ^ y) \
      |> List.sort Pervasives.compare = ["1a"; "2b"]
  *)

  let to_seq tbl k = Hashtbl.iter (fun key v -> k (key,v)) tbl

  let add_seq tbl seq = seq (fun (k,v) -> Hashtbl.add tbl k v)

  let of_seq seq =
    let tbl = Hashtbl.create 32 in
    add_seq tbl seq;
    tbl

  let add_seq_count tbl seq = seq (fun k -> incr tbl k)

  let of_seq_count seq =
    let tbl = Hashtbl.create 32 in
    add_seq_count tbl seq;
    tbl

  let to_list tbl =
    Hashtbl.fold
      (fun k v l -> (k,v) :: l)
      tbl []

  let of_list l =
    let tbl = Hashtbl.create 32 in
    List.iter (fun (k,v) -> Hashtbl.add tbl k v) l;
    tbl

  let update tbl ~f ~k =
    let v = get tbl k in
    match v, f k v with
      | None, None -> ()
      | None, Some v' -> Hashtbl.add tbl k v'
      | Some _, Some v' -> Hashtbl.replace tbl k v'
      | Some _, None -> Hashtbl.remove tbl k

  (*$R
    let tbl = Hashtbl.create 32 in
    update tbl ~k:1 ~f:(fun _ _ -> Some "1");
    assert_equal (Some "1") (get tbl 1);
    update tbl ~k:2 ~f:(fun _ v->match v with Some _ -> assert false | None -> Some "2");
    assert_equal (Some "2") (get tbl 2);
    assert_equal 2 (Hashtbl.length tbl);
    update tbl ~k:1 ~f:(fun _ _ -> None);
    assert_equal None (get tbl 1);
  *)

  let get_or_add tbl ~f ~k =
    try Hashtbl.find tbl k
    with Not_found ->
      let v = f k in
      Hashtbl.add tbl k v;
      v

  (*$R
    let tbl = Hashtbl.create 32 in
    let v1 = get_or_add tbl ~k:1 ~f:(fun _ -> "1") in
    assert_equal "1" v1;
    assert_equal (Some "1") (get tbl 1);
    let v2 = get_or_add tbl ~k:2 ~f:(fun _ ->"2") in
    assert_equal "2" v2;
    assert_equal (Some "2") (get tbl 2);
    assert_equal "2" (get_or_add tbl ~k:2 ~f:(fun _ -> assert false));
    assert_equal 2 (Hashtbl.length tbl);
    ()
  *)

  let print pp_k pp_v fmt m =
    Format.fprintf fmt "@[<hov2>tbl {@,";
    let first = ref true in
    Hashtbl.iter
      (fun k v ->
         if !first then first := false else Format.pp_print_string fmt ", ";
         pp_k fmt k;
         Format.pp_print_string fmt " -> ";
         pp_v fmt v;
         Format.pp_print_cut fmt ()
      ) m;
    Format.fprintf fmt "}@]"
end

include Poly

(** {2 Functor} *)

module type S = sig
  include Hashtbl.S

  val get : 'a t -> key -> 'a option
  (** Safe version of {!Hashtbl.find} *)

  val get_or : 'a t -> key -> default:'a -> 'a
  (** [get_or tbl k ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [tbl])
      @since 0.16 *)

  val add_list : 'a list t -> key -> 'a -> unit
  (** [add_list tbl x y] adds [y] to the list [x] is bound to. If [x] is
      not bound, it becomes bound to [[y]].
      @since 0.16 *)

  val incr : ?by:int -> int t -> key -> unit
  (** [incr ?by tbl x] increments or initializes the counter associated with [x].
      If [get tbl x = None], then after update, [get tbl x = Some 1];
      otherwise, if [get tbl x = Some n], now [get tbl x = Some (n+1)].
      @param by if specified, the int value is incremented by [by] rather than 1
      @since 0.16 *)

  val decr : ?by:int -> int t -> key -> unit
  (** Same as {!incr} but substract 1 (or the value of [by]).
      If the value reaches 0, the key is removed from the table.
      This does nothing if the key is not already present in the table.
      @since 0.16 *)

  val keys : 'a t -> key sequence
  (** Iterate on keys (similar order as {!Hashtbl.iter}) *)

  val values : 'a t -> 'a sequence
  (** Iterate on values in the table *)

  val keys_list : _ t -> key list
  (** [keys t] is the list of keys in [t].
      @since 0.8 *)

  val values_list : 'a t -> 'a list
  (** [values t] is the list of values in [t].
      @since 0.8 *)

  val map_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** Map on a hashtable's items, collect into a list *)

  val to_seq : 'a t -> (key * 'a) sequence
  (** Iterate on values in the table *)

  val of_seq : (key * 'a) sequence -> 'a t
  (** From the given bindings, added in order *)

  val add_seq : 'a t -> (key * 'a) sequence -> unit
  (** Add the corresponding pairs to the table, using {!Hashtbl.add}.
      @since 0.16 *)

  val add_seq_count : int t -> key sequence -> unit
  (** [add_seq_count tbl seq] increments the count of each element of [seq]
      by calling {!incr}. This is useful for counting how many times each
      element of [seq] occurs.
      @since 0.16 *)

  val of_seq_count : key sequence -> int t
  (** Similar to {!add_seq_count}, but allocates a new table and returns it
      @since 0.16 *)

  val to_list : 'a t -> (key * 'a) list
  (** List of bindings (order unspecified)  *)

  val of_list : (key * 'a) list -> 'a t
  (** Build a table from the given list of bindings [k_i -> v_i],
      added in order using {!add}. If a key occurs several times,
      it will be added several times, and the visible binding
      will be the last one. *)

  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  (** [update tbl ~f ~k] updates key [k] by calling [f k (Some v)] if
      [k] was mapped to [v], or [f k None] otherwise; if the call
      returns [None] then [k] is removed/stays removed, if the call
      returns [Some v'] then the binding [k -> v'] is inserted
      using {!Hashtbl.replace}
      @since 0.14 *)

  val get_or_add : 'a t -> f:(key -> 'a) -> k:key -> 'a
  (** [get_or_add tbl ~k ~f] finds and returns the binding of [k]
      in [tbl], if it exists. If it does not exist, then [f k]
      is called to obtain a new binding [v]; [k -> v] is added
      to [tbl] and [v] is returned.
      @since 1.0 *)

  val print : key printer -> 'a printer -> 'a t printer
  (** Printer for tables
      @since 0.13 *)
end

(*$inject
  module T = Make(CCInt)
*)

module Make(X : Hashtbl.HashedType)
  : S with type key = X.t and type 'a t = 'a Hashtbl.Make(X).t
= struct
  include Hashtbl.Make(X)

  let get tbl x =
    try Some (find tbl x)
    with Not_found -> None

  let get_or tbl x ~default =
    try find tbl x
    with Not_found -> default

  (*$=
    "c" (let tbl = T.of_list [1,"a"; 2,"b"] in T.get_or tbl 3 ~default:"c")
    "b" (let tbl = T.of_list [1,"a"; 2,"b"] in T.get_or tbl 2 ~default:"c")
  *)

  let incr ?(by=1) tbl x =
    let n = get_or tbl x ~default:0 in
    if n+by <= 0
    then remove tbl x
    else replace tbl x (n+by)

  (*$R
    let tbl = T.create 32 in
    T.incr tbl 1 ;
    T.incr tbl 2;
    T.incr tbl 1;
    assert_equal 2 (T.find tbl 1);
    assert_equal 1 (T.find tbl 2);
    assert_equal 2 (T.length tbl);
    T.decr tbl 2;
    assert_equal 0 (T.get_or tbl 2 ~default:0);
    assert_equal 1 (T.length tbl);
    assert_bool "2 removed" (not (T.mem tbl 2));
  *)

  let add_list tbl k v =
    let l = try find tbl k with Not_found -> [] in
    replace tbl k (v::l)

  let decr ?(by=1) tbl x =
    try
      let n = find tbl x in
      if n-by <= 0
      then remove tbl x
      else replace tbl x (n-by)
    with Not_found -> ()

  let keys tbl k = iter (fun key _ -> k key) tbl

  let values tbl k = iter (fun _ v -> k v) tbl

  let keys_list tbl = fold (fun k _ a -> k::a) tbl []
  let values_list tbl = fold (fun _ v a -> v::a) tbl []

  let map_list f h =
    fold
      (fun x y acc -> f x y :: acc)
      h []

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

  let to_seq tbl k = iter (fun key v -> k (key,v)) tbl

  let add_seq tbl seq = seq (fun (k,v) -> add tbl k v)

  let of_seq seq =
    let tbl = create 32 in
    add_seq tbl seq;
    tbl

  let add_seq_count tbl seq = seq (fun k -> incr tbl k)

  let of_seq_count seq =
    let tbl = create 32 in
    add_seq_count tbl seq;
    tbl

  let to_list tbl =
    fold
      (fun k v l -> (k,v) :: l)
      tbl []

  let of_list l =
    let tbl = create 32 in
    List.iter (fun (k,v) -> add tbl k v) l;
    tbl

  let print pp_k pp_v fmt m =
    Format.fprintf fmt "@[<hov2>tbl {@,";
    let first = ref true in
    iter
      (fun k v ->
         if !first then first := false else Format.pp_print_string fmt ", ";
         pp_k fmt k;
         Format.pp_print_string fmt " -> ";
         pp_v fmt v;
         Format.pp_print_cut fmt ()
      ) m;
    Format.fprintf fmt "}@]"
end

